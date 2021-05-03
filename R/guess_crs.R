#' Guess the CRS of a dataset that is missing CRS information
#'
#' @param input_sf An input sf object in a projected coordinate system that is
#'                 missing CRS information. For example, you may have loaded in a shapefile without
#'                 a .prj file, or your input data has no CRS definition attached.
#' @param target_location A coordinate pair of form \code{c(longitude, latitude)}
#'                        or an address/location that you know is located within your
#'                        input sf object.  If the mapboxapi package is installed, you can
#'                        supply a location name (e.g. an address or a city) instead of a
#'                        coordinate pair.
#' @param units If known, the units of your projected coordinate system (e.g. \code{"m"} for
#'              meters or \code{"us-ft"} for US feet).  This is not required but will make
#'              the guesses more accurate.
#' @param n_return The number of possible CRS choices to return; defaults to 10. A higher
#'                 number than that may include CRS options that are unlikely to work with
#'                 your data. Use the returned \code{dist_km} column to judge whether the CRS
#'                 guess makes sense for your data.
#'
#' @return A tibble of CRS guesses for your data, sorted in ascending order of distance between
#'         your target location and the input sf object's centroid when in that CRS.
#' @export
guess_crs <- function(input_sf, target_location, units = NULL,
                      n_return = 10) {

  # If the data already has a CRS, you don't need this function!
  if (!is.na(sf::st_crs(input_sf)$epsg)) {
    stop("Your data already has a CRS set; perhaps you want `crsuggest::suggest_crs()` instead.",
         call. = FALSE)
  }

  # If mapboxapi is installed, geocode the address
  if (is.character(target_location)) {
    if (!"mapboxapi" %in% utils::installed.packages()) {
      stop("The mapboxapi package is used for geocoding functionality; please install and set up mapboxapi or supply a coordinate pair instead.", call. = FALSE)
    } else {
      target_coords <- mapboxapi::mb_geocode(target_location)
    }
  } else if (is.numeric(target_location)) {
    if (length(target_location) != 2) {
      stop("Please supply a length-2 vector representing the coordinates of format `c(lon, lat)`.",
           call. = FALSE)
    } else {
      target_coords <- target_location
    }
  }

  # Make an sf object from the target coordinates
  target_sf <- target_coords %>%
    st_point() %>%
    st_sfc(crs = 4326) %>%
    st_sf()

  # Get a list of suggested CRSs for that location, optionally by units of measurement
  # if known
  crs_options <- crsuggest::suggest_crs(target_sf, limit = 50, units = units) %>%
    dplyr::filter(!is.na(crs_units))

  # We now need to get the centroid of the input sf object with no CRS
  no_crs_centroid <- suppressMessages(suppressWarnings(st_centroid(st_union(input_sf))))

  # We now iterate through the suggested CRS options and see if they work
  # To do this, calculate the distance between the target sf and the centroid,
  # with the CRS set
  codes <- crs_options$crs_code

  message("Evaluating CRS options...")
  dist_df <- purrr::map_df(codes, ~{
    target_sf_transformed <- sf::st_transform(target_sf, as.integer(.x))
    centroid_with_crs <- sf::st_set_crs(no_crs_centroid, as.integer(.x))

    dist <- sf::st_distance(target_sf_transformed, centroid_with_crs) %>%
      units::set_units("km") %>%
      as.numeric()

    dplyr::tibble(crs_code = .x, dist_km = dist)
  }) %>%
    dplyr::arrange(dist_km, dplyr::desc(crs_code)) %>%
    dplyr::slice_min(dist_km, n = n_return)

  top_crs <- dist_df$crs_code[1]

  message(sprintf("The 'best guess' for the CRS of your data is EPSG code %s.\nUse `sf::st_crs(your_data) <- %s` to use this CRS for your data.\nView the returned dataset for other possible options.", top_crs, top_crs))

  return(dist_df)

}
