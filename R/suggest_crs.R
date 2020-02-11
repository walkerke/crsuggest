#' Suggest coordinate systems for an input spatial dataset
#'
#' @param input A spatial dataset of class \code{"sf"}, \code{"Spatial*"}, or
#'              \code{"RasterLayer"}.
#' @param type The output CRS type; defaults to \code{"projected"}.
#' @param limit How many results to return; defaults to \code{10}.
#' @param gcs (optional) The EPSG code for the corresponding geographic coordinate system of the results (e.g. \code{4326} for WGS 1984).
#' @param units (optional) The measurement units of the coordinate systems in the returned results.  Can be one of \code{"m"}, \code{"ft"}, or \code{"ft-us"}.
#'
#' @return A data frame with information about coordinate reference systems that could be suitably used for CRS transformation.
#' @export
suggest_crs <- function(input, type = "projected",
                        limit = 10, gcs = NULL,
                        units = NULL) {

  # If the input is a raster layer, convert to a polygon at the
  # extent of that layer
  if ("RasterLayer" %in% class(input)) {
    input <- input %>%
      st_bbox() %>%
      st_as_sfc()
  }

  # If object is from the sp package, convert to sf
  if (any(grepl("Spatial", class(input)))) {
    input <- st_as_sf(input)
  }

  # Filter the CRS object for the selected type, GCS, and units if requested
  crs_type <- dplyr::filter(crsuggest::crs_sf, crs_type == type)

  if (!is.null(gcs)) {
    gcs <- as.character(gcs)
    crs_type <- dplyr::filter(crs_type, crs_gcs == gcs)
  }

  if (!is.null(units)) {
    if (!units %in% c("ft", "m", "us-ft")) {
      stop("Units must be one of 'm', 'ft', or 'us-ft'")
    }

    crs_type <- dplyr::filter(crs_type, crs_units == units)
  }

  # Transform the input SF object to 32663 for overlay
  sf_proj <- st_transform(input, st_crs(crs_type))

  # If geometry type is POINT or MULTIPOINT, union then find the convex hull
  # If geometry type is LINESTRING or MULTILINESTRING, use the bounding box
  # If geometry type is POLYGON or MULTIPOLYGON, union
  geom_type <- unique(st_geometry_type(sf_proj))

  # Consider at later date how to handle mixed geometries

  if (geom_type %in% c("POINT", "MULTIPOINT")) {
    sf_poly <- sf_proj %>%
      st_union() %>%
      st_convex_hull()
  } else if (geom_type %in% c("LINESTRING", "MULTILINESTRING")) {
    sf_poly <- sf_proj %>%
      st_bbox() %>%
      st_as_sfc()
  } else if (geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
    sf_poly <- sf_proj %>%
      st_union()

    # Simplify the polygon if it is too large (>500 vertices)
    # Keep simplifying until the count is sufficiently reduced
    # (as general shape will be OK here)
    vertex_count <- mapview::npts(sf_poly)

    if (vertex_count > 500) {
      tol <- 5000
      vc <- vertex_count
      while (vc > 500) {
        sf_poly <- st_simplify(sf_poly, dTolerance = tol)
        vc <- mapview::npts(sf_poly)
        tol <- tol * 2
      }
    }
  }

  # Subset the area extent polygons further to those that intersect with our area of interest
  # To (try to) avoid edge cases, use a shrunken version of the geometry
  # (90 percent of size) to do this
  geom <- st_geometry(sf_poly)
  cntr <- st_centroid(sf_poly)

  geom90 <- (geom - cntr) * 0.90 + cntr

  st_crs(geom90) <- st_crs(crs_type)

  crs_sub <- crs_type[geom90, ]

  # Calculate the Hausdorff distance between the area of interest and the polygons,
  # then sort in ascending order of distance and return the top requested CRSs
  crs_output <- crs_sub %>%
    dplyr::mutate(hausdist = as.numeric(
      st_distance(
        sf_poly, ., which = "Hausdorff"
      )
    )) %>%
    st_drop_geometry() %>%
    dplyr::arrange(hausdist) %>%
    dplyr::filter(dplyr::row_number() <= limit) %>%
    dplyr::select(-hausdist)

  return(crs_output)

}
