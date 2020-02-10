library(sf)

load("data/crs_sf.rda")

suggest_crs <- function(input, type = "projected") {

  crs_type <- dplyr::filter(crs_sf, crs_type == type)

  sf_wgs <- st_transform(input, st_crs(crs_type)) %>%
    st_union()

  centroid <- suppressWarnings(st_centroid(sf_wgs)) %>%
    st_sf()

  joined <- suppressMessages(st_join(centroid, crs_type))

  return(tibble::tibble(name = joined$crs_name, code = joined$crs_code))
}

library(tigris)
benton <- tracts("OR", "Benton", cb = TRUE, class = "sf")

bs <- suggest_crs(benton)

# Examining % area overlap
bu <- benton %>%
  st_transform(32663) %>%
  st_union()

crs_eq <- st_transform(crs_sf, 32663)

crs_sub <- crs_eq[bu, ] %>%
  dplyr::filter(crs_type == "projected")

bi <- st_intersection(bu, crs_sf)

crs_sub$hd <- st_distance(bu, crs_sub, which = "Hausdorff") %>% as.numeric()

st_geometry(crs_sub) <- NULL

