library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(rmapshaper)

crs_url <- "https://apps.epsg.org/api/v1/CoordRefSystem/?pageSize=8000"

db_crs_df <- read_csv("data-raw/crs_table.csv") %>%
  select(Code = COORD_REF_SYS_CODE,
         Base = BASE_CRS_CODE)

crs_df <- httr::GET(crs_url) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  magrittr::extract2(1) %>%
  left_join(db_crs_df, by = "Code")



extent_url <- "https://apps.epsg.org/api/v1/Extent/?pageSize=8000"

extent_df <- httr::GET(extent_url) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  magrittr::extract2(1)

# Grab the geometry from the API and assemble a dataset
# Takes a while, so only re-run when necessary
#
# extent_geom <- purrr::map(extent_df$Code, ~{
#   req <- glue::glue("https://apps.epsg.org/api/v1/Extent/{.x}/polygon/") %>%
#     httr::GET()
#
#   if (req$status_code == "200") {
#     req %>%
#       httr::content(as = "text") %>%
#       sf::read_sf(.) %>%
#       sf::st_cast("MULTIPOLYGON") %>%
#       dplyr::mutate(Code = .x)
#   }
#
# })
#
# extent_geom_sf <- dplyr::bind_rows(extent_geom)

# write_rds(extent_geom_sf, "tmp/extent_geom_sf.rds")

extent_geom_sf <- read_rds("tmp/extent_geom_sf.rds")

# Simplify the extent_geom_sf dataset
extent_sf_simple <- rmapshaper::ms_simplify(extent_geom_sf, keep = 0.3, sys = TRUE)

# Check for any oddities
mapview::mapview(extent_sf_simple[1:100,])

# Carry out the data merge process
# 1. Merge shapes to extent to get extent name
# 2. Merge extent to CRS file to get CRS info
# 3. Clean up
crs_extent <- extent_sf_simple %>%
  left_join(extent_df, by = "Code") %>%
  select(Area = Name) %>%
  left_join(crs_df, by = "Area") %>%
  filter(!is.na(Code)) %>%
  transmute(crs_code = as.character(Code),
            crs_name = Name,
            crs_type = Type,
            crs_gcs = Base)


# Step through each code and get the units, proj4string
uts <- purrr::map_chr(crs_extent$crs_code, ~{
  y <- as.numeric(.x)

  get_units <- function(x) {
    ut <- sf::st_crs(x)$units
    if (is.null(ut)) {
      return(NA)
    } else {
      return(ut)
    }

  }
  possible_units <- purrr::possibly(get_units, NA)

  possible_units(y)
})

proj4 <- purrr::map_chr(crs_extent$crs_code, ~{
  y <- as.numeric(.x)

  get_proj4 <- function(x) {
    ut <- sf::st_crs(x)$proj4string
    if (is.null(ut)) {
      return(NA)
    } else {
      return(ut)
    }

  }
  possible_proj4 <- purrr::possibly(get_proj4, NA)

  possible_proj4(y)
})

crs_extent$crs_units <- uts
crs_extent$crs_proj4 <- proj4

crs_sf <- crs_extent %>%
  select(starts_with("crs"), geometry) %>%
  st_transform(32663)

usethis::use_data(crs_sf, compress = "xz", overwrite = TRUE)
