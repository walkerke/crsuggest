library(tidyverse)
# Step 1: read in and convert .mdb database
mdb_path <- "data-raw/EPSG-v10_015-Access.mdb"

tables <- Hmisc::mdb.get(mdb_path, tables = TRUE)

names(tables) <- tables

table_list <- purrr::map(tables, ~{
  Hmisc::mdb.get(mdb_path, tables = .x)
})

crs_df <- table_list$`Coordinate Reference System` %>%
  dplyr::select(area_code = AREA.OF.USE.CODE,
                crs_type = COORD.REF.SYS.KIND,
                crs_code = COORD.REF.SYS.CODE,
                crs_name = COORD.REF.SYS.NAME,
                crs_gcs = BASE.CRS.CODE) %>%
  dplyr::mutate_each(as.character)

# Step through each code and get the units, proj4string
uts <- purrr::map_chr(crs_df$crs_code, ~{
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

proj4 <- purrr::map_chr(crs_df$crs_code, ~{
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

crs_df$crs_units <- uts
crs_df$crs_proj4 <- proj4

crs_sf <- sf::st_read("data-raw/EPSG_Polygons.shp") %>%
  dplyr::transmute(area_code = as.character(AREA_CODE)) %>%
  dplyr::left_join(crs_df, by = "area_code") %>%
  sf::st_transform(32663)

usethis::use_data(crs_sf, compress = "xz", overwrite = TRUE)
