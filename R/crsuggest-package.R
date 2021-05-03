#' Get information on suggested coordinate reference systems for spatial data
#'
#' Uses data from the EPSG Registry to look up suitable coordinate reference
#' system transformations for spatial datasets in R.  Returns a data frame with CRS codes
#' that can be used for CRS transformation and mapping projects.  Please see
#' the EPSG Dataset Terms of Use at \url{https://epsg.org/terms-of-use.html} for more information.
#'
#' @author Kyle Walker
#' @name crsuggest
#' @docType package
#' @import purrr
#' @import sf
#' @import dplyr
#' @importFrom mapview npts
#' @importFrom utils installed.packages
NULL
