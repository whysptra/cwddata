#' Our World in Data's Key Crop Yields
#'
#' A dataset containing crop yields for countries over time. Yields
#' are measured in tonnes per hectare for seven crops.
#'
#' @format a tibble with 143825 observations and 5 variables:
#'
#' - **entity**: full country name
#' - **code**: three letter country code
#' - **year**: year from 1961 to 2018
#' - **crop**: one of bananas, barley, beans, cassava, cocoa, maize, peas, potatoes, rice, soybeans, wheat
#' - **tonnes_per_hectare**: the total yield of the crop, measured in tonnes per hectare
#'
#'
#'@import tibble
#'
#'@source [Our World in Data](https://ourworldindata.org/crop-yields)  and [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-01/readme.md)
"key_crop_yields"
