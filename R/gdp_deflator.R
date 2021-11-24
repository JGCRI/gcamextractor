#' gdp_deflator
#'
#' From GCAM.
#' Calculate a gross domestic product (GDP) implicit price deflator between two years.
#'
#' The GDP deflator is a measure of price inflation with respect to a
#' specific base year; it allows us to back out the effects of inflation when we
#' compare prices over time.  This function calculates a deflator given a base
#' year (the year to convert from) and a conversion year (the year to convert
#' to).  To use the deflator, multiply prices in base-year dollars by the deflator; the
#' result will be prices in the converted dollar year.
#'
#' @param year Year to convert TO.
#' @param base_year Year to convert FROM.
#' @return GDP Deflator.  Multiply to convert FROM \code{base_year} dollars TO
#' \code{year} dollars.
#' @source U.S. Bureau of Economic Analysis, Gross domestic product (implicit
#' price deflator) [A191RD3A086NBEA], retrieved from FRED, Federal Reserve Bank
#' of St. Louis; https://fred.stlouisfed.org/series/A191RD3A086NBEA, April 12,
#' 2017
#' @author BBL
#' @export

gdp_deflator <- function(year, base_year) {
  # This time series is the BEA "A191RD3A086NBEA" product
  # Downloaded April 13, 2017 from https://fred.stlouisfed.org/series/A191RD3A086NBEA
  gdp_years <- 1929:2019
  gdp <- c(9.896, 9.535, 8.555, 7.553, 7.345, 7.749, 7.908, 8.001, 8.347,
           8.109, 8.033, 8.131, 8.68, 9.369, 9.795, 10.027, 10.288, 11.618,
           12.887, 13.605, 13.581, 13.745, 14.716, 14.972, 15.157, 15.298,
           15.559, 16.091, 16.625, 17.001, 17.237, 17.476, 17.669, 17.886,
           18.088, 18.366, 18.702, 19.227, 19.786, 20.627, 21.642, 22.784,
           23.941, 24.978, 26.337, 28.703, 31.361, 33.083, 35.135, 37.602,
           40.706, 44.377, 48.52, 51.53, 53.565, 55.466, 57.24, 58.395,
           59.885, 61.982, 64.392, 66.773, 68.996, 70.569, 72.248, 73.785,
           75.324, 76.699, 78.012, 78.859, 80.065, 81.887, 83.754, 85.039,
           86.735, 89.12, 91.988, 94.814, 97.337, 99.246, 100, 101.221,
           103.311, 105.214, 106.913, 108.828, 109.998, 111.445, 113.545,
           116.311, 118.339)
  names(gdp) <- gdp_years

  as.vector(unlist(gdp[as.character(year)] / gdp[as.character(base_year)]))
}
