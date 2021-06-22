#-----------------
# Internal Data
#-----------------

#' queries xml file
#'
#' @source gcmaextractor
#' @format .xml
#' @examples
#' \dontrun{
#'  library(gcmaextractor); library(XML)
#'  gcmaextractor::queries
#'  # Can save xml
#'  XML::saveXML(gcmaextractor::queries, file=paste(getwd(), "/queries.xml", sep = ""))
#' }
"queries"


#' data_capfactors
#'
#' @source paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(gcmaextractor);
#'  gcmaextractor::data_capfactors
#' }
"data_capfactors"


#' Example GCAMv5.2 .proj file
#'
#' @source readgcam() run outputs saved
#' @format proj file
#' @examples
#' \dontrun{
#'  library(gcmaextractor);
#'  gcmaextractor::example_GCAMv52_2050_proj
#' }
"example_GCAMv52_2050_proj"

#' Example GCAMv5.3 .proj file
#'
#' @source readgcam() run outputs saved
#' @format proj file
#' @examples
#' \dontrun{
#'  library(gcmaextractor);
#'  gcmaextractor::example_GCAMv53_2020_proj
#' }
"example_GCAMv53_2020_proj"

#' gcamextractor params
#'
#' @source from gcamextractor::mappings()$mapParamQuery$param
#' @format R table
#' @examples
#' \dontrun{
#'  library(gcmaextractor);
#'  gcmaextractor::data_params
#' }
"data_params"

#' gcamextractor queries
#'
#' @source from gcamextractor::mappings()$mapParamQuery$query
#' @format R table
#' @examples
#' \dontrun{
#'  library(gcmaextractor);
#'  gcmaextractor::data_queries
#' }
"data_queries"
