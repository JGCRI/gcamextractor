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


#' Example GCAM .proj file
#'
#' @source readgcam() run outputs saved
#' @format R table or .csv
#' @examples
#' \dontrun{
#'  library(gcmaextractor);
#'  gcmaextractor::exampleGCAMproj
#' }
"exampleGCAMproj"
