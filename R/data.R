#-----------------
# Internal Data
#-----------------

#' queries xml file
#'
#' @source gcamextractor
#' @format .xml
#' @examples
#' \dontrun{
#'  library(gcamextractor); library(XML)
#'  gcamextractor::queries
#'  # Can save xml
#'  XML::saveXML(gcamextractor::queries, file=paste(getwd(), "/queries.xml", sep = ""))
#' }
"queries"


#' data_capfactors
#'
#' @source paste(getwd(),"/dataFiles/gcam/capacity_factor_by_elec_gen_subsector.csv",sep="")
#' @format .csv
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::data_capfactors
#' }
"data_capfactors"


#' example_gcamv54_argentina_colombia_2025_proj
#'
#' @source readgcam() run outputs saved
#' @format proj file
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::example_gcamv54_argentina_colombia_2025
#' }
"example_gcamv54_argentina_colombia_2025_proj"

#' gcamextractor params
#'
#' @source from gcamextractor::mappings()$mapParamQuery$param
#' @format R table
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::data_params
#' }
"data_params"

#' gcamextractor queries
#'
#' @source from gcamextractor::mappings()$mapParamQuery$query
#' @format R table
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::data_queries
#' }
"data_queries"



#-------------------
# gcamextractor mapping between params and quereies
#-------------------

#' map_param_query
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::map_param_query
#' }
"map_param_query"


#-------------------
# Conversions
#-------------------

#' convert
#'
#' @source gcamextractor
#' @format list
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  names(gcamextractor::convert)
#' }
"convert"


#-------------------
# Global Warming Potentials
#-------------------

#' data_GWP
#'
#' Emissions Conversion to CO2eq
#' GWP conversions - uses 100-yr GWPs from IPPC AR4 and AR5
#' https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
#' Does not include all covnersions. Add them if they are tracked in GCAM
#' Mean HFC numbers GWP %>% dplyr::filter(grepl("HFC",ghg)) %>% summarize_at(vars(names(GWP)[!grepl("ghg",names(GWP))]),funs(mean))
#' GTP AR5 Box 3.2 Table 1 https://www.ipcc.ch/site/assets/uploads/2018/02/SYR_AR5_FINAL_full.pdf
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::data_GWP
#' }
"data_GWP"


#' conv_GgTg_to_MTC
#'
#' https://nepis.epa.gov/Exe/ZyNET.exe/P1001YTS.txt?ZyActionD=ZyDocument&Client=EPA&Index=2000%20Thru%202005&Docs=&Query=&Time=&EndTime=&SearchMethod=1&TocRestrict=n&Toc=&TocEntry=&QField=&QFieldYear=&QFieldMonth=&QFieldDay=&UseQField=&IntQFieldOp=0&ExtQFieldOp=0&XmlQuery=&File=D%3A%5CZYFILES%5CINDEX%20DATA%5C00THRU05%5CTXT%5C00000017%5CP1001YTS.txt&User=ANONYMOUS&Password=anonymous&SortMethod=h%7C-&MaximumDocuments=1&FuzzyDegree=0&ImageQuality=r75g8/r75g8/x150y150g16/i425&Display=hpfr&DefSeekPage=x&SearchBack=ZyActionL&Back=ZyActionS&BackDesc=Results%20page&MaximumPages=1&ZyEntry=3
#' https://www.firescience.gov/projects/09-2-01-9/supdocs/09-2-01-9_Appendix_C_-_Unit_Conversion_and_Other_Tables.pdf
#' MTC is megatonnes (10^6 tonnes) of Carbon.
#' Tg one terragram = 1 Megatonne = 10^6 tonnes
#' Convert to Mega tonnes of Carbon
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::conv_GgTg_to_MTC
#' }
"conv_GgTg_to_MTC"




#--------------------------------------------------------------------------------------------------
# GCAM Regions
#--------------------------------------------------------------------------------------------------

#' map_country_to_gcam_region
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::map_country_to_gcam_region
#' }
"map_country_to_gcam_region"


#' regions_US52
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::regions_US52
#' }
"regions_US52"


#' regions_US49
#'
#' # GCAM USA 49. Excludes Alaska, Hawaii and Puerto Rico. Includes DC.
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::regions_US49
#' }
"regions_US49"


#' regions_gcam32
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::regions_gcam32
#' }
"regions_gcam32"


#' regions_gcam_basins
#'
#' @source gcamextractor
#' @format tibble
#' @examples
#' \dontrun{
#'  library(gcamextractor);
#'  gcamextractor::regions_gcam_basins
#' }
"regions_gcam_basins"





