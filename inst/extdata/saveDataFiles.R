#-----------------
# Load Libraries
#-----------------
library(tibble);library(dplyr);library(devtools); library(rgcam); library(usethis)

#-------------------
# Data Files
#-------------------

# Example .proj file
#projFile <-"C:/Z/projects/metisGCAMUSA/metisOutputs/readGCAM/exampleGCAMproj.proj"
projFile <-paste0(getwd(),"/inst/extdata/example_GCAMv52_2050_proj.proj")
example_GCAMv52_2050_proj <- rgcam::loadProject(projFile)
use_data(example_GCAMv52_2050_proj, overwrite=T)

#projFile <-"C:/Z/projects/metisGCAMUSA/metisOutputs/readGCAM/exampleGCAMproj.proj"
projFile <-paste0(getwd(),"/inst/extdata/example_GCAMv53_2020_proj.proj")
example_GCAMv53_2020_proj <- rgcam::loadProject(projFile)
use_data(example_GCAMv53_2020_proj, overwrite=T)

# Metis XMl Query Files
xmlFilePath = paste0(getwd(),"/inst/extdata/queries.xml")
xmlfile <- XML::xmlTreeParse(xmlFilePath)
xmltop <- XML::xmlRoot(xmlfile)
top <- XML::xmlNode(XML::xmlName(xmltop))
for(i in 1:length(xmltop)){
      top <- XML::addChildren(top, xmltop[[i]])
}
queries <- top
use_data(queries, overwrite=T)

# Capacity factors
data_capfactors <- data.table::fread(file=paste0(getwd(),"/inst/extdata/capacity_factor_by_elec_gen_subsector.csv"),skip=5,encoding="Latin-1")
use_data(data_capfactors, overwrite=T)


data_params <- unique(gcamextractor::mappings()$mapParamQuery$param); data_params
data_queries <- unlist(unique(gcamextractor::mappings()$mapParamQuery$query)); data_queries
use_data(data_params, overwrite=T)
use_data(data_queries, overwrite=T)
