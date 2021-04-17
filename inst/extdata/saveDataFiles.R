#-----------------
# Load Libraries
#-----------------
library(tibble);library(dplyr);library(devtools); library(rgcam); library(usethis)

#-------------------
# Data Files
#-------------------

# Example .proj file
#projFile <-"C:/Z/projects/metisGCAMUSA/metisOutputs/readGCAM/exampleGCAMproj.proj"
projFile <-paste0(getwd(),"/inst/extdata/exampleGCAM52releaseSSP3SSP52050.proj")
exampleGCAMproj <- rgcam::loadProject(projFile)
use_data(exampleGCAMproj, overwrite=T)

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

