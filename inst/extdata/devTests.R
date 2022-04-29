library(gcamextractor); library(dplyr)


# List of params in gcamextractor
params <- gcamextractor::data_params; params

gcamdatabase_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/output/database_SSP5"
gcamdata_folder_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/input/gcamdata"
rgcam::localDBConn("C:/Z/models/GCAMVersions/gcam-usa-im3/output","database_SSP5")
reReadData_i = F
dataProjFile_i = "C:/Z/models/gcamextractor/cerf/dataProj_cerf.proj"
regionsSelect_i = NULL
paramsSelect_i = "cerf"
folder_i="cerf"

dataGCAM <- readgcam(reReadData = reReadData_i,
                     gcamdatabase = gcamdatabase_i,
                     gcamdata_folder = gcamdata_folder_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)

reReadData = reReadData_i
reReadData = F
gcamdatabase = gcamdatabase_i
gcamdata_folder = gcamdata_folder_i
dataProjFile = dataProjFile_i
regionsSelect = regionsSelect_i
paramsSelect = paramsSelect_i
folder = folder_i
queryFile=NULL
scenOrigNames = "All"
scenNewNames = NULL


# List of params in gcamextractor for GO
params <- gcamextractor::data_params; params

gcamdatabase_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/output/database_SSP5"
rgcam::localDBConn("C:/Z/models/GCAMVersions/gcam-usa-im3/output","database_SSP5")
reReadData_i = F
dataProjFile_i = "dataProj_go.proj"
regionsSelect_i = NULL
paramsSelect_i = "go"
folder_i="go"

dataGCAM <- readgcam(reReadData = reReadData_i,
                     gcamdatabase = gcamdatabase_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)

reReadData = reReadData_i
reReadData = F
gcamdatabase = gcamdatabase_i
gcamdata_folder = gcamdata_folder_i
dataProjFile = dataProjFile_i
regionsSelect = regionsSelect_i
paramsSelect = paramsSelect_i
folder = folder_i
queryFile=NULL

# Test "emissNonCO2BySector"
params <- gcamextractor::data_params; params

gcamdatabase_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/output/database_SSP5"
rgcam::localDBConn("C:/Z/models/GCAMVersions/gcam-usa-im3/output","database_SSP5")
reReadData_i = T
dataProjFile_i = "dataProj_testNonCO2"
regionsSelect_i = NULL
paramsSelect_i = "emissNonCO2BySector"
folder_i="testNonCO2"


dataGCAM <- readgcam(reReadData = reReadData_i,
                     maxMemory = "2g",
                     gcamdatabase = gcamdatabase_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)

(dataGCAM$dataAll %>% dplyr::filter(param=="emissNonCO2BySector"))$subRegion%>%unique()



dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn("C:/Z/models/GCAMVersions/gcam-usa-im3/output",
                                                              "database_SSP5",
                                                              migabble = T,
                                                              maxMemory = "1g"),
                                    proj = gsub("//","/",paste(getwd(), "/", "dataProj.proj", sep = "")),
                                    scenario = "SSP5",
                                    queryFile = gsub("//","/",paste("C:/Z/models/gcamextractor/testNonCO2",
                                                                    "/subSetQueries.xml", sep = "")))

dataProjLoaded <- rgcam::loadProject(gsub("//","/",paste(getwd(), "/", "dataProj.proj", sep = "")))


# Check For PIC

rgcam::localDBConn( "/pic/projects/im3/gcamusa/gcam-usa-im3/output/",
                    "database_Ref_RCP8p5_NORESM_5trail_delta_applied2015",
                   migabble = T,
                   maxMemory = "8g")

dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn("/pic/projects/im3/gcamusa/gcam-usa-im3/output/",
                                                              "database_Ref_RCP8p5_NORESM_5trail_delta_applied2015",
                                                              migabble = T,
                                                              maxMemory = "8g"),
                                    proj = gsub("//","/",paste(getwd(), "/", "dataProj.proj", sep = "")),
                                    scenario = "Ref_RCP8p5_NORESM_5trail_delta_applied2015",
                                    queryFile = gsub("//","/",paste("/pic/projects/im3/gcamusa/diagnostics/outputs_runoff_GCMs_5trail_delta",
                                                                    "/subSetQueries.xml", sep = "")))

dataProjLoaded <- rgcam::loadProject(gsub("//","/",paste(getwd(), "/", "dataProj.proj", sep = "")))

# Check for Yang Ou db
library(gcamextractor)
gcamdatabase_i = "C:/Z/models/tests/database_basexdb"
rgcam::localDBConn("C:/Z/models/tests","database_basexdb")
reReadData_i = T
dataProjFile_i = "dataProj_yang_test.proj"
regionsSelect_i = NULL
paramsSelect_i = c("electricity")
folder_i="yang_test"

dataGCAM <- readgcam(reReadData = reReadData_i,
                     gcamdatabase = gcamdatabase_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)

# Check for Yang Ou db
library(gcamextractor); library(dplyr)
gcamdatabase_i = "database_rcp85hotter_ssp5_runoff"
rgcam::localDBConn("C:/Z/models/gcamextractor","database_rcp85hotter_ssp5_runoff")
reReadData_i = T
dataProjFile_i = "dataProj_test.proj"
regionsSelect_i = NULL
paramsSelect_i = c("pop")
folder_i="testx"

dataGCAM <- readgcam(reReadData = reReadData_i,
                     gcamdatabase = gcamdatabase_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)

reReadData = reReadData_i
gcamdatabase = gcamdatabase_i
dataProjFile = dataProjFile_i
regionsSelect = regionsSelect_i
paramsSelect = paramsSelect_i
folder = folder_i

