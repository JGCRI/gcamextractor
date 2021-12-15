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
