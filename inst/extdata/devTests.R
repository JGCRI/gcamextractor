library(gcamextractor); library(dplyr)


# List of params in gcamextractor
params <- gcamextractor::data_params; params

gcamdatabase_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/output/database_SSP5"
gcamdata_folder_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/input/gcamdata"
rgcam::localDBConn("C:/Z/models/GCAMVersions/gcam-usa-im3/output","database_SSP5")
reReadData_i = T
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
gcamdatabase = gcamdatabase_i
gcamdata_folder = gcamdata_folder_i
dataProjFile = dataProjFile_i
regionsSelect = regionsSelect_i
paramsSelect = paramsSelect_i
folder = folder_i



dataGCAM$data
dataGCAM$dataAggClass1
(dataGCAM$dataAggClass1)$subRegion%>%unique()


reReadData = F
#gcamdatabase = gcamdatabase_i,
dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj.proj"
regionsSelect = "Colombia"
paramsSelect= param_i

