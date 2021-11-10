library(gcamextractor); library(dplyr)


# List of params in gcamextractor
params <- gcamextractor::data_params; params

gcamdatabase_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/output/database_SSP5"
gcamdata_folder_i = "C:/Z/models/GCAMVersions/gcam-usa-im3/input/gcamdata"
rgcam::localDBConn("C:/Z/models/GCAMVersions/gcam-usa-im3/output","database_SSP5")

# reReadData = F
# gcamdatabase = gcamdatabase_i
# gcamdata_folder = gcamdata_folder_i
# dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj.proj"
# regionsSelect = NULL
# paramsSelect= "all"

dataGCAM <- readgcam(reReadData = T,
                     gcamdatabase = gcamdatabase_i,
                     gcamdata_folder = gcamdata_folder_i,
                     dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj_cerf.proj",
                     regionsSelect = NULL,
                     paramsSelect = "cerf",
                     folder = "cerf")

# reReadData = T
# gcamdatabase = gcamdatabase_i
# gcamdata_folder = gcamdata_folder_i
# dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj_cerf.proj"
# regionsSelect = NULL
# paramsSelect= "cerf"



dataGCAM$data
dataGCAM$dataAggClass1
(dataGCAM$dataAggClass1)$subRegion%>%unique()


reReadData = F
#gcamdatabase = gcamdatabase_i,
dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj.proj"
regionsSelect = "Colombia"
paramsSelect= param_i

