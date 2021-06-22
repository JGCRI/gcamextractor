library(gcamextractor)


# List of params in gcamextractor
params <- gcamextractor::mappings()
params <- unique(params$mapParamQuery$param); params

gcamdatabase_i = "C:/Z/models/GCAMVersions/gcam-core_stash/output/database_5p3_2020"
param_i = "energyFinalConsumBySecEJ"


dataGCAM <- readgcam(reReadData = T,
                     gcamdatabase = gcamdatabase_i,
                     dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj.proj",
                     regionsSelect = "USA",
                     paramsSelect= "all")

reReadData = F
#gcamdatabase = gcamdatabase_i,
dataProjFile = "C:/Z/models/gcamextractor/outputs/readGCAM/dataProj.proj"
regionsSelect = "Colombia"
paramsSelect= param_i

