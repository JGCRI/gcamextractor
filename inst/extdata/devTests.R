library(gcamextractor)


dataGCAM <- readgcam(#reReadData = T,
                     #dirOutputs = dirOutputs_i,
                     #gcamdatabase = "C:/Z/models/GCAMVersions/gcam-core_v5.3_IM3_usa-dispatch-merge/output/database_NonCO2",
                     #scenOrigNames = scenOrigNames_i[[db_i]],
                     #scenNewNames = scenNewNames_i[[db_i]],
                     dataProjFile = "C:/Z/models/gcamextractor/outputs/dataProj.proj",
                     regionsSelect = "Colombia",
                     paramsSelect= c("emissCO2BySector","emissNonCO2BySector")
                     )



