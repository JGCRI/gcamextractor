library(gcamextractor); library(dplyr)

# List of params in gcamextractor
params <- gcamextractor::data_params; params

gcamdatabase_i = "C:/Z/projects/current/00_IM3/pic_checks/databases/database_rcp85hotter_ssp5_runoff"
#gcamdatabase_i = "C:/Z/models/gcamextractor/database_rcp85hotter_ssp5_runoff"
gcamdata_folder_i = "C:/gcam/gcam-usa-im3/input/gcamdata"
rgcam::localDBConn("C:/Z/projects/current/00_IM3/pic_checks/databases/","database_rcp85hotter_ssp5_runoffx")
reReadData_i = T
dataProjFile_i = "dataProj_cerf.proj"
regionsSelect_i = "United States"
paramsSelect_i = "cerf"
folder_i="cerf_test"

dataGCAM <- readgcam(reReadData = reReadData_i,
                     gcamdatabase = gcamdatabase_i,
                     gcamdata_folder = gcamdata_folder_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)


# reReadData = T
# gcamdatabase = gcamdatabase_i
# gcamdata_folder = gcamdata_folder_i
# dataProjFile = dataProjFile_i
# regionsSelect = regionsSelect_i
# paramsSelect = paramsSelect_i
# folder = folder_i
# queryFile=NULL
# scenOrigNames = "All"
# scenNewNames = NULL
