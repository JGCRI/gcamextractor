#..........................
# Check with releases of GCAM
#..........................
library(gcamextractor);library(dplyr); library(rgcam)

# List of params in gcamextractor
params <- gcamextractor::params; params

# Check
# rgcam::localDBConn("C:/gcam/gcam-v6.0-Windows-Release-Package/output/","database_basexdb")

paramsSelect_i=(gcamextractor::map_param_query %>% dplyr::filter(group=="diagnostic"))$param; paramsSelect_i
length(paramsSelect_i)

# GCAM 6.0
data <- readgcam(gcamdatabase = "C:/gcam/gcam-v6.0-Windows-Release-Package/output/database_basexdb",
                 folder = "test_gcamv6p0",
                 paramsSelect = "All",
                 saveData = F)
# GCAM 5.4
data <- readgcam(gcamdatabase = "C:/gcam/gcam-v5.4-Windows-Release-Package/output/database_basexdb",
                 folder = "test_gcamv5p4",
                 paramsSelect = paramsSelect_i[23],
                 saveData = F)

#..........................
# EXAMPLE FOR DOCS - gcamextractor - rchart - rmap
#..........................
library(gcamextractor); library(rmap); library(rchart)

# List of params in gcamextractor
params <- gcamextractor::params; params

# Extract Data
data <- readgcam(gcamdatabase = "C:/gcam/gcam_v5p3_seasia/output/database_basexdb_seasia_breakout_cities_OFF",
                 paramsSelect = c("pop","elecByTechTWh"),
                 folder = "test_folder")

# View extracted data
names(data)
head(data$dataAggParam)
head(data$dataAggClass1)

# Filter population data to specific countries and years
data_chart <- data$dataAggClass1 %>%
                dplyr::filter(subRegion %in% c("Argentina", "Colombia"))

# Plot data with rchart
charts <- rchart::chart(data_chart)


# Prep Data
data_map <- data$dataAggClass1 %>%
  dplyr::filter(x %in% c("2015"))

# Plot with rmap
maps <- rmap::map(data_map,
                  underLayer = rmap::mapCountries,
                  background = T)


#----------------------
# Test Proj file
#----------------------

library(gcamextractor); library(rmap); library(rchart)
df <- gcamextractor::readgcam(dataProjFile = gcamextractor::example_gcamv54_argentina_colombia_2025_proj)


#----------------------
# Test Proj file
#----------------------
library(gcamextractor); library(rmap); library(rchart)

data <- readgcam(gcamdatabase = "C:/gcam/gcam-v5.4-Windows-Release-Package/output/database_basexdb",
                 paramsSelect = c("watWithdrawBySec"),
                 folder = "example_gcam_annual_2022")


data$dataAggClass1$class%>%unique()
data_map <- data$dataAggClass1 %>%
  dplyr::filter(x %in% c(2015))

maps <- rmap::map(data_map%>%filter(param=="elecByTechTWh"))


#----------------------
# CERF Debug
#----------------------

library(gcamextractor); library(dplyr)

# List of params in gcamextractor
params <- gcamextractor::params; params

gcamdatabase_i = "C:/Z/projects/current/00_IM3/pic_checks/databases/database_rcp85hotter_ssp5_runoff"
gcamdata_folder_i = "C:/gcam/gcam-usa-im3/input/gcamdata"
rgcam::localDBConn("C:/Z/projects/current/00_IM3/pic_checks/databases/","database_rcp85hotter_ssp5_runoff")
reReadData_i = T
dataProjFile_i = "dataProj_cerf.proj"
regionsSelect_i = NULL
folder_i="cerf_test"

# Issue #20
paramsSelect_i = c("elec_heat_rate_BTUperkWh")

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





