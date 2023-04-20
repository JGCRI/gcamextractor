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
data <- readgcam(gcamdatabase = "C:/gcam/gcam-v6.0-Windows-Release-Package/output/database_basexdb_ssp235",
                 folder = "test_gcamv6p0",
                 paramsSelect = c("gdp","pop","agProdByCrop","landAlloc","elecByTechTWh","watWithdrawBySec","emissCO2BySector"),
                 saveData = T)

data$data$param%>%unique()

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
params <- gcamextractor::params; params %>% sort()

gcamdatabase_i = "C:/Z/projects/current/00_IM3/pic_checks/database_rcp85hotter_ssp5"
gcamdata_folder_i = "C:/gcam/gcam-usa-im3/input/gcamdata"
rgcam::localDBConn("C:/Z/projects/current/00_IM3/pic_checks/","database_rcp85hotter_ssp5")
reReadData_i = T
dataProjFile_i = "dataProj_cerf.proj"
regionsSelect_i = c("Global","USA",gcamextractor::map_state_to_gridregion$state%>%unique(),"PR","Alaska grid","California grid","Central East grid","Central Northeast grid",
                    "Central Northwest grid", "Central Southwest grid","Florida grid","Hawaii grid",
                    "Mid-Atlantic grid","New England grid","New York grid","Northwest grid",
                    "Southeast grid","Southwest grid","Texas grid")
folder_i="cerf_test"

# Issue #20
paramsSelect_i = c("cerf")

scenOrigNames_i = c("rcp85hotter_ssp5") # make sure these exist (See outputs of the rgcam::localDBConn)
scenNewNames_i = c("rcp85hotter_ssp5")

dataGCAM <- readgcam(reReadData = reReadData_i,
                     gcamdatabase = gcamdatabase_i,
                     gcamdata_folder = gcamdata_folder_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     scenOrigNames = scenOrigNames_i,
                     scenNewNames = scenNewNames_i,
                     folder = folder_i)

dataGCAM$data %>% dplyr::select(param,scenario) %>% unique()
(dataGCAM$data)$value%>%range()

# On PIC direct
library(rgcam)
dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn("/pic/projects/im3/gcamusa/gcam-usa-im3/output/","database_rcp85hotter_ssp5"),
                                    proj = "test_proj.proj",
                                    scenario = "rcp85hotter_ssp5",
                                    queryFile = "/pic/projects/im3/gcamusa/diagnostics/outputs_CERF/queries_test.xml")
dataProjLoaded <- rgcam::loadProject("test_proj.proj")
queryx <- "elec investment capacity factor"
tbl <- rgcam::getQuery(dataProjLoaded, queryx)

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


# SEASIA Tests

library(dplyr)
library(gcamextractor)
library(rchart)

# Create path to database
cities_db <- "C:/gcam/gcam_v5p3_seasia/output/database_seasia_cities"


# Run gcamextractor for desired database, regions, and parameters
cities <- gcamextractor::readgcam(gcamdatabase = cities_db,
                                  regionsSelect = c("Malaysia", "KualaLumpur", "Rest of Malaysia"),
                                  regionsAggregate = list(c("Malaysia", "KualaLumpur", "Rest of Malaysia")),
                                  regionsAggregateNames = c("All of Malaysia"),
                                  paramsSelect = "pop",
                                  folder = "cities")





#.............
# Check NonCO2
#............

library(gcamextractor); library(dplyr)

# List of params in gcamextractor
params <- gcamextractor::params; params

gcamdatabase_i = "C:/gcam/gcam-v6.0-Windows-Release-Package/output/database_basexdb"
gcamdata_folder_i = "C:/gcam/gcam-v6.0-Windows-Release-Package/input/gcamdata"
rgcam::localDBConn("C:/gcam/gcam-v6.0-Windows-Release-Package/output/","database_basexdb")
reReadData_i = T
dataProjFile_i = "dataProj_nonCO2.proj"
regionsSelect_i = NULL
folder_i="nonCO2_test"

# Issue #20
paramsSelect_i = c("emissGHGByGasGWPAR5")

dataGCAM <- readgcam(reReadData = F,
                     gcamdatabase = gcamdatabase_i,
                     gcamdata_folder = gcamdata_folder_i,
                     dataProjFile = dataProjFile_i,
                     regionsSelect = regionsSelect_i,
                     paramsSelect = paramsSelect_i,
                     folder = folder_i)

dataGCAM$dataAggParam
dataGCAM$dataAggParam$param %>% unique()
dataGCAM$data$class1 %>% unique()
dataGCAM$dataAll%>%tail() %>% as.data.frame()


##
conn <- rgcam::localDBConn("C:/gcam/gcam-v6.0-Windows-Release-Package/output/",
                           "database_basexdb",migabble = FALSE)
prj <- rgcam::addScenario(conn,
                          "prj.proj",
                          "Reference",
                          "C:/Z/models/gcamextractor/inst/extdata/queries_check.xml")
