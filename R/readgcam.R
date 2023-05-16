#' readgcam
#'
#' This function connects to a gcamdatabase and uses a query file to
#' out results into a table ready for plotting.
#' @param folder Default = getwd(). Path to directory for outputs
#' @param nameAppend  Default="". Name to append to saved files.
#' @param gcamdata_folder (OPTIONAL) Default=NULL. Full path to gcamdata folder. Required for some params.
#' @param gcamdatabase Default = NULL. Full path to GCAM database folder.
#' @param queryFile Defualt = NULL. When NULL gcamextractor loads pre-saved xml file gcamextractor::queries_xml
#' @param dataProjFile Default = NULL. Optional. A default 'dataProj.proj' is produced if no .Proj file is specified.
#' @param maxMemory Default = "4g". Set the maxMemory. Sometimes need to increase this for very large data.
#' @param scenOrigNames Default = "All". Original Scenarios names in GCAM database in a string vector.
#' For example c('scenario1','scenario2).
#' @param scenNewNames New Names which may be shorter and more useful for figures etc.
#' Default will use Original Names. For example c('scenario1','scenario2)
#' @param reReadData If TRUE will read the GCAM data base and create a queryData.proj file
#' in the same folder as the GCAM database. If FALSE will load a '.proj' file if a file
#' with full path is provided otherwise it will search for a dataProj.proj file in the existing
#' folder which may have been created from an old run.
#' @param regionsSelect Default = NULL. The regions to analyze in a vector. Example c('Colombia','Argentina'). Full list:
#' USA, Africa_Eastern, Africa_Northern, Africa_Southern, Africa_Western, Australia_NZ, Brazil, Canada
#' Central America and Caribbean, Central Asia, China, EU-12, EU-15, Europe_Eastern, Europe_Non_EU,
#' European Free Trade Association, India, Indonesia, Japan, Mexico, Middle East, Pakistan, Russia,
#' South Africa, South America_Northern, South America_Southern, South Asia, South Korea, Southeast Asia,
# Taiwan, Argentina, Colombia, Uruguay)
#' @param regionsAggregate Default = NULL. Vector or list of vectors containing regions to aggregate into a new region.
#' Example c('South America_Northern', 'South America_Southern') will add one aggregated region while
#' list(c('South America_Northern', 'South America_Southern'), c('USA', 'Canada', 'Mexico')) will add two aggregated regions.
#' @param regionsAggregateNames Default = NULL. Vector of names for aggregated regions. Length must be 1 if regionsAggregate
#' is a vector or match the length of the list given by regionsAggregate. Example: c('South America', 'North America')
#' @param paramsSelect Default = "diagnostic".
#'
#' Choose "All" or paramSet from "energy", "electricity", "transport",
#' "water" , "socioecon" ,"ag" , "livestock" ,"land"  ,"emissions".
#'
#' Or pick an individual param from the list:
#'
#' # energy
#' "energyPrimaryByFuelEJ","energyPrimaryRefLiqProdEJ",
#' "energyFinalConsumBySecEJ","energyFinalByFuelEJ","energyFinalSubsecByFuelTranspEJ",
#' "energyFinalSubsecByFuelBuildEJ", "energyFinalSubsecByFuelIndusEJ","energyFinalSubsecBySectorBuildEJ",
#' "energyPrimaryByFuelMTOE","energyPrimaryRefLiqProdMTOE",
#' "energyFinalConsumBySecMTOE","energyFinalbyFuelMTOE","energyFinalSubsecByFuelTranspMTOE",
#' "energyFinalSubsecByFuelBuildMTOE", "energyFinalSubsecByFuelIndusMTOE","energyFinalSubsecBySectorBuildMTOE",
#' "energyPrimaryByFuelTWh","energyPrimaryRefLiqProdTWh",
#' "energyFinalConsumBySecTWh","energyFinalbyFuelTWh","energyFinalSubsecByFuelTranspTWh",
#' "energyFinalSubsecByFuelBuildTWh", "energyFinalSubsecByFuelIndusTWh","energyFinalSubsecBySectorBuildTWh",
#'
#' # electricity
#' "elecByTechTWh","elecCapByFuel","elecFinalBySecTWh","elecFinalByFuelTWh", "elecConsumByDemandSector",
#' "elecNewCapCost","elecNewCapGW","elecAnnualRetPrematureCost","elecAnnualRetPrematureGW","elecCumCapCost","elecCumCapGW","elecCumRetPrematureCost","elecCumRetPrematureGW",
#'
#' # transport
#' "transportPassengerVMTByMode", "transportFreightVMTByMode", "transportPassengerVMTByFuel", "transportFreightVMTByFuel",
#'
#' # buildings
#' "serviceOutputByTechBuildings", "buildingFloorspace",
#'
#' # water
#' "watConsumBySec", "watWithdrawBySec", "watWithdrawByCrop", "watBioPhysCons", "watIrrWithdrawBasin","watIrrConsBasin",
#'
#' # socioecon
#' "gdpPerCapita", "gdp", "gdpGrowthRate", "pop",
#'
#'  # ag
#'  "agProdbyIrrRfd", "agProdBiomass", "agProdForest","agProdByCrop",
#'
#'  # livestock
#' "livestock_MeatDairybyTechMixed","livestock_MeatDairybyTechPastoral","livestock_MeatDairybyTechImports", "livestock_MeatDairybySubsector",
#'
#' # land
#' "landIrrRfd", "landIrrCrop","landRfdCrop", "landAlloc","landAllocByCrop",
#'
#'  # emissions
#' "emissLUC", "emissGHGBySectorGWPAR5","emissGHGBySectorGTPAR5",
#' "emissNonCO2ByResProdGWPAR5", "emissBySectorGWPAR5FFI","emissMethaneBySourceGWPAR5",
#' "emissByGasGWPAR5FFI", "emissByGasGWPAR5LUC", "emissBySectorGWPAR5LUC",
#' "emissNonCO2ByResProdGTPAR5", "emissBySectorGTPAR5FFI","emissMethaneBySourceGTPAR5",
#' "emissByGasGTPAR5FFI", "emissByGasGTPAR5LUC","emissBySectorGTPAR5LUC",
#' "emissCO2BySectorNoBio",
#'
#' # hydrogen
#'
#'
#' @param saveData Default = "T". Set to F if do not want to save any data to file.
#' @param exogenousNoBio Default = FALSE. For "no bio" emissions queries, should negative biomass CO2 emissions accounting be done exogenously rather than using the 'CO2 emissions by sector (no bio)' query?
#' @return A list with the scenarios in the gcam database, queries in the queryxml file and a
#' tibble with gcam data formatted for gcamextractor charts aggregated to different categories.
#' These include data, dataAggParam, dataAggClass1, dataAggClass2.
#' @keywords gcam, gcam database, query
#' @importFrom magrittr %>%
#' @export


readgcam <- function(gcamdatabase = NULL,
                     gcamdata_folder = NULL,
                     queryFile = NULL,
                     dataProjFile = "dataProj.proj",
                     maxMemory = "4g",
                     scenOrigNames = "All",
                     scenNewNames = NULL,
                     reReadData = T,
                     regionsSelect = NULL,
                     regionsAggregate = NULL,
                     regionsAggregateNames = NULL,
                     paramsSelect = "diagnostic",
                     folder = getwd(),
                     nameAppend = "",
                     saveData = T,
                     exogenousNoBio = F
){


  # gcamdatabase = NULL
  # gcamdata_folder = NULL
  # maxMemory = "4g"
  # dataProjFile = "dataProj.proj"
  # queryFile = NULL
  # scenOrigNames = "All"
  # scenNewNames = NULL
  # reReadData = T
  # regionsSelect = NULL
  # paramsSelect="diagnostic"
  # folder=paste(getwd(), "/outputs", sep = "")
  # nameAppend=""
  # saveData = T
  # exogenousNoBio = F

  #................
  # Initialize variables by setting to NULL
  #................

  NULL -> vintage -> year -> xLabel -> x -> value -> sector -> scenario -> region -> param -> origX -> origValue ->
    origUnits -> origScen -> origQuery -> classPalette2 -> classPalette1 -> classLabel2 -> classLabel1 -> class2 ->
    class1 -> connx -> aggregate -> Units -> sources -> paramx -> fuel -> technology -> input -> output -> water ->
    landleaf -> ghg -> Convert -> regionsSelectAll->cf1971to2100->gcamCapacityFactor -> . -> GWPAR5 -> tblelecByTechTWh ->
    totalFFINonCO2 -> FracBioFuel -> FracFossilFuel -> TotalLiquids -> agg_tech->scenarios->queries->
    class_temp -> resource -> subRegAreaSum -> subsector->tblFinalNrgIntlAvShipMod -> 'transportation' ->
    'International Aviation' -> 'International Ship' -> 'International Aviation oil' -> 'a oil' ->
    'International Ship oil' -> 'International Aviation liquids' -> liquids -> 'International Ship liquids'->crop->
    paramsSelectAll -> tblFinalNrgIntlAvShip -> datax -> group -> basin -> subRegion -> query -> subresource ->
    transport -> gcamdata -> half.life -> lifetime -> read.csv -> sector_1 -> steepness -> PrimaryFuelCO2Coef ->
    PrimaryFuelCO2Coef.name -> country -> grid_region -> 'io-coefficient' -> 'minicam.energy.input' ->
    'remove.fraction' ->  state ->  subsector.name -> 'to.technology' -> coefficient -> tbl_carbon_capture_rate ->
    gcamdata_files -> aggSector -> tblGHGEmissRes -> segment -> hours -> scenarios_new -> str -> subRegions_new -> x_new ->
    sector2 -> sector1 -> nodeinput -> scenOrigNames_i -> tech -> tblNonCO2EmissUSA -> FracBioFuel_tbl

  basedir <- getwd()

  # Normalize path to gcamdatabase
  if(!is.null(gcamdatabase)){gcamdatabase <- normalizePath(gcamdatabase)}

  if(any(paramsSelect %in% c('elec_lifetime_scurve_yr', 'elec_lifetime_yr',
                             'elec_fuel_co2_content_tonsperMBTU',
                             'elec_carbon_capture_rate_fraction',
                             'elec_carbon_capture_escl_rate_fraction'))){
    paramsSelect <- c('pop',paramsSelect)
  }

  if(!is.null(regionsSelect)){
    if(any(grepl("$all^", regionsSelect, ignore.case = T))){
      regionsSelect <- NULL
    }

    regionsSelect <- gsub("^United States$", "USA", regionsSelect, ignore.case=T)

    }

  if(!is.null(gcamdata_folder)){
    if(!dir.exists(gcamdata_folder)){
      rlang::warn(paste0("gcamdata_folder provided : ", gcamdata_folder, "does not exist."))
      rlang::warn(paste0("Skipped parameters that require gcamdata folder."))
    }
  }

#.....................
# Read in all raw gcamdata base and gcam folder data.
#.....................

  if(T){ # Read in all raw gcamdata base and gcam folder data.

#.....................
# Params and Queries
#.....................

  paramQueryMap <- (gcamextractor::map_param_query)%>%dplyr::select(group,param,query,gcamdata)

  # Check if queriesSelect is a querySet or one of the queries
  if(!any(c("all","All","ALL") %in% paramsSelect)){
  if(any(paramsSelect %in% unique(paramQueryMap$group))){
    queriesSelectx <- as.vector(unlist(unique((paramQueryMap%>%dplyr::filter(group %in% paramsSelect))$query)))
    #rlang::inform(paste("queriesSelect chosen include the following querySets: ",paste(paramsSelect,collapse=", "),".",sep=""))
    #rlang::inform(paste("Which include the following queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
    #rlang::inform(paste("Other queries not run include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
  }else{
    if(any(paramsSelect %in% as.vector(unique(paramQueryMap$param)))){
      queriesSelectx<- as.vector(unlist(unique((paramQueryMap%>%dplyr::filter(param %in% paramsSelect))$query)))
      #rlang::inform(paste("queriesSelect chosen include the following queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
     # rlang::inform(paste("Other queries not run include: ",paste(as.vector(unlist(querySets))[!as.vector(unlist(querySets)) %in% queriesSelectx],collapse=", "),".",sep=""))
    }else {
      queriesSelectx <-  NULL
      rlang::inform(paste("Params in queries.xml include: ",paste(as.vector(unlist(unique(paramQueryMap$param))),collapse=", "),".",sep=""))
      rlang::inform(paste("None of the chosen paramsSelect are available in gcamextractor params: ",paste(paramsSelect,collapse=", "),".",sep=""))
      stop("None of the params chosen are available.")
      }
  }}else{
    queriesSelectx <- as.vector(unlist(unique(paramQueryMap$query)))
  }

  queriesSelectx <- unique(queriesSelectx [!is.na(queriesSelectx )])

  # remove USA queries if not needed
  if(!is.null(regionsSelect) && !any(c("USA", gcamextractor::regions_US52) %in% regionsSelect)){
    queriesSelectx <- queriesSelectx[!grepl("USA", queriesSelectx)]
  }
  print("SELECTED QUERIES:")
  print(queriesSelectx)

#.............................
# Create necessary directories if they dont exist.
#............................

  if (!dir.exists(folder)){
    dir.create(folder)}  # Output Directory

#................
# Set file paths
#................

  if(is.null(gcamdatabase)){
    gcamdatabasePath = NULL
    gcamdatabaseName = NULL
  }else{
    if(is.character(gcamdatabase)){
      if(dir.exists(gcamdatabase)){
        gcamdatabasePath <- dirname(gcamdatabase); gcamdatabasePath
        gcamdatabaseName <- basename(gcamdatabase); gcamdatabaseName
        rlang::inform(paste("Connecting to GCAM database provided ",gcamdatabase,"...",sep=""))
      }else{stop(paste("The GCAM database path provided does not exist: ", gcamdatabase, sep=""))}
    }else{
      rlang::inform(paste("gcamdatabase provided is not a character string to the GCAM database path. Please check your entry."))
    }
  }

  if(is.null(queryFile)){
    XML::saveXML(gcamextractor::queries_xml, file=paste0(folder,"/queries.xml"))
    queryFile <- paste0(folder,"/queries.xml")
    xfun::gsub_file(queryFile,"&apos;","'")
    queryPath <- gsub("[^/]+$","",queryFile)
    queryxml <- basename(queryFile)
  }else{
    if(is.character(queryFile)){
      if(file.exists(queryFile)){
        queryPath <- gsub("[^/]+$","",queryFile)
        queryxml <- basename(queryFile)
        rlang::inform(paste("Connecting to the queryFile provided ",queryFile,"...",sep=""))
      }else{rlang::inform(paste("The queryFile path provided dos not exist: ", queryFile, sep=""))}
    }else{
      rlang::inform(paste("The queryFile path provided is not a character string to the query file. Please check your entry."))
    }
  }

  if(is.null(dataProjFile)){
    dataProj = "dataProj"
    dataProjPath = gsub("//","/",paste(folder,"/", sep = ""))
  }else{
    if(is.list(dataProjFile)){
      dataProjPath <- gsub("//","/",paste(folder,"/", sep = ""))
      dataProj <- paste("dataProj", sep = "")
    }else{
    if(is.character(dataProjFile)){
      if(grepl("/",dataProjFile)){
        if(file.exists(dataProjFile)){
        dataProjPath <- gsub("[^/]+$","",dataProjFile)
        dataProj <- basename(dataProjFile)
        rlang::inform(paste("Connecting to the dataProjFile provided ",dataProjFile,"...",sep=""))}else{
          dataProjPath <- gsub("[^/]+$","",dataProjFile)
          dataProj <- basename(dataProjFile)
          rlang::inform(paste0("Creating folder for dataProjFile: ", dataProjFile))
          if(!dir.exists(dataProjPath)){dir.create(dataProjPath)}
          rlang::inform(paste0(gsub("//","/",paste("Will save GCAM data to ",dataProjPath,"/",dataProjFile,"...",sep=""))))
        }
      }else{
        dataProjPath <- gsub("//","/",paste(folder,"/", sep = ""))
        dataProj <- dataProjFile
        rlang::inform(paste("Will save data to: ", dataProjPath,"/",dataProjFile, sep=""))
      }
    }else{
      rlang::inform(paste("The dataProjFile path provided is not a character string to the query file. Please check your entry."))
    }
    }
  }

  # Set new scenario names if provided
  if (is.null(scenOrigNames)) {
    scenNewNames <- NULL
    } else {
    if(any(c("all","All","ALL") %in% scenOrigNames)){
      scenNewNames <- NULL
      }
    }

#.............................................
# Read gcam database or existing dataProj.proj
#............................................

  # In case user sets reReadData=F and provides a .proj file instead of a gcamdatabase
  if((is.null(gcamdatabasePath) | is.null(gcamdatabaseName)) &
     reReadData==T){
    if(is.list(dataProjFile)){
      reReadData=F
      }
    if(file.exists(paste(dataProjPath,"/",dataProj,sep=""))){
    reReadData=F
    }
  }

  if(!all(is.na(queriesSelectx))){
  if (!reReadData) {
 # Check for proj file path and folder if incorrect give error
    if(!is.list(dataProjFile)){
    if(!file.exists(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))){
      stop(gsub("//","/",paste("dataProj file: ", dataProjPath,"/",dataProj," is incorrect or doesn't exist.",sep="")))}
    }

  # Checking if dataProjFile is preloaded xml gcamextractor::xmlMetiQueries
  if(is.list(dataProjFile)){
    dataProjLoaded <- rgcam::loadProject(dataProjFile)
    }else{
   if (file.exists(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))) {
      dataProjLoaded <- rgcam::loadProject(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))
   } else {
     stop(paste("No ", dataProj, " file exists. Please set reReadData=T to create dataProj.proj"))
   }}

      scenarios <- rgcam::listScenarios(dataProjLoaded); scenarios  # List of Scenarios in GCAM database
      queries <- rgcam::listQueries(dataProjLoaded); queries  # List of queries in GCAM database

      # Select Scenarios
      if(is.null(scenOrigNames)){
        scenOrigNames <- scenarios[1]
        rlang::inform(paste("scenOrigNames set to NULL so using only first scenario: ",scenarios[1],sep=""))
        rlang::inform(paste("from all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
        rlang::inform(paste0("To run all scenarios please set scenOrigNames to 'All' or"))
        rlang::inform(paste("you can choose a subset of scenarios by setting the scenOrigNames input (eg. scenOrigNames = c('scen1','scen2'))" ,sep=""))
      } else {
        if(any(c("all","All","ALL") %in% scenOrigNames)){
          scenOrigNames <- scenarios
          rlang::inform(paste("scenOrigNames set to 'All' (Default) so using all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
          rlang::inform(paste("You can choose a subset of scenarios by setting the scenOrigNames input (eg. scenOrigNames = c('scen1','scen2'))" ,sep=""))
        } else {
          if(any(scenOrigNames %in% scenarios)){
            rlang::inform(paste("scenOrigNames available in scenarios are :",paste(scenOrigNames[scenOrigNames %in% scenarios],collapse=", "),sep=""))
            if(length(scenOrigNames[!scenOrigNames %in% scenarios])>0){
              rlang::inform(paste("scenOrigNames not available in scenarios are :",paste(scenOrigNames[!scenOrigNames %in% scenarios],collapse=", "),sep=""))}
            if(length(scenarios[!scenarios %in% scenOrigNames])>0){
              rlang::inform(paste("Other scenarios not selected are :",paste(scenarios[!scenarios %in% scenOrigNames],collapse=", "),sep=""))}
          } else {
            rlang::inform(paste("None of the scenOrigNames : ",paste(scenOrigNames,collapse=", "),sep=""))
            rlang::inform(paste("are in the available scenarios : ",paste(scenarios,collapse=", "),sep=""))
          }
        }
      }

      scenarios <- scenOrigNames # Set scenarios to chosen scenarios

  } else {

  # Check for query file and folder if incorrect give error
    if(!file.exists(gsub("//","/",paste(queryPath, "/", queryxml, sep = "")))){stop(paste("query file: ", queryPath,"/",queryxml," is incorrect or doesn't exist.",sep=""))}
    if(file.exists(gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))){unlink(gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))}

    # Subset the query file if queriesSelect is not "All"
    if(!any(c("All","all","ALL") %in% paramsSelect)){

    xmlFilePath = gsub("//","/",paste(queryPath, "/", queryxml, sep = ""))
    xmlfile <- XML::xmlTreeParse(xmlFilePath)
    xmltop <- XML::xmlRoot(xmlfile)
    top <- XML::xmlNode(XML::xmlName(xmltop))

    # Subset regions in queries
    for(i in 1:length(xmltop)){
      for(j in 1:length(queriesSelectx)){
       if(queriesSelectx[j] == XML::xmlGetAttr(xmltop[[i]][[length(xmltop[[i]])]], "title")){
          # include only selected regions when applicable
          # (user has selected regions and original query uses "all-regions")
          if(!is.null(regionsSelect) &&
             !regionsSelect %in% c("All", "all", "ALL") &&
             XML::names.XMLNode(xmltop[[i]])[1] == "all-regions" &&
             !(queriesSelectx[j] == "CO2 emissions by sector" &&
               any(c("emissCO2CumGlobal2010to2100", "emissCO2CumGlobal2010to2100RCP") %in% paramsSelect)) &&
             !(queriesSelectx[j] == "prices by sector") &&
             !(queriesSelectx[j] == "elec operating costs by tech and vintage")
             ){
            # keep all regions for CO2 emissions by sector if cumulative global emissions are selected
            # remove the all-regions element
            to_add <- XML::removeChildren(xmltop[[i]],1)
            # add each region
            for(k in 1:length(regionsSelect)){
              to_add <- XML::addChildren(to_add,
                                         XML::xmlNode("region",
                                                      attrs = c(name = regionsSelect[k])))
            }
            # reorder children so that regions are at the top
            XML::xmlChildren(to_add) <- XML::xmlChildren(to_add)[c(2:(length(regionsSelect)+1),1)]
            # add modified node back into xml
            top <- XML::addChildren(top, to_add)
          }else{
            # else include all regions (don't change query from orig query file)
            top <- XML::addChildren(top, xmltop[[i]])
          }
        }
      }
    }

    XML::saveXML(top, file=gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))
    } else {
      rlang::inform(paste("paramsSelect includes 'All' so running all available queries: ",paste(queriesSelectx,collapse=", "),".",sep=""))
      file.copy(from=gsub("//","/",paste(queryPath, "/", queryxml, sep = "")),
                to=gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))
    }

    if(!file.exists(gsub("//","/",paste(queryPath, "/subSetQueries.xml", sep = "")))){
      stop(gsub("//","/",paste("query file: ", queryPath,"/subSetQueries.xml is incorrect or doesn't exist.",sep="")))}else{
        xfun::gsub_file(paste(queryPath, "/subSetQueries.xml", sep = ""),"&apos;","'")
        rlang::inform(paste0(gsub("//","/",paste("Reading queries from queryFile created: ", queryPath,"/subSetQueries.xml.",sep=""))))
      }

    # Check for gcamdatbasePath and gcamdatabasename
    if(!is.null(gcamdatabase)){
    if(is.null(gcamdatabasePath) | is.null(gcamdatabaseName)){
      stop(gsub("//","/",paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep="")))}
    if(!file.exists(gsub("//","/",paste(gcamdatabasePath, "/", gcamdatabaseName, sep = "")))){
      stop(gsub("//","/",paste("GCAM database: ", gcamdatabasePath,"/",gcamdatabaseName," is incorrect or doesn't exist.",sep="")))}
    }

    # Get names of scenarios in database
    # Change directory to avoid rgcam error when gcamdatabase is in the same folder as gcamextractor
    if(gcamdatabasePath == getwd()){
    rlang::inform("Switching folder because database is in same folder as current working directory which does not work with rgcam.")
    temp_folder <- paste0(getwd(),"/temp_dir")
    dir.create(temp_folder)
    setwd(temp_folder)
    x <- utils::capture.output(rgcam::localDBConn(gcamdatabasePath,gcamdatabaseName,maxMemory=maxMemory), type="message")
    # Reset dir
    setwd(basedir)
    unlink(temp_folder, force=T, recursive=T)
    rlang::inform("Back to original working directory.")} else {
      x <- utils::capture.output(rgcam::localDBConn(gcamdatabasePath,gcamdatabaseName,maxMemory=maxMemory), type="message")
    }
    x <- gsub(", ",",",gsub(": ","",gsub("Database scenarios:  ","",x)));x
    scenarios <- as.vector(unlist(strsplit(gsub("Database scenarios: ","",x),",")))
    rlang::inform(paste("All scenarios in data available: ", paste(scenarios,collapse=", "), sep=""))


    if(file.exists(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))){
      unlink(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))}  # Delete old project file

    # Select Scenarios
    if(is.null(scenOrigNames)){
      scenOrigNames <- scenarios[1]
      rlang::inform(paste("scenOrigNames set to NULL so using only first scenario: ",scenarios[1],sep=""))
      rlang::inform(paste("from all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
      rlang::inform(paste0("To run all scenarios please set scenOrigNames to 'All'"))
    } else {
        if(any(c("all","All","ALL") %in% scenOrigNames)){
          scenOrigNames <- scenarios
          rlang::inform(paste("scenOrigNames set to 'All' so using all available scenarios: ",paste(scenarios,collapse=", "),sep=""))
        } else {
          if(any(scenOrigNames %in% scenarios)){
            rlang::inform(paste("scenOrigNames available in scenarios are : ",paste(scenOrigNames[scenOrigNames %in% scenarios],collapse=", "),sep=""))
            if(length(scenOrigNames[!scenOrigNames %in% scenarios])>0){
            rlang::inform(paste("scenOrigNames not available in scenarios are :",paste(scenOrigNames[!scenOrigNames %in% scenarios],collapse=", "),sep=""))}
            if(length(scenarios[!scenarios %in% scenOrigNames])>0){
            rlang::inform(paste("Other scenarios not selected are :",paste(scenarios[!scenarios %in% scenOrigNames],collapse=", "),sep=""))}
          } else {
            rlang::inform(paste("None of the scenOrigNames : ",paste(scenOrigNames,collapse=", "),sep=""))
            rlang::inform(paste("are in the available scenarios : ",paste(scenarios,collapse=", "),sep=""))
            stop("Please check scenOrigNames and rerun.")
          }
        }
    }

    for (scenario_i in scenOrigNames) {

        # Fix paths
        projPath_i = suppressWarnings(normalizePath(paste0(dataProjPath, "/", dataProj))); projPath_i
        queryPath_i = normalizePath(paste0(queryPath, "/subSetQueries.xml")); queryPath_i
        if(gcamdatabasePath == getwd()){
        # Change directory to avoid rgcam error when gcamdatabase is in the same folder as gcamextractor
        rlang::inform("Switching folder because database is in same folder as current working directory which does not work with rgcam.")
        temp_folder <- paste0(getwd(),"/temp_dir")
        dir.create(temp_folder)
        setwd(temp_folder)
       dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn(gcamdatabasePath, gcamdatabaseName,maxMemory=maxMemory),
                                           proj = projPath_i,
                                           scenario = scenario_i,
                                           queryFile = queryPath_i)  # Check your queries file
       # Reset dir
       setwd(basedir)
       unlink(temp_folder, force=T, recursive=T)
       rlang::inform("Back to original working directory.")
        } else {
          dataProj.proj <- rgcam::addScenario(conn = rgcam::localDBConn(gcamdatabasePath, gcamdatabaseName,maxMemory=maxMemory),
                                              proj = projPath_i,
                                              scenario = scenario_i,
                                              queryFile = queryPath_i)  # Check your queries file
        }
      }
    dataProjLoaded <- rgcam::loadProject(gsub("//","/",paste(dataProjPath, "/", dataProj, sep = "")))

    # Save list of scenarios and queries
    scenarios <- rgcam::listScenarios(dataProjLoaded); scenarios  # List of Scenarios in GCAM database
    queries <- rgcam::listQueries(dataProjLoaded); queries  # List of queries in GCAM database
  }

  queries <- rgcam::listQueries(dataProjLoaded); queries  # List of Queries in queryxml
  } else {
    queries <- NA
  }

  # Set new scenario names if provided
  if (is.null(scenNewNames)) {
    scenNewNames <- scenOrigNames}else{
      scenNewNames <- scenNewNames[1:length(scenOrigNames)]
    }

  # Read in paramaters from query file to create formatted table
 queriesx <- queries

  if(!any(queriesSelectx %in% queries)){
    rlang::inform("No queries exist for the param selected in the queryxml file.")}

  paramsSelectAll <- as.vector(unlist(unique(paramQueryMap$param)))


  if(any(c("all","All","ALL") %in% paramsSelect)){
    paramsSelectx <- paramsSelectAll
    } else {
      if(any(paramsSelect %in% as.vector(unique(paramQueryMap$group)))){
        paramsSelectx <- unique((paramQueryMap%>%dplyr::filter(group %in% paramsSelect))$param)
      } else {paramsSelectx=paramsSelect}
    }

  # Read in relevant gcamdatafiles for all params requiring data
  if(!is.null(gcamdata_folder)){
    if(dir.exists(gcamdata_folder)){

      # Get list of relevant gcamdata files for selected params
      (paramQueryMap %>%
        dplyr::filter(gcamdata != "no",
                      param %in% paramsSelectx))$gcamdata %>%
        unlist() %>%
        unique() ->
        gcamdata_filenames; gcamdata_filenames

      # Read in each file needed and assign to list and rename the list item
      gcamdata_files <- list()
      if(!is.null(gcamdata_filenames)){
        count = 1
      for(i in 1:length(gcamdata_filenames)){
        if(!file.exists(paste0(gcamdata_folder, gsub(".csv","",gcamdata_filenames[[i]]), ".csv"))){
          params_remove <- (paramQueryMap %>% dplyr::filter(grepl(gcamdata_filenames[[i]], gcamdata)))$param
          rlang::warn(paste0("File: ", gcamdata_filenames[[i]],
                               " does not exist so skipping file and related param ",
                               (paramQueryMap %>% dplyr::filter(grepl(gcamdata_filenames[[i]], gcamdata)))$param,
                               "."))
          paramsSelectx <- paramsSelectx[!paramsSelectx %in% params_remove]
        } else {
        gcamdata_file_i <-  tibble::as_tibble(utils::read.csv(paste0(gcamdata_folder, gsub(".csv","",gcamdata_filenames[[i]]), ".csv"), comment.char = "#"))
        gcamdata_files[[count]] <- gcamdata_file_i
        names(gcamdata_files)[[count]] <- gcamdata_filenames[[i]]
        count = count + 1
        }
      }
      }
      names(gcamdata_files)
    }
  }

} # Close Read in all raw gcamdata base and gcam folder data.



#.....................
# Collect and format selected parameters
#.....................

  # Check if any of the selected parameters are available in the GCAM data
  if(any(paramsSelectx %in% paramsSelectAll)){

  datax <- tibble::tibble()

  if(T){

  queriesx <- queriesx[queriesx %in% queries]

  print(queriesx)

  # Electricity ------------------------------------------

  ## elecConsumByDemandSectorTWh ===============================================
  paramx <- "elecConsumByDemandSectorTWh"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "elec consumption by demand sector"
    if(queryx %in% queriesx){
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::mutate(input=gsub("elect\\_td\\_ind","industry",input),
                      input=gsub("elect\\_td\\_bld","building",input),
                      input=gsub("elect\\_td\\_trn","transportation",input))%>%
        dplyr::rename(aggSector = input) %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecConsumByDemandSectorTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value * gcamextractor::convert$conv_EJ_to_TWh,
                      units = "Electricity Consumption by Sector (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = aggSector,
                      classLabel1 = "Aggregated Sector",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "Sector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    }
  }


  ## elecByTechTWh =============================================================
  paramx <- "elecByTechTWh"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    tbl<-tibble::tibble()

    queryx <- "elec gen by gen tech and cooling tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>%
          dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      Units == "EJ")%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecByTechTWh",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value * gcamextractor::convert$conv_EJ_to_TWh,
                      units = "Electricity Generation by Fuel (TWh)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = subsector,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = technology,
                      classLabel2 = "Technology",
                      classPalette2 = "pal_all")
    }else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }

    if(nrow(tbl)>0){
      tbl <- tbl %>%
        dplyr::mutate(class1=dplyr::case_when(class1=="rooftop_pv"~"solar",
                                              TRUE~class1))%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::mutate(origQuery="origQuery") %>%
        dplyr::group_by(scenario, region, subRegion,    param, sources,class1,class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX) %>%
        dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblelecByTechTWh<-tbl}
  }

  ## elecPrices ================================================================
  paramx <- "elecPricesBySector"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    tbl<-tibble::tibble()

    queryx <- "elec prices by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>%
          dplyr::filter(region %in% regionsSelect)
      }

      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames),
                         by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "elecPricesBySector",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year,
                      subRegion=region,
                      scenario = scenNewNames,
                      # convert from 1975 USD to 2015 USD
                      value = (value*gcamextractor::gdp_deflator(2015,1975)),
                      units = "Electricity Price (2015 USD)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_all",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "pal_all") %>%
        dplyr::select(scenario, region, subRegion, param, sources, vintage, class1, class2, x, xLabel, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX) %>%
        dplyr::group_by(scenario, region, subRegion, param, sources, vintage, class1, class2, x, xLabel, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX) %>%
        dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(value))

      datax <- dplyr::bind_rows(datax, tbl)
    }
  }


  # Energy ---------------------------------------------------------------------

  ## Summary ===================================================================

  ### energyFinalConsumBySecEJ #################################################
  paramx<-"energyFinalConsumBySecEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "total final energy by aggregate sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalConsumBySecEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Final Energy by Sector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = dplyr::case_when(
                        grepl("building|comm|resid",sector)~"building",
                        grepl("industr|alumin|iron|agri|fert|mining|construc|chem|cement",sector)~"industry",
                        grepl("transport",sector)~"transport",
                        grepl("municipal|water", sector)~"other",
                        T ~ sector),
                      classLabel1 = "Sector",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "Subsector",
                      classPalette2 = "pal_all")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))



      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### energyFinalConsumBySecEJNoFeedstock ######################################
  paramx<-"energyFinalConsumBySecEJNoFeedstock"
  # Total final energy by aggregate end-use sector not including industrial feedstocks
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "total final energy by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      !grepl("feedstock", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalConsumBySecEJNoFeedstock",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Final Energy by Sector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = dplyr::case_when(
                        grepl("building|comm|resid",sector)~"building",
                        grepl("industr|alumin|iron|agri|fert|mining|construc|chem|cement",sector)~"industry",
                        grepl("transport|trn",sector)~"transport",
                        grepl("water|municipal", sector)~"other",
                        T ~ sector),
                      classLabel1 = "Sector",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "Subsector",
                      classPalette2 = "pal_all")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    }
  }



  ### energyFinalByFuelEJ ######################################################
  paramx<-"energyFinalByFuelEJ"
  # Total final energy by fuel
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "final energy consumption by sector and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(!grepl("trn",input))%>%
        dplyr::mutate(input=dplyr::if_else(input=="global solar resource","solar",input),
                      sector = dplyr::case_when(grepl("fert|ag|cement|chem|construc|indus|iron|mining", sector) ~ "industry",
                                                grepl("resid|comm", sector) ~ "buildings",
                                                grepl("trn", sector) ~ "transportation",
                                                T ~ sector),
                      param = "energyFinalByFuelEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "Sector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### energyFinalByFuelEJNoFeedstock ###########################################
  paramx<-"energyFinalByFuelEJNoFeedstock"
  # Total final energy by fuel (without industrial feedstocks)
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "final energy consumption by sector and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(!grepl("trn",input),
                      !grepl("feedstock", sector))%>%
        dplyr::mutate(input=dplyr::if_else(input=="global solar resource","solar",input),
                      sector = dplyr::case_when(grepl("fert|ag|cement|chem|construc|indus|iron|mining", sector) ~ "industry",
                                                grepl("resid|comm", sector) ~ "buildings",
                                                grepl("trn", sector) ~ "transportation",
                                                T ~ sector),
                      param = "energyFinalByFuelEJNoFeedstock",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = input,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "Sector",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ## Buildings =================================================================

  ### energyFinalSubsecBySectorBuildEJ #########################################
  paramx<-"energyFinalSubsecBySectorBuildEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "building final energy by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "energyFinalSubsecBySectorBuildEJ",
                      sector = dplyr::case_when(grepl("comm", sector) & grepl("light", sector) ~ "commercial lighting",
                                                grepl("comm", sector) & grepl("hvac", sector) ~ "commercial ACMV",
                                                grepl("comm", sector) & grepl("cook", sector) ~ "commercial cooking",
                                                grepl("comm", sector) & grepl("other", sector) ~ "commercial other",
                                                grepl("resid", sector) & grepl("light", sector) ~ "residential lighting",
                                                grepl("resid", sector) & grepl("cook", sector) ~ "residential cooking",
                                                grepl("resid", sector) & grepl("air con|vent", sector) ~ "residential ACMV",
                                                grepl("resid", sector) & grepl("other|refrig|telev|water", sector) ~ "residential other",
                                                T ~ sector),
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Building Final Energy By Subsector (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = sector,
                      classLabel1 = "Sector",
                      classPalette1 = "pal_all",
                      class2 = subsector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  ### energyFinalSubsecByFuelBuildEJ ###########################################
  paramx<-"energyFinalSubsecByFuelBuildEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "building final energy by fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::rename(fuel = input) %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(fuel=gsub("elect_td_bld","electricity",fuel),
                      fuel=gsub("delivered gas","gas",fuel),
                      fuel=gsub("delivered biomass","biomass",fuel),
                      fuel=gsub("delivered coal","coal",fuel),
                      fuel=gsub("refined liquids enduse","liquids",fuel),
                      fuel=gsub("global solar resource", "solar", fuel),
                      param = "energyFinalSubsecByFuelBuildEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Building Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = fuel,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}



  ## Industry ==================================================================

  ### energyFinalSubsecByFuelIndusEJ ###########################################
  paramx<-"energyFinalSubsecByFuelIndusEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "industry final energy by tech and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(fuel=input) %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(fuel=gsub("elect_td_ind","electricity",fuel),
                      fuel=gsub("wholesale gas","gas",fuel),
                      fuel=gsub("delivered biomass","biomass",fuel),
                      fuel=gsub("delivered coal","coal",fuel),
                      fuel=gsub("refined liquids industrial","liquids",fuel),
                      fuel=dplyr::case_when(grepl("H2", fuel) ~ "hydrogen",
                                            T ~ fuel),
                      param = "energyFinalSubsecByFuelIndusEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Industry Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = fuel,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}

  ### energyFinalSubsecByFuelAndCCSIndusEJ #####################################
  paramx<-"energyFinalSubsecByFuelAndCCSIndusEJ"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "industry final energy by tech and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(fuel=input) %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(fuel=gsub("elect_td_ind","electricity",fuel),
                      fuel=gsub("wholesale gas","gas",fuel),
                      fuel=gsub("delivered biomass","bioenergy",fuel),
                      fuel=gsub("delivered coal","coal",fuel),
                      fuel=gsub("refined liquids industrial","liquids",fuel),
                      fuel=dplyr::case_when(grepl("H2", fuel) ~ "hydrogen", T ~ fuel),
                      fuel = dplyr::case_when(grepl("CCS", technology) & !grepl("elec", fuel) ~ paste0(fuel, " CCS"),
                                                T ~ fuel),
                      param = "energyFinalSubsecByFuelAndCCSIndusEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Industry Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = fuel,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}



  ## Transportation ============================================================

  ### energyFinalByFuelTransportPassEJ #########################################
  paramx<-"energyFinalByFuelTransportPassEJ"
  # Transport final energy by fuel
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport final energy by mode and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(fuel=input) %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      grepl("pass", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(fuel=gsub("elect_td_trn","electricity",fuel),
                      fuel=gsub("wholesale gas","gas",fuel),
                      fuel=gsub("delivered gas", "gas", fuel),
                      fuel=gsub("delivered biomass","biomass",fuel),
                      fuel=gsub("delivered coal","coal",fuel),
                      fuel=gsub("refined liquids enduse","liquids",fuel),
                      fuel=dplyr::case_when(grepl("H2", fuel) ~ "hydrogen",
                                            T ~ fuel),
                      param = "energyFinalByFuelTransportPassEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger Transport Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = fuel,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### energyFinalByFuelTransportFreightEJ #########################################
  paramx<-"energyFinalByFuelTransportFreightEJ"
  # Transport final energy by fuel
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport final energy by mode and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::rename(fuel=input) %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      grepl("freight", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(fuel=gsub("elect_td_trn","electricity",fuel),
                      fuel=gsub("wholesale gas","gas",fuel),
                      fuel=gsub("delivered gas", "gas", fuel),
                      fuel=gsub("delivered biomass","biomass",fuel),
                      fuel=gsub("delivered coal","coal",fuel),
                      fuel=gsub("refined liquids enduse","liquids",fuel),
                      fuel=dplyr::case_when(grepl("H2", fuel) ~ "hydrogen",
                                            T ~ fuel),
                      param = "energyFinalByFuelTransportFreightEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger Transport Final Energy by Fuel (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = fuel,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = fuel,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### energyFinalByTechTransportPassEJ #########################################
  paramx<-"energyFinalByTechTransportPassEJ"
  # Transport final energy by tech
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport final energy by tech and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      grepl("pass", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(tech = dplyr::case_when(grepl("Aviation", mode) ~  "airplane",
                                              mode == "Bus" & technology == "BEV" ~ "bus (BEV)",
                                              mode == "Bus" & technology == "FCEV" ~ "bus (FCEV)",
                                              mode == "Bus" & grepl("Hybrid", technology) ~ "bus (hybrid)",
                                              mode == "Bus" ~ "bus (other)",
                                              grepl("LDV", sector) & technology == "BEV" ~ "LDV (BEV)",
                                              grepl("LDV", sector) & technology == "FCEV" ~ "LDV (FCEV)",
                                              grepl("LDV", sector) & grepl("Hybrid", technology) ~ "LDV (hybrid)",
                                              grepl("LDV", sector) ~ "LDV (other)",
                                              mode == "HSR" ~ "Rail (HSR)",
                                              mode == "Passenger Rail" & technology == "Electric" ~ "Rail (electric)",
                                              mode == "Passenger Rail" ~ "Rail (other)",
                                              T ~ paste0(mode, "_", technology)
                                              ),
                      param = "energyFinalByTechTransportPassEJ",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger Transport Final Energy by Tech (EJ)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = tech,
                      classLabel1 = "Tech",
                      classPalette1 = "pal_all",
                      class2 = tech,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### energyFinalByTechTransportFreightEJ #########################################
  paramx<-"energyFinalByTechTransportFreightEJ"
  # Transport final energy by tech
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport final energy by tech and fuel"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      grepl("freight", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(tech = dplyr::case_when(grepl("road", sector) & technology == "BEV" ~ "truck (BEV)",
                                              grepl("road", sector) & technology == "FCEV" ~ "truck (FCEV)",
                                              grepl("road", sector) & grepl("Hybrid", technology) ~ "truck (hybrid)",
                                              grepl("road", sector) ~ "truck (other)",
                                              mode == "Domestic Ship" ~ "Shipping",
                                              mode == "Freight Rail" ~ "Rail",
                                              T ~ paste0(mode, "_", technology)),
        param = "energyFinalByTechTransportFreightEJ",
        sources = "Sources",
        origScen = scenario,
        origQuery = queryx,
        origValue = value,
        origUnits = Units,
        origX = year, subRegion=region,
        scenario = scenNewNames,
        units = "Passenger Freight Final Energy by Tech (EJ)",
        vintage = paste("Vint_", year, sep = ""),
        x = year,
        xLabel = "Year",
        aggregate = "sum",
        class1 = tech,
        classLabel1 = "Tech",
        classPalette1 = "pal_all",
        class2 = tech,
        classLabel2 = "classLabel2",
        classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%
        dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  # Emissions ------------------------------------------------------------------

  ## Summary ===================================================================

  ### emissLUC #################################################################
  # need LUC for CO2BySector, GHGBYSector, and GHGByGas
  if(any(c("emissCO2BySector", "emissCO2BySectorNoBio", "emissGHGBySectorGWPAR5", "emissGHGBySectorNoBioGWPAR5", "emissGHGByGasGWPAR5", "emissGHGByGasNoBioGWPAR5", "emissLUC") %in% paramsSelectx)){

    queryx <- "Land Use Change Emission (future)"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::left_join(gcamextractor::conv_GgTg_to_MTC,by="Units") %>%
        dplyr::mutate(origValue=value,value=value*Convert*gcamextractor::convert$conv_C_CO2, # convert from MTC to MTCO2eq
                      origUnits=Units,units="Emissions LUC - (MTCO2)")%>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      origValue = origValue,
                      units = "LUC CO2 Emissions (MTCO2eq)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classPalette1 = "pal_all",
                      classPalette2 = "classPalette2",
                      ghg = "CO2",
                      sector = "LUC") %>%
        dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                      aggregate, classPalette1, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX, ghg, sector) %>%
        dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                        aggregate, classPalette1, classPalette2,
                        origScen, origQuery, origUnits, origX, ghg, sector) %>%
        dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup() %>%
        dplyr::filter(!is.na(value))
      tblLUEmiss<-tbl
    }
  } # end CO2 LUC query

    # add LUC to data if emissLUC param is chosen
    if("emissLUC" %in% paramsSelect){
      tblEmissLUC <- tblLUEmiss %>%
        dplyr::rename(class1 = sector, class2 = ghg) %>%
        dplyr::mutate(classLabel1 = "sector",
                      classLabel2 = "ghg",
                      param = "emissLUC")
      datax <- dplyr::bind_rows(datax, tblEmissLUC)
    }

    ### emissCO2BySector #######################################################
    # need CO2 for CO2BySector, GHGBySector, and GHGByGas
    if(any(c("emissCO2BySector", "emissGHGBySectorGWPAR5", "emissGHGByGasGWPAR5", "emissGHGBySectorPowerGWPAR5") %in% paramsSelectx)){
      queryx <- "CO2 emissions by sector"
      if (queryx %in% queriesx) {
        tbl <- rgcam::getQuery(dataProjLoaded, queryx)
        if (!is.null(regionsSelect)) {
          tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        tbl <- tbl %>%
          dplyr::filter(scenario %in% scenOrigNames)%>%
          dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          dplyr::mutate(
            origValue=value,
            value=value*44/12, # convert from MTC to MTCO2eq
            origUnits=Units,
            units="CO2 emissions - (MTCO2)",
            sources = "Sources",
            origScen = scenario,
            origQuery = queryx,
            origX = year, subRegion=region,
            scenario = scenNewNames,
            vintage = paste("Vint_", year, sep = ""),
            x = year,
            xLabel = "Year",
            aggregate = "sum",
            classPalette1 = "pal_all",
            classPalette2 = "pal_all",
            ghg = "CO2") %>%
          dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                        aggregate, ghg, sector, classPalette1, classPalette2,
                        origScen, origQuery, origValue, origUnits, origX)
        tblCO2Emiss <- tbl
      }
    }

    # Aggregate sectors
    if(any(c("emissCO2BySector", "emissGHGBySectorGWPAR5", "emissGHGByGasGWPAR5") %in% paramsSelectx)){
        #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
        tblCO2EmissAgg <- tblCO2Emiss %>%
          dplyr::mutate(
            sector=dplyr::case_when(
              grepl("refining",sector,ignore.case=T)~"refining",
              grepl("regional biomass|regional biomassOil|regional corn for ethanol|regional sugar for ethanol|biomass", sector,ignore.case=T)~"biomass",
              grepl("trn_aviation_intl", sector)~"International Aviation",
              grepl("trn_shipping_intl", sector) ~ "International Shipping",
              grepl("trn_",sector,ignore.case=T)~"transport",
              grepl("comm |resid ",sector,ignore.case=T)~"building",
              grepl("electricity|elec_|electricity |csp_backup",sector,ignore.case=T)~"electricity",
              grepl("H2",sector,ignore.case=T)~"hydrogen",
              grepl("cement|N fertilizer|industrial|ind ",sector,ignore.case=T)~"industry",
              grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",sector,ignore.case=T)~"industry",
              grepl("Beef|Dairy|Pork|Poultry",sector,ignore.case=T)~"livestock",
              grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|RootTuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",sector,ignore.case=T)~"crops",
              TRUE~sector)) %>%
          dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                          aggregate, ghg, sector, classPalette1, classPalette2,
                          origScen, origQuery, origUnits, origX) %>%
          dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T))) %>% dplyr::ungroup()%>%
          dplyr::filter(!is.na(value))
  }

  ### emissCO2BySectorNoBio ####################################################
  # need for CO2BySectorNoBio, GHGBySectorNoBio, and GHGByGasNoBio
  if(any(c("emissCO2BySectorNoBio", "emissGHGBySectorNoBioGWPAR5", "emissGHGByGasNoBioGWPAR5",
           "emissGHGBySectorBuildingsGWPAR5") %in% paramsSelectx)){

    queryx <- c("CO2 emissions by tech", "energy consumption by tech",
                "outputs by tech", "prices of all markets",
                "CO2 sequestration by tech", "CO2 emissions by sector (no bio)")
    if(all(queryx %in% queriesx)){
    # if exogenous no bio accounting is needed, get the necessary queries and
    # do the accounting
    if(exogenousNoBio){
        # get all the queries needed
        emiss.by.tech <- rgcam::getQuery(dataProjLoaded, "CO2 emissions by tech")
        energy.consump.by.tech <- rgcam::getQuery(dataProjLoaded, "energy consumption by tech")
        output.by.tech <- rgcam::getQuery(dataProjLoaded, "outputs by tech")
        market.prices <- rgcam::getQuery(dataProjLoaded, "prices of all markets")
        co2.sequestration <- rgcam::getQuery(dataProjLoaded, "CO2 sequestration by tech")

        # run the no bio accounting
        co2_by_sector_noBio <-
          allocate_biomass_emiss_reductions(energy.consump.by.tech,
                                            output.by.tech,
                                            market.prices,
                                            emiss.by.tech,
                                            c("regional biomass", "regional biomassOil",
                                              "regional sugar for ethanol"))


      # filter CO2 sequestration for biomass techs and aggregate to sector level
      co2_sequestration_bio_sector <- co2.sequestration %>%
        #dplyr::select(-Units) %>%
        dplyr::filter(grepl("bio|cellulosic", technology)) %>%
        dplyr::group_by(scenario, region, year, sector, Units) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup()

      # add biomass CO2 sequestration (BECCS)
      co2_by_sector_noBio_plus_seq <-
        co2_by_sector_noBio %>%
        dplyr::mutate(year = as.numeric(year)) %>%
        dplyr::full_join(co2_sequestration_bio_sector) %>%
        dplyr::group_by(scenario, region, year, sector) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        # remove elec_biomass since it should cancel out to 0 (may be small rounding errors)
        dplyr::filter(!grepl("elec_biomass", sector))

      # add biomass CO2 sequestration as negative emissions (multiple BECCS categories)
      co2_by_sector_final <-
        co2_sequestration_bio_sector %>%
        dplyr::mutate(value = -value,
                      sector = dplyr::case_when(grepl("elec_biomass", sector) & value < 0 ~ "BECCS_elec",
                                                grepl("refin", sector) & value < 0 ~ "BECCS_refining",
                                                grepl("H2", sector) & value < 0 ~ "BECCS_H2",
                                                grepl("alumina", sector) & value < 0 ~ "BECCS_alumina",
                                                T ~ sector)) %>%
        dplyr::full_join(co2_by_sector_noBio_plus_seq)


      tbl <- co2_by_sector_final

    } # end using exogenous no bio accounting

    # if exogenous no bio accounting is not needed, just use the no bio query
    else{
      queryx <- "CO2 emissions by sector (no bio)"
        tbl <- rgcam::getQuery(dataProjLoaded, queryx) # Tibble
    } # end using no bio query

      # filter to selected region
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }

      #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(
          origValue=value,
          value=value*44/12, # convert from MTC to MTCO2eq
          origUnits=Units,
          units="CO2 emissions - (MTCO2)",
          sources = "Sources",
          origScen = scenario,
          origQuery = dplyr::case_when(exogenousNoBio ~ "CO2 emissions by tech",
                                       T ~ "CO2 emissions by sector (no bio)"),
          origX = year, subRegion=region,
          scenario = scenNewNames,
          vintage = paste("Vint_", year, sep = ""),
          x = year,
          xLabel = "Year",
          aggregate = "sum",
          classPalette1 = "pal_all",
          classPalette2 = "pal_all",
          ghg = "CO2") %>%
        dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)
      tblCO2EmissNoBio <- tbl

    }

  } # end CO2 by sector (no bio) query


  # Aggregate sectors
  if(any(c("emissCO2BySectorNoBio", "emissGHGBySectorNoBioGWPAR5", "emissGHGByGasNoBioGWPAR5") %in% paramsSelectx)){
      tblCO2EmissNoBioAgg <- tblCO2EmissNoBio %>%
        dplyr::mutate(
          sector=dplyr::case_when(
            grepl("BECCS", sector) ~ "BECCS",
            #grepl("refining",sector,ignore.case=T)~"refining",
            grepl("regional biomass|regional biomassOil|regional corn for ethanol|regional sugar for ethanol", sector,ignore.case=T)~"biomass",
            #grepl("trn_aviation_intl", sector)~"International Aviation",
            #grepl("trn_shipping_intl", sector) ~ "International Shipping",
            grepl("trn_",sector,ignore.case=T)~"Transportation",
            grepl("comm |resid ",sector,ignore.case=T)~"Buildings",
            grepl("electricity|elec_|electricity |csp_backup",sector,ignore.case=T)~"Electricity",
            grepl("H2",sector,ignore.case=T)~"Industry",
            grepl("cement|N fertilizer|industrial|ind|alumin|refin|iron|chemical|construction|mining|agri|desal",sector,ignore.case=T)~"Industry",
            grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids|wholesale gas|delivered gas|delivered biomass",sector,ignore.case=T)~"Industry",
            grepl("Beef|Dairy|Pork|Poultry",sector,ignore.case=T)~"livestock",
            grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|RootTuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",sector,ignore.case=T)~"crops",
            TRUE~sector)) %>%
        dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                        aggregate, ghg, sector, classPalette1, classPalette2,
                        origScen, origQuery, origUnits, origX) %>%
        dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T))) %>% dplyr::ungroup() %>%
        dplyr::filter(!is.na(value))
    }


    # add CO2 by sector to data if emissCO2BySector param is chosen
    if("emissCO2BySector" %in% paramsSelectx){
      # LUC
      tblEmissCO2BySector_LU <- tblLUEmiss %>%
        dplyr::rename(class1 = sector, class2 = ghg) %>%
        dplyr::mutate(classLabel1 = "sector",
                        classLabel2 = "ghg")

      # other sectors
      tblEmissCO2BySector_CO2 <- tblCO2EmissAgg %>%
        dplyr::rename(class1 = sector, class2 = ghg) %>%
        dplyr::mutate(classLabel1 = "sector",
                      classLabel2 = "ghg")

    # other sectors
    tblEmissCO2BySector_CO2 <- tblCO2Emiss %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")
    # all CO2
    tblEmissCO2BySector <- dplyr::bind_rows(tblEmissCO2BySector_LU,
                                            #tblEmissCO2BySector_seq,
                                            tblEmissCO2BySector_CO2) %>%
      dplyr::mutate(param = "emissCO2BySector",
             units="CO2 emissions - (MTCO2)")
    datax <- dplyr::bind_rows(datax, tblEmissCO2BySector)
  }

  # add CO2 by sector (no bio) to data if emissCO2BySectorNoBio param is chosen
  if("emissCO2BySectorNoBio" %in% paramsSelectx){
    # LUC
    tblEmissCO2BySector_LU <- tblLUEmiss %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # other sectors
    tblEmissCO2BySectorNoBio_CO2 <- tblCO2EmissNoBioAgg %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")
    # all CO2
    tblEmissCO2BySectorNoBio <- dplyr::bind_rows(tblEmissCO2BySector_LU,
                                                 #tblEmissCO2BySector_seq,
                                                 tblEmissCO2BySectorNoBio_CO2) %>%
      dplyr::mutate(param = "emissCO2BySectorNoBio",
                    units="CO2 emissions - (MTCO2)")
    datax <- dplyr::bind_rows(datax, tblEmissCO2BySectorNoBio)
  }


  ### emissGHGBySectorGWPAR5 ###################################################
  # need nonCO2 for GHGBySector and GHGByGas
  if(any(c("emissGHGBySectorGWPAR5", "emissGHGBySectorNoBioGWPAR5", "emissGHGByGasGWPAR5",
           "emissGHGByGasNoBioGWPAR5", "emissGHGBySectorBuildingsGWPAR5", "emissGHGBySectorPowerGWPAR5") %in% paramsSelectx)){
      queryx <- "nonCO2 emissions by sector"
      if (queryx %in% queriesx) {
        tbl <- rgcam::getQuery(dataProjLoaded, queryx)
        if (!is.null(regionsSelect)) {
          tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
        }
        #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
        tbl <- tbl %>%
          dplyr::filter(scenario %in% scenOrigNames)%>%
          dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
          dplyr::mutate(ghg = dplyr::case_when ((grepl("HFC", ghg)) ~ "HFCs",
                                                   (grepl("SF6", ghg)) ~ "SF6",
                                                   (grepl("CO2", ghg)) ~ "CO2",
                                                   (grepl("N2O", ghg)) ~ "N2O",
                                                   (grepl("CH4", ghg)) ~ "CH4",
                                                   (grepl("SO2", ghg)) ~ "SO2",
                                                   (grepl("NH3", ghg)) ~ "NH3",
                                                   (grepl("CF4", ghg)) ~ "CF4",
                                                   (grepl("C2F6", ghg)) ~ "C2F6",
                                                   TRUE ~ "Other"))%>%
          dplyr::filter(!ghg %in% c('SO2', 'NH3', 'Other')) %>%
          #dplyr::left_join(emiss_sector_mapping, by=c('class2' = 'class1')) %>%
          #dplyr::mutate(class2=agg_sector) %>%
          #dplyr::select(-agg_sector) %>%
          dplyr::left_join(gcamextractor::GWP,by="ghg") %>%
          dplyr::left_join(gcamextractor::conv_GgTg_to_MTC,by="Units") %>%
          dplyr::filter(ghg!='CO2') %>%
          dplyr::mutate(origValue=value,
                        value=value*GWPAR5*Convert,
                        origUnits=Units,
                        origUnits = dplyr::case_when(ghg=="Other"~"Units",TRUE~origUnits),
                        units="GHG Emissions GWPAR5 (MTCO2eq)",
                        sources = "Sources",
                        origScen = scenario,
                        origQuery = queryx,
                        origX = year, subRegion=region,
                        scenario = scenNewNames,
                        vintage = paste("Vint_", year, sep = ""),
                        x = year,
                        xLabel = "Year",
                        aggregate = "sum",
                        classPalette2 = "pal_all",
                        classPalette1 = "pal_all") %>%
          dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                        aggregate, ghg, sector, classPalette1, classPalette2,
                        origScen, origQuery, origValue, origUnits, origX)

        tblNonCO2Emiss <- tbl
      } else {
        print("NonCO2 WAS NOT IN QUERIES")
        tblNonCO2Emiss <- tibble::tibble()
        # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
      }}

      # Aggregate Sectors
      if(any(c("emissGHGBySectorGWPAR5", "emissGHGBySectorNoBioGWPAR5", "emissGHGByGasGWPAR5", "emissGHGByGasNoBioGWPAR5") %in% paramsSelectx)){
          #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
        print(str(tblNonCO2Emiss))
        tblNonCO2EmissAgg <- tblNonCO2Emiss %>%
            dplyr::mutate(
              sector=dplyr::case_when(
                grepl("refining",sector,ignore.case=T)~"refining",
                grepl("regional biomass|regional biomassOil|regional corn for ethanol|biomass" ,sector,ignore.case=T)~"biomass",
                grepl("trn_",sector,ignore.case=T)~"transport",
                grepl("comm |resid ",sector,ignore.case=T)~"building",
                grepl("electricity|elec_|electricity |csp_backup",sector,ignore.case=T)~"electricity",
                grepl("H2",sector,ignore.case=T)~"hydrogen",
                grepl("cement|N fertilizer|industrial|ind ",sector,ignore.case=T)~"industry",
                grepl("gas pipeline|gas processing|unconventional oil production|gas to liquids",sector,ignore.case=T)~"industry",
                grepl("Beef|Dairy|Pork|Poultry",sector,ignore.case=T)~"livestock",
                grepl("FiberCrop|MiscCrop|OilCrop|OtherGrain|PalmFruit|Corn|Rice|Root_Tuber|RootTuber|SheepGoat|SugarCrop|UnmanagedLand|Wheat|FodderGrass|FodderHerb",sector,ignore.case=T)~"crops",
                TRUE~sector)) %>%
            dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                            aggregate, ghg, sector, classPalette1, classPalette2,
                            origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
            dplyr::filter(!is.na(value))
        }


  ### emissGHGByResProdGWPAR5 #################################################
  # need nonCO2 res prod for GHGBySector and GHGByGas
  if(any(c("emissGHGBySectorGWPAR5", "emissGHGBySectorNoBioGWPAR5", "emissGHGByGasGWPAR5",
           "emissGHGByGasNoBioGWPAR5", "emissGHGByResProdGWPAR5") %in% paramsSelectx)){
    #rlang::inform(paste0("Running param: ", paramx,"..."))
    # GHG emissions by resource production, using AR5 GWP values
    queryx <- "nonCO2 emissions by resource production"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(sector = resource) %>%
        dplyr::mutate(ghg = dplyr::case_when ((grepl("HFC", ghg)) ~ "HFCs",
                                                 (grepl("SF6", ghg)) ~ "SF6",
                                                 (grepl("CO2_FUG", ghg)) ~ "CO2_FUG",
                                                 (grepl("CO2", ghg)) ~ "CO2",
                                                 (grepl("N2O", ghg)) ~ "N2O",
                                                 (grepl("CH4", ghg)) ~ "CH4",
                                                 (grepl("SO2", ghg)) ~ "SO2",
                                                 (grepl("NH3", ghg)) ~ "NH3",
                                                 (grepl("CF4", ghg)) ~ "CF4",
                                                 (grepl("C2F6", ghg)) ~ "C2F6",
                                                 TRUE ~ "Other"))%>%
        dplyr::filter(!ghg %in% c('SO2', 'NH3', 'Other')) %>%
        #dplyr::left_join(emiss_sector_mapping, by=c('class2' = 'class1')) %>%
        #dplyr::mutate(class2=agg_sector) %>%
        #dplyr::select(-agg_sector) %>%
        dplyr::left_join(gcamextractor::GWP,by="ghg")%>%
        dplyr::left_join(gcamextractor::conv_GgTg_to_MTC,by="Units") %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(ghg=="Other"~"Units",TRUE~origUnits),
                      units="GHG Emissions by Resource GWPAR5 (MTCO2eq)")%>%
        dplyr::mutate(sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classPalette1 = "pal_all",
                      classPalette2 = "pal_all") %>%
        dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                        aggregate, ghg, sector, classPalette1, classPalette2,
                        origScen, origQuery,origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      tblNonCO2EmissRes <- tbl
    }
  }

  # add GHG by resource production to data if emissGHGByResProdGWPAR5 param is chosen
  if("emissGHGByResProdGWPAR5" %in% paramsSelectx){
    tblGHGEmissByResProd <- tblNonCO2EmissRes %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg",
                    param = "emissGHGByResProdGWPAR5",
                    units = "GHG emissions (MTCO2eq)")

    datax <- dplyr::bind_rows(datax, tblGHGEmissByResProd)
  }

  # add GHG by sector to data if emissGHGBySectorGWPAR5 param is chosen
  if("emissGHGBySectorGWPAR5" %in% paramsSelectx){
    tblGHGEmissBySector <- dplyr::bind_rows(
      tblLUEmiss, tblCO2EmissAgg, tblNonCO2EmissAgg, tblNonCO2EmissUSA,
      tblNonCO2EmissRes) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg",
                    param = "emissGHGBySectorGWPAR5",
                    units = "GHG emissions (MTCO2eq)")

    datax <- dplyr::bind_rows(datax, tblGHGEmissBySector)
  }

  # add GHG by gas to data if emissGHGByGasGWPAR5 param is chosen
  if("emissGHGByGasGWPAR5" %in% paramsSelectx){
    tblGHGEmissByGas <- dplyr::bind_rows(
      tblLUEmiss, tblCO2EmissAgg, tblNonCO2EmissAgg, tblNonCO2EmissUSA,
      tblNonCO2EmissRes) %>%
      dplyr::rename(class1 = ghg, class2 = sector) %>%
      dplyr::mutate(classLabel1 = "ghg",
                    classLabel2 = "sector",
                    param = "emissGHGByGasGWPAR5",
                    units = "GHG emissions (MTCO2eq)")

    datax <- dplyr::bind_rows(datax, tblGHGEmissByGas)
  }

  # add GHG by sector (no bio) to data if emissGHGBySectorNoBioGWPAR5 param is chosen
  if("emissGHGBySectorNoBioGWPAR5" %in% paramsSelectx){
    tblGHGEmissBySectorNoBio <- dplyr::bind_rows(
      tblLUEmiss, tblCO2EmissNoBioAgg, tblNonCO2EmissAgg, tblNonCO2EmissUSA,
      tblNonCO2EmissRes) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg",
                    param = "emissGHGBySectorNoBioGWPAR5",
                    units = "GHG emissions (MTCO2eq)")

    datax <- dplyr::bind_rows(datax, tblGHGEmissBySectorNoBio)
  }

  # add GHG by gas (no bio) to data if emissGHGByGasNoBioGWPAR5 param is chosen
  if("emissGHGByGasNoBioGWPAR5" %in% paramsSelectx){
    tblGHGEmissByGasNoBio <- dplyr::bind_rows(
      tblLUEmiss, tblCO2EmissNoBioAgg, tblNonCO2EmissAgg, tblNonCO2EmissUSA,
      tblNonCO2EmissRes) %>%
      dplyr::rename(class1 = ghg, class2 = sector) %>%
      dplyr::mutate(classLabel1 = "ghg",
                    classLabel2 = "sector",
                    param = "emissGHGByGasNoBioGWPAR5",
                    units = "GHG emissions (MTCO2eq)")

    datax <- dplyr::bind_rows(datax, tblGHGEmissByGasNoBio)
  }



  ### co2SequestrationBySector #################################################
  if(any(c("co2SequestrationBySector") %in% paramsSelectx)){
    queryx <- "CO2 sequestration by sector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(
          origValue=value,
          value=value*gcamextractor::convert$conv_C_CO2*(-1), # convert from MTC to MTCO2eq and make negative
          origUnits=Units,
          units="CO2 emissions - (MTCO2)",
          sources = "Sources",
          origScen = scenario,
          origQuery = queryx,
          origX = year, subRegion=region,
          scenario = scenNewNames,
          vintage = paste("Vint_", year, sep = ""),
          x = year,
          xLabel = "Year",
          aggregate = "sum",
          classPalette1 = "pal_all",
          classPalette2 = "pal_all",
          ghg = "CO2") %>%
        dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)
      tblCO2Seq <- tbl
      datax <- dplyr::bind_rows(datax, tblCO2Seq)
    }
  } # end CO2 sequestration query


  ### emissCO2BySubsector ######################################################
  # need CO2 by subsector for emissGHGBySectorTransportGWPAR5, emissGHGBySectorIndustryGWPAR5
  if(any(c("emissGHGBySectorTransportGWPAR5", "emissGHGBySectorIndustryGWPAR5") %in% paramsSelectx)){
    queryx <- "CO2 emissions by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(
          origValue=value,
          value=value*44/12, # convert from MTC to MTCO2eq
          origUnits=Units,
          units="CO2 emissions - (MTCO2)",
          sources = "Sources",
          origScen = scenario,
          origQuery = queryx,
          origX = year, subRegion=region,
          scenario = scenNewNames,
          vintage = paste("Vint_", year, sep = ""),
          x = year,
          xLabel = "Year",
          aggregate = "sum",
          classPalette1 = "pal_all",
          classPalette2 = "pal_all",
          ghg = "CO2") %>%
        dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                      aggregate, ghg, sector, subsector, classPalette1, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)
      tblCO2EmissSubsec <- tbl
    }
  }

  ### emiss nonCO2 by subsector ################################################
  # need for emissGHGBySectorTransportGWPAR5
  if(any(c("emissGHGBySectorTransportGWPAR5", "emissGHGBySectorIndustryGWPAR5") %in% paramsSelectx)){
    queryx <- "nonCO2 emissions by subsector"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      #emiss_sector_mapping <- utils::read.csv(CO2mappingFile, skip=1)
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(ghg = dplyr::case_when ((grepl("HFC", ghg)) ~ "HFCs",
                                              (grepl("SF6", ghg)) ~ "SF6",
                                              (grepl("CO2", ghg)) ~ "CO2",
                                              (grepl("N2O", ghg)) ~ "N2O",
                                              (grepl("CH4", ghg)) ~ "CH4",
                                              (grepl("SO2", ghg)) ~ "SO2",
                                              (grepl("NH3", ghg)) ~ "NH3",
                                              (grepl("CF4", ghg)) ~ "CF4",
                                              (grepl("C2F6", ghg)) ~ "C2F6",
                                              TRUE ~ "Other"))%>%
        dplyr::left_join(gcamextractor::GWP,by="ghg") %>%
        dplyr::left_join(gcamextractor::conv_GgTg_to_MTC,by="Units") %>%
        dplyr::filter(ghg!='CO2') %>%
        dplyr::mutate(origValue=value,
                      value=value*GWPAR5*Convert,
                      origUnits=Units,
                      origUnits = dplyr::case_when(ghg=="Other"~"Units",TRUE~origUnits),
                      units="GHG Emissions GWPAR5 (MTCO2eq)",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      classPalette2 = "pal_all",
                      classPalette1 = "pal_all") %>%
        dplyr::select(scenario, region, subRegion, sources, x, xLabel, vintage, units, value,
                      aggregate, ghg, sector, subsector, classPalette1, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)

      tblNonCO2EmissSubsec <- tbl
    } else {
      tblNonCO2EmissSubsec <- tibble::tibble()
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ## Power =====================================================================

  ### emissGHGBySectorPowerGWPAR5 ##############################################
  paramx <- "emissGHGBySectorPowerGWPAR5"
  if(paramx %in% paramsSelectx) {
    # CO2 emissions
    tblCO2EmissPower <- tblCO2Emiss %>%
      dplyr::filter(grepl("elec", sector)) %>%
      dplyr::mutate(sector = dplyr::case_when(
        grepl("biomass", sector) ~ paste0(ghg, "_biomass"),
        grepl("coal", sector) ~ paste0(ghg, "_coal"),
        grepl("gas", sector) ~ paste0(ghg, "_natural gas"),
        grepl("refined liquids", sector) ~ paste0(ghg, "_refined liquids"),
        ghg != "CO2" ~ ghg,
        T ~ paste0(sector, "_", ghg))) %>%
      dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # NonCO2 emissions
    tblNonCO2EmissPower <- tblNonCO2Emiss %>%
      dplyr::filter(grepl("elec", sector)) %>%
      dplyr::mutate(sector = dplyr::case_when(
        grepl("biomass", sector) ~ paste0(ghg, "_biomass"),
        grepl("coal", sector) ~ paste0(ghg, "_coal"),
        grepl("gas", sector) ~ paste0(ghg, "_natural gas"),
        grepl("refined liquids", sector) ~ paste0(ghg, "_refined liquids"),
        ghg != "CO2" ~ ghg,
        T ~ paste0(sector, "_", ghg))) %>%
      dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # all CO2
    tblEmissGHGPower <- dplyr::bind_rows(tblCO2EmissPower,
                                         tblNonCO2EmissPower) %>%
      dplyr::mutate(param = "emissGHGBySectorPowerGWPAR5",
                    units="GHG emissions - (MTCO2eq)")
    datax <- dplyr::bind_rows(datax, tblEmissGHGPower)
  }

  ## Buildings =================================================================

  ### emissGHGBySectorBuildingsGWPAR5 ##########################################
  paramx <- "emissGHGBySectorBuildingsGWPAR5"
  if(paramx %in% paramsSelectx) {
      # CO2 emissions
      tblCO2EmissBuild <- tblCO2EmissNoBio %>%
        dplyr::filter(grepl("comm|resid", sector)) %>%
        dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                        aggregate, ghg, sector, classPalette1, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::rename(class1 = sector, class2 = ghg) %>%
        dplyr::mutate(classLabel1 = "sector",
                      classLabel2 = "ghg")

      # NonCO2 emissions
      tblNonCO2EmissBuild <- tblNonCO2Emiss %>%
        dplyr::filter(grepl("comm|resid", sector)) %>%
        dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                        aggregate, ghg, sector, classPalette1, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::rename(class1 = sector, class2 = ghg) %>%
        dplyr::mutate(classLabel1 = "sector",
                      classLabel2 = "ghg")

      # all CO2
      tblEmissGHGBuild <- dplyr::bind_rows(tblCO2EmissBuild,
                                           tblNonCO2EmissBuild) %>%
        dplyr::mutate(param = "emissGHGBySectorBuildingsGWPAR5",
                      units="GHG emissions - (MTCO2eq)")
      datax <- dplyr::bind_rows(datax, tblEmissGHGBuild)
  }

  ## Transport =================================================================

  ### emissGHGBySectorTransportGWPAR5 ##########################################
  paramx <- "emissGHGBySectorTransportGWPAR5"
  if(paramx %in% paramsSelectx) {
    # CO2 emissions
    tblCO2EmissTrn <- tblCO2EmissSubsec %>%
      dplyr::filter(grepl("trn", sector)) %>%
      dplyr::mutate(sector = dplyr::case_when(
        subsector %in% c("Car", "Large Car and Truck", "Mini Car") ~ "4W LDV",
        subsector %in% c("Heavy truck", "Light truck", "Medium truck") ~ "Freight road",
        T ~ subsector)) %>%
      dplyr::select(-subsector) %>%
      dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # NonCO2 emissions
    tblNonCO2EmissTrn <- tblNonCO2EmissSubsec %>%
      dplyr::filter(grepl("trn", sector)) %>%
      dplyr::mutate(sector = dplyr::case_when(
        subsector %in% c("Car", "Large Car and Truck", "Mini Car") ~ "4W LDV",
        subsector %in% c("Heavy truck", "Light truck", "Medium truck") ~ "Freight road",
        T ~ subsector)) %>%
      dplyr::select(-subsector) %>%
      dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # all CO2
    tblEmissGHGTrn <- dplyr::bind_rows(tblCO2EmissTrn,
                                       tblNonCO2EmissTrn) %>%
      dplyr::mutate(param = "emissGHGBySectorTransportGWPAR5",
                    units="GHG emissions - (MTCO2eq)")
    datax <- dplyr::bind_rows(datax, tblEmissGHGTrn)
  }

  ## Industry ==================================================================

  ### emissGHGBySectorIndustryGWPAR5 ###########################################
  paramx <- "emissGHGBySectorIndustryGWPAR5"
  if(paramx %in% paramsSelectx) {
    # CO2 emissions
    tblCO2EmissInd <- tblCO2EmissSubsec %>%
      dplyr::filter(grepl("ind|N fertilizer|cement", sector)) %>%
      dplyr::mutate(sector = dplyr::case_when(
        sector == "industrial energy use" ~ subsector,
        sector == "industrial feedstocks" ~ sector,
        grepl("cement", sector) ~ "cement",
        grepl("industrial processes", sector) ~ "industrial processes",
        grepl("N fertilizer", sector) ~ "N fertilizer",
        T ~ paste0(sector, "_", subsector))) %>%
      dplyr::select(-subsector) %>%
      dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # NonCO2 emissions
    tblNonCO2EmissInd <- tblNonCO2EmissSubsec %>%
      dplyr::filter(grepl("ind|N fertilizer|cement", sector)) %>%
      dplyr::mutate(sector = dplyr::case_when(
        sector == "industrial energy use" ~ subsector,
        sector == "industrial feedstocks" ~ sector,
        grepl("cement", sector) ~ "cement",
        grepl("industrial processes", sector) ~ "industrial processes",
        grepl("N fertilizer", sector) ~ "N fertilizer",
        T ~ paste0(sector, "_", subsector))) %>%
      dplyr::select(-subsector) %>%
      dplyr::group_by(scenario, region, subRegion, sources, x, xLabel, vintage, units,
                      aggregate, ghg, sector, classPalette1, classPalette2,
                      origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::rename(class1 = sector, class2 = ghg) %>%
      dplyr::mutate(classLabel1 = "sector",
                    classLabel2 = "ghg")

    # all CO2
    tblEmissGHGInd <- dplyr::bind_rows(tblCO2EmissInd,
                                       tblNonCO2EmissInd) %>%
      dplyr::mutate(param = "emissGHGBySectorIndustryGWPAR5",
                    units="GHG emissions - (MTCO2eq)")
    datax <- dplyr::bind_rows(datax, tblEmissGHGInd)
  }


  # Outputs --------------------------------------------------------------------

  ## Transport =================================================================

  ### transportPassengerVMTByMode ###############################################
  paramx<-"transportPassengerVMTByMode"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport service output by mode"
    vmt_array <- c("trn_pass", "trn_pass_road", "trn_pass_road_LDV",
                   "trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !mode %in% c('Cycle', 'Walk', '2W', '4W', 'LDV', 'road')) %>%
        dplyr::mutate(#mode=gsub("International\\sAviation","Plane",mode),
                      #mode=gsub("Domestic\\sAviation","Plane",mode),
                      mode=gsub("HSR","Rail",mode),
                      mode=gsub("Passenger\\sRail","Rail",mode),
                      mode=gsub("Bus","Bus",mode),
                      mode=gsub("Moped","MotorBike",mode),
                      mode=gsub("Motorcycle\\s[(]50-250cc[)]","MotorBike",mode),
                      mode=gsub("Motorcycle\\s[:(:][:>:]250cc[:):]","MotorBike",mode),
                      mode=gsub("Compact\\sCar","LDV",mode),
                      mode=gsub("Large\\sCar\\sand\\sTruck", "LDV", mode),
                      mode=gsub("2W\\sand\\s3W", "LDV", mode),
                      mode=gsub("Large\\sCar\\sand\\sSUV","LDV",mode),
                      mode=gsub("Mini\\sCar","LDV",mode),
                      mode=gsub("Subcompact\\sCar","LDV",mode),
                      mode=gsub("Car", "LDV", mode),
                      param = "transportPassengerVMTByMode",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger (million pass-km)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = mode,
                      classLabel1 = "Mode",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### transportFreightVMTByMode ################################################
  paramx<-"transportFreightVMTByMode"
  # Total final energy by aggregate end-use sector
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport service output by mode"
    vmt_array <- c("trn_freight", "trn_freight_road")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect,
                                     sector %in% vmt_array)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !mode %in% c('road')) %>%
        dplyr::mutate(mode=dplyr::if_else(mode=="Domestic Ship","Ship",mode),
                      mode=dplyr::if_else(mode=="Freight Rail","Rail",mode),
                      #mode=dplyr::if_else(mode=="International Ship","Ship",mode),
                      mode=dplyr::if_else(mode=="Truck (6-15t)","Truck",mode),
                      mode=dplyr::if_else(mode=="Truck (0-1t)","Truck",mode),
                      mode=dplyr::if_else(mode=="Truck (>15t)","Truck",mode),
                      param = "transportFreightVMTByMode",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Freight (million ton-km)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = mode,
                      classLabel1 = "Mode",
                      classPalette1 = "pal_all",
                      class2 = sector,
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### transportPassengerVMTByFuel ##############################################
  paramx<-"transportPassengerVMTByFuel"
  # Passenger VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport service output by tech"
    vmt_array <- c("trn_pass", "trn_pass_road", "trn_pass_road_LDV",
                   "trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array,
                      !technology %in% c('Cycle', 'Walk', '2W', '4W', 'LDV', 'road')) %>%
        dplyr::mutate(technology=gsub("NG","gas", technology),
                      technology=gsub("FCEV","hydrogen", technology),
                      technology=gsub("Hydrogen","hydrogen", technology),
                      technology=gsub("BEV","electricity", technology),
                      technology=gsub("Electric","electricity", technology),
                      technology=gsub("Liquids","liquids", technology),
                      technology=gsub("Hybrid Liquids","liquids", technology),
                      technology=gsub("Hybrid liquids","liquids", technology),
                      param = "transportPassengerVMTByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger (million pass-km)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'subRegion','x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### transportPassengerVMTByTech ##############################################
  paramx<-"transportPassengerVMTByTech"
  # Passenger VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport service output by tech"
    vmt_array <- c("trn_pass", "trn_pass_road", "trn_pass_road_LDV",
                   "trn_pass_road_LDV_2W", "trn_pass_road_LDV_4W")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('Cycle', 'Walk', '2W', '4W', 'LDV', 'road')) %>%
        dplyr::mutate(technology=dplyr::case_when(
          subsector == "Bus" & technology == "BEV" ~ "bus (BEV)",
          subsector == "Bus" & technology== "FCEV" ~ "bus (FCEV)",
          subsector == "Bus" & grepl("Hybrid", technology) ~ "bus (hybrid)",
          subsector == "Bus" ~ "bus (other)",
          grepl("Car|2W", subsector) & technology == "BEV" ~ "LDV (BEV)",
          grepl("Car|2W", subsector) & technology == "FCEV" ~ "LDV (FCEV)",
          grepl("Car|2W", subsector) & technology == "Hybrid Liquids" ~ "LDV (hybrid)",
          grepl("Car|2W", subsector) ~ "LDV (other)",
          subsector == "Passenger Rail" & technology %in% c("Electric", "Tech-Adv-Electric") ~ "rail (electric)",
          subsector == "Passenger Rail" & technology %in% c("Liquids", "Tech-Adv-Liquid") ~ "rail (other)",
          grepl("Aviation", subsector) ~ "Airplane",
          subsector == "HSR" ~ "rail (HSR)",
          T ~ paste0(subsector, "_", technology)),
                      param = "transportPassengerVMTByTech",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Passenger (million pass-km)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region', 'subRegion','x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### transportFreightVMTByFuel ################################################
  paramx<-"transportFreightVMTByFuel"
  # Freight VMT (services) by fuel
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport service output by tech"
    vmt_array <- c("trn_freight", "trn_freight_road")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('road')) %>%
        dplyr::mutate(technology=gsub("NG","gas", technology),
                      technology=gsub("Liquids","liquids", technology),
                      technology=gsub("Hybrid liquids","liquids", technology),
                      technology=gsub("Hybrid Liquids","liquids", technology),
                      technology=gsub("Electric","electricity", technology),
                      technology=gsub("BEV","electricity", technology),
                      technology=gsub("FCEV","hydrogen", technology),
                      technology=gsub("Coal","coal", technology),
                      param = "transportFreightVMTByFuel",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Freight (million ton-km)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### transportFreightVMTByTech ################################################
  paramx<-"transportFreightVMTByTech"
  # Freight VMT (services) by tech
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "transport service output by tech"
    vmt_array <- c("trn_freight", "trn_freight_road")
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::filter(sector %in% vmt_array, !technology %in% c('road')) %>%
        dplyr::mutate(technology = dplyr::case_when(
          subsector == "Domestic Ship" ~ "Shipping",
          subsector == "Freight Rail" ~ "Rail",
          sector == "trn_freight_road" & technology == "BEV" ~ "truck (BEV)",
          sector == "trn_freight_road" & technology == "FCEV" ~ "truck (FCEV)",
          sector == "trn_freight_road" & technology == "Hybrid Liquids" ~ "truck (hybrid)",
          sector == "trn_freight_road" ~ "truck (other)",
          T ~ paste0(subsector, "_", technology)
        ),
                      param = "transportFreightVMTByTech",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      units = "Freight (million ton-km)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = technology,
                      classLabel1 = "Fuel",
                      classPalette1 = "pal_all",
                      class2 = subsector,
                      classLabel2 = "subsector",
                      classPalette2 = "classPalette2")
      if("energyPrimaryRefLiqProdEJ" %in% paramsSelectx){
        # Break out biofuels
        tbl <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl, by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::mutate(value = dplyr::if_else(class1=='liquids', value*FracBioFuel, value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel) %>%
          dplyr::mutate(class1=dplyr::if_else(class1=='liquids', 'biofuel', class1))
        tbl2 <- tbl %>%
          dplyr::left_join(FracBioFuel_tbl %>% dplyr::mutate(class1='biofuel'), by=c('scenario', 'region','subRegion', 'x', 'class1')) %>%
          dplyr::filter(class1=='biofuel') %>%
          dplyr::mutate(class1='fossil fuel') %>%
          dplyr::mutate(value=dplyr::if_else(class1=='fossil fuel', (value/FracBioFuel)*(1-FracBioFuel), value)) %>%
          dplyr::select(-FracBioFuel, -FracFossilFuel)
        tbl <- rbind(tbl, tbl2)
      }
      tbl <- tbl %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}



  ## Buildings =================================================================

  ### serviceOutputByTechBuildingsResid ########################################
  paramx <- "serviceOutputByTechBuildingsResid"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "building service output by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      grepl("resid", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(
          sector = dplyr::case_when(grepl("cook", sector) ~ "cooking",
                                    grepl("light", sector) ~ "lighting",
                                    grepl("air con|vent", sector) ~ "ACMV",
                                    T ~ "other"),
          class1 = paste0(sector, " (", subsector, ")"),
          param = "serviceOutputByTechBuildingsResid",
          sources = "Sources",
          origScen = scenario,
          origQuery = queryx,
          origValue = value,
          origUnits = Units,
          origX = year, subRegion=region,
          scenario = scenNewNames,
          units = Units,
          vintage = paste("Vint_", year, sep = ""),
          x = year,
          xLabel = "Year",
          aggregate = "sum",
          classLabel1 = "technology",
          classPalette1 = "pal_all",
          class2 = "class2",
          classLabel2 = "classLabel2",
          classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ### serviceOutputByTechBuildingsResid ########################################
  paramx <- "serviceOutputByTechBuildingsComm"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "building service output by tech"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames,
                      grepl("comm", sector))%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(
          sector = dplyr::case_when(grepl("cook", sector) ~ "cooking",
                                    grepl("light", sector) ~ "lighting",
                                    grepl("hvac", sector) ~ "ACMV",
                                    T ~ "other"),
          class1 = paste0(sector, " (", subsector, ")"),
          param = "serviceOutputByTechBuildingsComm",
          sources = "Sources",
          origScen = scenario,
          origQuery = queryx,
          origValue = value,
          origUnits = Units,
          origX = year, subRegion=region,
          scenario = scenNewNames,
          units = Units,
          vintage = paste("Vint_", year, sep = ""),
          x = year,
          xLabel = "Year",
          aggregate = "sum",
          classLabel1 = "technology",
          classPalette1 = "pal_all",
          class2 = "class2",
          classLabel2 = "classLabel2",
          classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}



  # Socioeconomics -------------------------------------------------------------

  ### pop ######################################################################
  paramx <- "pop"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    # Population
    queryx <- "Population by region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "pop",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value/1000,
                      units = "Population (Millions)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = "class1",
                      classLabel1 = "Population",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ## gdpPerCapita ==============================================================
  paramx <- "gdpPerCapita"
  if(paramx  %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    # GDP MER per Capita MER by region
    queryx <- "GDP per capita MER by region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "gdpPerCapita",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value,
                      units = "GDP per Capita (Thousand 1990 USD per Person)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = "class1",
                      classLabel1 = "GDP Per Capita",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ## gdp =======================================================================
  paramx <- "gdp"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    # GDP MER by region
    queryx <- "GDP MER by region"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames)%>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(param = "gdp",
                      sources = "Sources",
                      origScen = scenario,
                      origQuery = queryx,
                      origValue = value,
                      origUnits = Units,
                      origX = year, subRegion=region,
                      scenario = scenNewNames,
                      value = value/1000,
                      units = "GDP (Billion 1990 USD)",
                      vintage = paste("Vint_", year, sep = ""),
                      x = year,
                      xLabel = "Year",
                      aggregate = "sum",
                      class1 = "class1",
                      classLabel1 = "GDP",
                      classPalette1 = "pal_16",
                      class2 = "class2",
                      classLabel2 = "classLabel2",
                      classPalette2 = "classPalette2") %>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
      tblgdp<-tbl
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  ## buildingFloorspace ========================================================
  paramx <- "buildingFloorspace"
  if(paramx %in% paramsSelectx){
    rlang::inform(paste0("Running param: ", paramx,"..."))
    queryx <- "building floorspace"
    if (queryx %in% queriesx) {
      tbl <- rgcam::getQuery(dataProjLoaded, queryx)  # Tibble
      if (!is.null(regionsSelect)) {
        tbl <- tbl %>% dplyr::filter(region %in% regionsSelect)
      }
      tbl <- tbl %>%
        dplyr::filter(scenario %in% scenOrigNames) %>%
        dplyr::left_join(tibble::tibble(scenOrigNames, scenNewNames), by = c(scenario = "scenOrigNames")) %>%
        dplyr::mutate(
          class1 = nodeinput,
          param = "buildingFloorspace",
          sources = "Sources",
          origScen = scenario,
          origQuery = queryx,
          origValue = value,
          origUnits = Units,
          origX = year,
          subRegion=region,
          scenario = scenNewNames,
          units = Units,
          vintage = paste("Vint_", year, sep = ""),
          x = year,
          xLabel = "Year",
          aggregate = "sum",
          classLabel1 = "nodeinput",
          classPalette1 = "pal_all",
          class2 = "class2",
          classLabel2 = "classLabel2",
          classPalette2 = "classPalette2")%>%
        dplyr::select(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units, value,
                      aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                      origScen, origQuery, origValue, origUnits, origX)%>%
        dplyr::group_by(scenario, region, subRegion,    param, sources, class1, class2, x, xLabel, vintage, units,
                        aggregate, classLabel1, classPalette1,classLabel2, classPalette2,
                        origScen, origQuery, origUnits, origX)%>%dplyr::summarize_at(dplyr::vars("value","origValue"),list(~sum(.,na.rm = T)))%>%dplyr::ungroup()%>%
        dplyr::filter(!is.na(value))
      datax <- dplyr::bind_rows(datax, tbl)
    } else {
      # if(queryx %in% queriesSelectx){rlang::inform(paste("Query '", queryx, "' not found in database", sep = ""))}
    }}


  } # Close datax assignments


  if(nrow(datax)>0){

  datax <- datax %>%
    unique()

  # ...........
  # unit Conversions
  # ...........
  # dataxEJtoMTOE <- datax %>% dplyr::filter(grepl("\\(EJ\\)",units)) %>%
  #   dplyr::mutate(value=value*gcamextractor::convert$conv_EJ_to_MTOE,
  #                 units = gsub("\\(EJ\\)","(Mtoe)",units),
  #                 param = gsub("EJ","MTOE",param)); dataxEJtoMTOE
  #
  # dataxEJtoTWh <- datax %>% dplyr::filter(grepl("\\(EJ\\)",units)) %>%
  #   dplyr::mutate(value=value*gcamextractor::convert$conv_EJ_to_TWh,
  #                 units = gsub("\\(EJ\\)","(TWh)",units),
  #                 param = gsub("EJ","TWh",param))

  #datax <- dplyr::bind_rows(datax,dataxEJtoMTOE,dataxEJtoTWh)


  # Remove repeated USA regions
  for(scenario_i in unique(datax$scenario)){
  for(param_i in unique(datax$param)){

    regions_i <-(datax %>% dplyr::filter(param==param_i,scenario==scenario_i))$region%>%unique(); regions_i

    if(any(grepl("USA",regions_i)) & length(regions_i[regions_i %in% gcamextractor::regions_US52])>1){
      datax <- datax %>%
        dplyr::mutate(region = dplyr::case_when(param==param_i &
                                           scenario==scenario_i &
                                           (region=="USA" & !param %in% c("emissNonCO2BySectorGWPAR5",
                                                                          "emissNonCO2BySector",
                                                                          "emissCO2BySector",
                                                                          "emissCO2CumGlobal2010to2100",
                                                                          "emissCO2CumGlobal2010to2100RCP"))~ "RegionRemove",
                                         TRUE ~ region)) %>%
        dplyr::filter(!grepl("remove",region,ignore.case = T)) %>%
        dplyr::filter(!grepl("remove",subRegion,ignore.case = T))
    }

    subRegions_i <-(datax %>% dplyr::filter(param==param_i,scenario==scenario_i))$subRegion%>%unique(); subRegions_i

    if(any(grepl("USA",subRegions_i)) & length(subRegions_i[subRegions_i %in% gcamextractor::regions_US52])>1){
      datax <- datax %>%
        dplyr::mutate(subRegion = dplyr::case_when(param==param_i &
                                           scenario==scenario_i &
                                           (subRegion=="USA" & !param %in% c("emissNonCO2BySectorGWPAR5",
                                                                             "emissNonCO2BySector",
                                                                             "emissCO2BySector",
                                                                             "emissCO2CumGlobal2010to2100",
                                                                             "emissCO2CumGlobal2010to2100RCP"))~ "subRegionRemove",
                                         TRUE ~ subRegion)) %>%
        dplyr::filter(!grepl("remove",region,ignore.case = T))  %>%
        dplyr::filter(!grepl("remove",subRegion,ignore.case = T))
    }
  }
  }

  # datax$scenario%>%unique()
  # datax$param%>%unique()

  # chart(tbl,xData="x",yData="value",useNewLabels = 0)

  # Check
  # unique(datax$param)%>%sort();unique(datax$scenario)%>%sort()
  # datax%>%as.data.frame()%>%dplyr::select(scenario,class1,class2,x,param,value)%>%
  # dplyr::filter(x %in% c(2010:2050),param=="elecNewCapGW",scenario=="GCAMRef")%>%
  # dplyr::group_by(scenario,x)%>%dplyr::summarize(sum=sum(value))

  #.....................
  # Save Data in CSV
  #.....................


  if(!all(regionsSelect %in% unique(datax$region))){
    rlang::inform(paste("Regions not available in data: ", paste(regionsSelect[!(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
    rlang::inform(paste("Running remaining regions: ",  paste(regionsSelect[(regionsSelect %in% unique(datax$region))],collapse=", "), sep=""))
  }

  if(saveData){

    # All Data
    utils::write.csv(datax,
                     file = paste(folder, "/gcamDataTable_Extended",nameAppend, ".csv", sep = ""), row.names = F)
    rlang::inform(paste("GCAM data table saved to: ",
                gsub("//","/",paste(folder, "/gcamDataTable_Extended",nameAppend,".csv", sep = ""))))

     # Data
    utils::write.csv(datax %>% dplyr::select("scenario","region","subRegion","param","classLabel1","class1","classLabel2","class2",
                                             "xLabel","x","vintage","units","value"),
                     file = paste(folder, "/gcamDataTable",nameAppend, ".csv", sep = ""), row.names = F)
    rlang::inform(paste("GCAM data table saved to: ",
                gsub("//","/",paste(folder, "/gcamDataTable",nameAppend,".csv", sep = ""))))
  }

  # aggregate regions
  if(!is.null(regionsAggregate)){
    # if regionsAggregate is a vector, convert to list
    if(!is.list(regionsAggregate)){
      regionsAggregate <- list(regionsAggregate)
    }
    useCustomName <- F
    # check length of names vector matches length of groups list
    if(!is.null(regionsAggregateNames)){
      if(length(regionsAggregateNames) == length(regionsAggregate)){
        useCustomName <- T
      } else {rlang::inform("Number of aggregate region names does not match number of aggregate region groups. Using default aggregate names.")}
    }
    # perform aggregation for each group of regions
    for(i in 1:length(regionsAggregate)){
      group = regionsAggregate[[i]]
      # assign custom region aggregate name if given
      if(useCustomName){aggName = regionsAggregateNames[i]}
      # generate default aggregated region name if not given
      else {aggName = paste(group, collapse = "_")}
      # perform aggregation for summed parameters
      dataxAggRegionsSums <- datax %>%
        dplyr::filter(aggregate=="sum",
                      region %in% group) %>%
        dplyr::group_by(dplyr::across(-c(region, subRegion, value, origValue))) %>%
        dplyr::summarise(value=sum(value)) %>%
        dplyr::mutate(region = aggName,
                      subRegion = aggName)
      # perform aggregation for averaged parameters
      dataxAggRegionsMeans <- datax %>%
        dplyr::filter(aggregate=="mean",
                      region %in% group) %>%
        dplyr::group_by(dplyr::across(-c(region, subRegion, value, origValue))) %>%
        dplyr::summarise(value=mean(value)) %>%
        dplyr::mutate(region = aggName,
                      subRegion = aggName)
      # combine aggregated data with original data
      datax<-dplyr::bind_rows(datax, dataxAggRegionsSums, dataxAggRegionsMeans)%>%dplyr::ungroup()
    }

  }

    # Aggregate across Class 2
    dataxAggsums<-datax%>%
      dplyr::filter(aggregate=="sum")%>%
      dplyr::select(-c(class1,classLabel1,classPalette1, origQuery, origUnits))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T))) %>%
      dplyr::ungroup()
    dataxAggmeans<-datax%>%
      dplyr::filter(aggregate=="mean")%>%
      dplyr::select(-c(class1,classLabel1,classPalette1, origQuery, origUnits))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))%>%
      dplyr::ungroup()
    dataxAggClass<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

    dataAggClass2 = dataxAggClass %>% dplyr::rename(class=class2,classLabel=classLabel2,classPalette=classPalette2) %>% unique()



    if(saveData){
      utils::write.csv(dataxAggClass %>%
                         dplyr::rename(class=class2,classLabel=classLabel2,classPalette=classPalette2)%>%
                         dplyr::select("scenario","region","subRegion","param","classLabel","class","classLabel",
                                       "xLabel","x","vintage","units","value"),
                       file = gsub("//","/",paste(folder,
                                                  "/gcamDataTable_aggClass2",
                                                  nameAppend,".csv", sep = "")),row.names = F)

      rlang::inform(paste("GCAM data aggregated to class 2 saved to: ",gsub("//","/",paste(folder,
                                "/gcamDataTable_aggClass2",
                                nameAppend,".csv", sep = "")),sep=""))

      }

    # Aggregate across Class 1
    dataxAggsums<-datax%>%
      dplyr::filter(aggregate=="sum")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2, origQuery, origUnits))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T)))%>%
      dplyr::ungroup()
    dataxAggmeans<-datax%>%
      dplyr::filter(aggregate=="mean")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2, origQuery, origUnits))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))%>%
      dplyr::ungroup()
    dataxAggClass<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

    dataAggClass1 = dataxAggClass  %>% dplyr::rename(class=class1,classLabel=classLabel1,classPalette=classPalette1) %>% unique()

    if(saveData){

    utils::write.csv(dataxAggClass  %>%
                       dplyr::rename(class=class1,classLabel=classLabel1,classPalette=classPalette1) %>%
                       dplyr::select("scenario","region","subRegion","param","classLabel","class","classLabel",
                                     "xLabel","x","vintage","units","value"),
                     file = gsub("//","/",paste(folder,
                                                "/gcamDataTable_aggClass1",
                                                nameAppend,".csv", sep = "")),row.names = F)

    rlang::inform(paste("GCAM data aggregated to class 1 saved to: ",gsub("//","/",paste(folder,
                                                                                   "/gcamDataTable_aggClass1",
                                                                                   nameAppend,".csv", sep = "")),sep=""))

    }

    # Aggregate across Param
    dataxAggsums<-datax%>%
      dplyr::filter(aggregate=="sum")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2,class1,classLabel1))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~sum(.,na.rm = T)))%>%
      dplyr::ungroup()
    dataxAggmeans<-datax%>%
      dplyr::filter(aggregate=="mean")%>%
      dplyr::select(-c(class2,classLabel2,classPalette2,class1,classLabel1))%>%
      dplyr::group_by_at(dplyr::vars(-value,-origValue))%>%
      dplyr::summarize_at(c("value"),list(~mean(.,na.rm = T)))%>%
      dplyr::ungroup()
    dataxAggClass<-dplyr::bind_rows(dataxAggsums,dataxAggmeans)%>%dplyr::ungroup()

    dataAggParam = dataxAggClass %>% dplyr::rename(classPalette=classPalette1) %>% unique()

    if(saveData){
    utils::write.csv(dataxAggClass %>%
                       dplyr::rename(classPalette=classPalette1) %>%
                       dplyr::select("scenario","region","subRegion","param","xLabel","x","vintage","units","value"),
                     file = gsub("//","/",paste(folder,
                                                "/gcamDataTable_aggParam",
                                                nameAppend,".csv", sep = "")),row.names = F)


    rlang::inform(paste("GCAM data aggregated to param saved to: ",gsub("//","/",paste(folder,
                                                                                   "/gcamDataTable_aggParam",
                                                                                   nameAppend,".csv", sep = "")),sep=""))
    }

  }else{rlang::inform(paste0("No data for any of the regions, params or queries selected"))} # Close datax nrow check

  }else{ # CLose Param Check
    rlang::inform(paste("None of the parameters in paramsSelect: ", paste(paramsSelect,collapse=",")," are available."))}

  rlang::inform(paste0("Outputs returned as list containing data, scenarios and queries."))
  rlang::inform(paste0("For example if df <- readgcam(dataProjFile = gcamextractor::example_GCAMv52_2050_proj)"))
  rlang::inform(paste0("Then you can view the outputs as df$data, df$dataAggClass1, df$dataAggClass2, df$dataAggParam, df$scenarios, df$queries."))
  rlang::inform((gsub("//","/",paste("All outputs in : ",normalizePath(folder),sep=""))))
  rlang::inform(paste0("readgcam run completed."))


  if(nrow(datax)>0){
  return(list(dataAll = datax,
              data = datax %>% dplyr::select("scenario","region","subRegion","param","classLabel1","class1","classLabel2","class2",
                                             "xLabel","x","vintage","units","value"),
              dataAggClass1 = dataAggClass1 %>% dplyr::select("scenario","region","subRegion","param","classLabel","class","classLabel",
                                                              "xLabel","x","vintage","units","value"),
              dataAggClass2 = dataAggClass2  %>% dplyr::select("scenario","region","subRegion","param","classLabel","class","classLabel",
                                                               "xLabel","x","vintage","units","value"),
              dataAggParam = dataAggParam %>% dplyr::select("scenario","region","subRegion","param","xLabel","x","vintage","units","value"),
              scenarios = scenarios,
              queries = queries))} else {
                rlang::warn("No data extracted for chosen arguments.")
              }

    }
