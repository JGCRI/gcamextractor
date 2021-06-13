library(gcamextractor)
library(rpackageutils)
library(testthat)
library(rprojroot)
library(rgcam)

testthat::skip_on_cran()
testthat::skip_on_travis()

# Default arguments
# gcamdatabase = NULL
# queryFile = NULL
# dataProjFile = paste(getwd(), "/outputs/dataProj.proj", sep = "")
# scenOrigNames = "All"
# scenNewNames = NULL
# reReadData = T
# dirOutputs = paste(getwd(), "/outputs", sep = "")
# regionsSelect = NULL
# paramsSelect = "All"
# folderName = NULL
# nameAppend = ""
# saveData = T


# Tests using .proj file =======================================================
# Test arguments
gcamData <- gcamextractor::readgcam(queryFile = NULL,
                                    dataProjFile = gcamextractor::exampleGCAMproj,
                                    scenOrigNames = 'GCAM_SSP5',
                                    scenNewNames = 'SSP5',
                                    paramsSelect = 'elecByTechTWh',
                                    regionsSelect = 'USA')


test_that("data exists", {
  test <- nrow(gcamData$data)
  testthat::expect_gt(test, 0)
})


test_that("scenario is selected and new name is applied", {
  test <- unique(gcamData$data$scenario)
  testthat::expect_equal(test, 'SSP5')
})

test_that("region is selected", {
  test <- unique(gcamData$data$region)
  testthat::expect_equal(test, 'USA')
})

test_that("parameter is selected", {
  test <- unique(gcamData$data$param)
  testthat::expect_equal(test, 'elecByTechTWh')
})


# ==============================================================================
# Test errors and warnings
test_path <- paste0(rprojroot::find_root(rprojroot::is_testthat),"/outputs")

test_that("throw error message if the path to gcamdatabase doesn't exist", {
  testthat::expect_error(gcamextractor::readgcam(gcamdatabase = test_path))
})

test_that("throw error message if the path to .proj file doesn't exist", {
  testthat::expect_error(gcamextractor::readgcam(dataProjFile = paste0(test_path, '/test.proj'),
                                                 reReadData = F))
})

#...................
# Test that modified queries work
#...................

# Use a modified query file and see if it works with the command.

#...................
# Test each param exists gcamextractor::mappings$mapParamQuery
#...................

# unique(gcamData$param) %in% unique(mappings$mapParamQuery$param)


# Tests using gcamdatabase ======================================================

# NOTE: rgcam::localDBConn does not work when testing because of issues with paths and relative paths.
# NOTE: Need to fix rgcam::localDBConn before the readgcam function will work in tests for GCAM databases.

# if(!dir.exists(paste0(rprojroot::find_root(rprojroot::is_testthat),"/testOutput"))){
#   dir.create(paste0(rprojroot::find_root(rprojroot::is_testthat),"/testOutput"))
# }
# test_path = paste0(rprojroot::find_root(rprojroot::is_testthat),"/testOutput"); test_path
# datadir <- rpackageutils::download_unpack_zip(
#   url = "https://zenodo.org/record/4763523/files/database_basexdb_5p3_release.zip?download=1",
#   data_directory = test_path)

# datadir = paste0(rprojroot::find_root(rprojroot::is_testthat),"/testOutput/database_basexdb_5p3_release")
# x <- capture.output(rgcam::localDBConn(dirname(datadir),basename(datadir)), type="message")
# x <- gsub(", ",",",gsub(": ","",gsub("Database scenarios:  ","",x)));x
# scenarios <- as.vector(unlist(strsplit(gsub("Database scenarios: ","",x),",")))
# print(paste0("SCENARIOS FOUND: ",scenarios))
#
# gcamData <- gcamextractor::readgcam(gcamdatabase = datadir,
#                                     queryFile = NULL,
#                                     scenOrigNames = 'Reference_gcam5p3_release',
#                                     scenNewNames = 'Ref',
#                                     paramsSelect = 'pop',
#                                     regionsSelect = 'USA')
#
#
# test_that("gcamdatabase data is read in correctly", {
#   test <- nrow(gcamData$data)
#   testthat::expect_gt(test, 0)
# })
#
# test_that("Use gcamdatabase & reReadData = T will save a .proj output", {
#   test <- file.exists(paste0(rprojroot::find_root(rprojroot::is_testthat),'/outputs/dataProj.proj'))
#   testthat::expect_equal(test, TRUE)
# })



