library(gcamextractor); library(testthat); library(dplyr); library(rlang);
context("gcamextractor Tests")

# Run tests with test data
# Can define your own write_dir and dir_name.

data_folder <- "im3_test_data"

gcamextractor::get_example_data(
  write_dir = getwd(),
  dir_name = data_folder,
  data_link = "https://zenodo.org/record/7853231/files/gcamextractor_im3_gcamuse_test_data.zip?download=1"
) -> data_folder; data_folder;

#data_folder <- "C:/Z/models/gcamextractor/tests/testthat/im3_test_data"
print(paste0("data_folder : ", data_folder))


# IM3 GCAM-USA CERF Test
#..........................
#rgcam::localDBConn(data_folder,"database_rcp85hotter_ssp5_test2025")
dataGCAM <- gcamextractor::readgcam(reReadData = T,
                     #gcamdatabase = paste0(data_folder,"/database_rcp85hotter_ssp5_test2025"),
                     gcamdata_folder = paste0(data_folder,"/gcamdata"),
                     dataProjFile = paste0(data_folder,"/dataProj_cerf.proj"),
                     regionsSelect = c("Global","USA",gcamextractor::map_state_to_gridregion$state%>%unique(),"PR","Alaska grid","California grid","Central East grid","Central Northeast grid",
                                       "Central Northwest grid", "Central Southwest grid","Florida grid","Hawaii grid",
                                       "Mid-Atlantic grid","New England grid","New York grid","Northwest grid",
                                       "Southeast grid","Southwest grid","Texas grid"),
                     paramsSelect = c("cerf"),
                     folder = paste0(data_folder,"/cerf_test"))


test_that("gcamextractor::read_gcam produces expected file contents for im3 gcam-usa cerf", {

    testthat::expect_identical(
      read.csv(paste0(data_folder,"/cerf_test/gcamDataTable.csv")),
      read.csv(paste0(data_folder,"/test_outputs/gcamDataTable.csv")),
      tolerance = 0.001)

  })


