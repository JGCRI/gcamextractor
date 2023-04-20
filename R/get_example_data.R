#' get_example_data
#'
#' Function to download the test data files from zenodo that are needed.
#'
#' @param write_dir Default = getwd()
#' @param dir_name = "gcamextractor_example_data". Name of directory to install zip file to.
#' @param data_link Default = NULL
#' @keywords test
#' @return number
#' @importFrom rlang :=
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' library(osiris)
#' osiris::get_example_data()
#' }

get_example_data <- function(write_dir = getwd(),
                             dir_name = "gcamextractor_example_data",
                             data_link = NULL) {
  #.........................
  # Initialize
  #.........................

  rlang::inform("Starting get_example_data")

  if (!is.null(data_link)) {
    # Download zip file from zenodo
    rlang::inform("Starting download...")

    if (!file.exists(paste0(write_dir, "/", dir_name, ".zip"))) {
      options(timeout = max(3600, getOption("timeout")))

      utils::download.file(
        url = data_link,
        destfile = paste0(write_dir, "/", dir_name, ".zip"),
        mode = "wb"
      )

      rlang::inform("Download complete.")

      # Download zip file from zenodo
      rlang::inform("Starting unzip...")

      utils::unzip(paste0(write_dir, "/", dir_name, ".zip"),
                   exdir = paste0(write_dir, "/", dir_name))

      unlink(paste0(write_dir, "/", dir_name, ".zip"))

      rlang::inform("Unzip complete.")
    } else {
      rlang::inform(
        paste0(
          "Skipping download since file lready exists:", write_dir, "/", dir_name, ".zip"
        )
      )
      rlang::inform("Delete file to re-download if desired.")
    }

  } else {
    rlang::inform("No data_link provided. Skipping get_example_data.")
  }

  #.........................
  # Close Out
  #.........................

  rlang::inform("get_example_data completed.")

  return(paste0(write_dir, "/", dir_name))
}
