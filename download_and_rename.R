library(httr)
library(lubridate)

url <- "https://donnees.electionsquebec.qc.ca/production/provincial/financement/contribution/contributions-pro-fr.csv"
dest_file <- paste0("data/", Sys.Date(), "contributions-pro-fr.csv")

response <- GET(url, write_disk(dest_file, overwrite = TRUE), progress())


if (http_status(response)$category == "Success") {
  cat("File downloaded and saved as", dest_file, "\n")
} else {
  cat("Error downloading the file. Please try again later.\n")
}
