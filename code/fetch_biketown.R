# Functions to fetch public Biketown trip data
# https://www.biketownpdx.com/system-data
remove.packages("pacman")
# pacman allows checking for and installing missing packages
if (!require("pacman")) {install.packages("pacman")}; library(pacman)
pacman::p_load("lubridate")
pacman::p_load("dplyr")
pacman::p_load("stringr")
pacman::p_load("readr")

# another way to do same thing using base R
#pkgs <- c("lubridate", "dplyr", "stringr", "readr")
#install.packages(pkgs)

get_data <- function(start="7/2016", end=NULL,
                     base_url="https://s3.amazonaws.com/biketown-tripdata-public/",
                     outdir="data/biketown/") {
  # takes start and end in mm/yyyy format, and tries to download files
  
  # if no end date given, set to now
  end <- ifelse(is.null(end), format(now(), "%m/%Y"), end)

  # make url function only available within get_data
  make_url <- function(date) {
    url <- paste0(base_url, format(date, "%Y_%m"), ".csv")
    return(url)
  }
  
  # parse date range
  start_date <- lubridate::myd(start, truncated = 2)
  end_date <- myd(end, truncated = 2)
  date_range <- seq(start_date, end_date, by="months")
  
  # 3 ways to the same end...
  
  # lapply(a, b) just applies function b to sequence a 
  # and returns a list of the modified sequence
  # urls <- lapply(date_range, make_url)
  
  # 1) for loop over named list of urls
  # for loops can be easier for early development of code
  # for (u in urls) {
  #   download.file(u, destfile = paste0(outdir, 
  #                                      str_sub(u, -11)))
  # }
  
  # 2) as an apply with an in-line function 
  # result <- lapply(urls, function (u) {
  #   download.file(u, destfile = paste0(outdir, str_sub(u, -11)))
  # })
  
  # 3) tidy piped version that combines url generation & download
  lapply(date_range, make_url) %>%
    lapply(function(u) {download.file(u, destfile = paste0(outdir, 
                                                            str_sub(u,-11)))})
}

### Manual Run ###
# params
# start = "11/2018"
# end = "12/2018"
# 
# get_data(start, end)
