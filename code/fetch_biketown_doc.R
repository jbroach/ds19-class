# Load public biketown trip data by date range
# see: https://www.biketownpdx.com/system-data

# pacman allows checking for and loading packages in short line of code
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
#pacman::p_load("tidyverse")
pacman::p_load("lubridate")
pacman::p_load("dplyr")
pacman::p_load("stringr")
pacman::p_load("readr")

get_data <- function(start, end=NA, 
                     base_url="https://s3.amazonaws.com/biketown-tripdata-public/",
                     outdir="data/biketown") {
  # NOte takes dates as month/year
  # if no end date given, read to present time
  end <- ifelse(is.na(end), format(now(), "%m/%Y"), end)
  print(end)
  # make_url function availavble only within get_data function
  make_url <- function(date, base_url=base_url) {
    paste0(base_url, format(date, "%Y_%m"), ".csv")
  }
  # parse date range
  start_date <- lubridate::myd(start, truncated=2)
  end_date <- lubridate::myd(end, truncated=2)
  date_range <- seq(start_date, end_date, by='months')
  
  # lapply(a, b) just applies function b to sequence a  
  urls <- lapply(date_range, make_url, base_url = base_url)
  
  # for loops can be easier to write but are slower for long loops
  #for (u in urls) {
  #  download.file(u, destfile = paste0(outdir, str_sub(u,-11))) 
  #}
  #lapply(urls, 
  #       function(u) {download.file(u, 
  #                                  destfile = paste0(outdir, str_sub(u,-11)))})
  lapply(date_range, make_url, base_url = base_url) %>%
    lapply(function(u) {try(download.file(u, 
                                      destfile = paste0(outdir, 
                                                        str_sub(u,-11))))})
}

### Manual Run ###
# Params
start = "9/2018"
end = "9/2018"
outdir = "data/biketown/"
get_data(start, end, 
         outdir=outdir)
get_data(start, 
         outdir=outdir)

# Stitch monthly files together for analysis
biketown_all <- paste0(outdir, list.files(outdir)) %>%
  lapply(read.csv, stringsAsFactors = F) %>%
  bind_rows() 

# Or, write out stitched file
write.csv(biketown_all, "biketown_all.csv")
