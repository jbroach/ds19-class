# A few changes for 2019 DS course edition

library(readxl)
library(lubridate)
library(tidyverse)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_names <- c("Hawthorne", "Tilikum", "Steel")

# define a funtion that loads bike counts from Excel Sheets (ewwwww!)
load_data <- function(bridge_name, input_file) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1) %>%
    filter(total > 0) %>%
    select(date, total) %>%
    mutate(bridge = bridge_name, date = as.Date(date)) # as.Date drops useless time
}

# load data from each bridge into a list of data frames
# then combine all three data frames
bikecounts <- lapply(bridge_names, load_data, input_file = input_file) %>%
  bind_rows()

# factor-ize bridge name, since here it makes sense
bikecounts <- bikecounts %>% mutate(bridge = factor(bridge))



# read from csv
weather <- read_csv("data/NCDC-CDO-USC00356750.csv")

# read from fwf
#weather_fwf <- read_table("data/NCDC-CDO-USC00356750.txt", comment="--")
