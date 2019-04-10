# Read in Excel bike count and tabular weather data

library(readxl)
library(lubridate)
library(dplyr)
library(readr)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_names <- c("Hawthorne", "Tilikum", "Steel")

# define a function that loads excel sheets (ewwwwwww!)
load_data <- function(bridge_name, input_file) {
  bikecounts <- read_excel(input_file, sheet = bridge_name,
                           skip = 1) %>%
    filter(total > 0) %>%
    select(date, total) %>%
    mutate(bridge = bridge_name,
           date = as.Date(date)) # drops useless time
}

# h <- load_data("Broadway", input_file)

# load data from each sheet into a list
# then combine all three into one data frame
bikecounts <- lapply(bridge_names, load_data, 
                     input_file = input_file) %>%
  bind_rows()

# factor-ize bridge name, since here it makes sense to me
bikecounts <- bikecounts %>% mutate(bridge = factor(bridge))

# read in weather data
weather <- readr::read_csv("data/NCDC-CDO-USC00356750.csv")
