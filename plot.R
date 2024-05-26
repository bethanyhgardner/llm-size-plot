# Setup----
library(tidyverse)  # general data wrangling
library(googlesheets4)  # download file from Google Sheets
library(janitor)  # convert column names to play nice with R

gs4_deauth()  # only accessing public files


# Data----
# Size data & sources are in anyone-with-the-link-can-view Google Sheet
data_url <- str_c(
  "https://docs.google.com/spreadsheets/d/",
  "1HGxA9URfEeUYp48qNXMMOPBAt3a18mSsdYqI5WbeE5w/edit#gid=0"
)

# Download it, drop source columns, and save offline copy as CSV
read_sheet(data_url, col_types = "ciTccccc---") |>  # first 8 columns only
  clean_names() |>  # convert column names to snake_case
  write_csv("data.csv")  # export
