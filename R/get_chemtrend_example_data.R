
# loading libraries ------------------------------------------------------
library(tidyverse)
library(sf)
library(RPostgreSQL)

# config -----------------------------------------------------------------
variables <- RcppTOML::parseToml("config/variables.TOML") 

# connect db
# connect with Deltares WKP database
drv = dbDriver(variables$database$driver)
con <- DBI::dbConnect(
  drv,
  dbname = "waterkwaliteit_test", # "waterkwaliteit_test"
  host = variables$database$server,
  port = variables$database$port,
  user = rstudioapi::askForPassword("database_userid"),
  password = rstudioapi::askForPassword("Database password")
)

# read data --------------------------------------------------------------
rijkstwateren <- st_read(variables$links$url_rijkswateren)
