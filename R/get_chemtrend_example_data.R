
# loading libraries ------------------------------------------------------
library(tidyverse)
library(sf)

# config -----------------------------------------------------------------
cfg <- config::get(file = 'config/variables.toml')

# read data --------------------------------------------------------------
rijkstwateren <- st_read('"https://geo.rijkswaterstaat.nl/services/ogc/gdr/nnn_begrenzing_rijkswateren/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=natuurnetwerk_nederland_begrenzing_rijkswateren&outputFormat=json&content-disposition=attachment"')
