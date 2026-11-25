
library(tidyverse)
library(glue)
library(yaml)

source("R/minIO_helpers.R")
source("R/slugify.R")
source("R/build_report.R")
source("R/get_plot.R")
source("R/render_parameter.R")

testparams <- c('T', 'Cl', 'Ntot', 'GELDHD', 'Ptot', 'As', 'Cu', 'imdcpd', 'PCB101', 'Flu')

combitabel3 <- readRDS(
  "config/content_table.rds"
) %>%
  filter(parameter.code %in% testparams | grootheid.code %in% testparams)
  

watersystemen <- read_csv2(
  "config/watersysteem_yaml_koppeling.csv"
)

cfg <- read_yaml(
  "config/config_per_rw_active/markermeer.yml"
)

koppeling_meetpunten_rijkswateren <- read_delim("config/koppeling_meetpunten_rijkswateren.csv", 
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

build_report(
  watersysteem = "Markermeer",
  cfg = cfg,
  metadata = combitabel3,
  koppeling_meetpunten_rijkswateren = koppeling_meetpunten_rijkswateren,
  output_file = "reports/markermeer.qmd"
)


