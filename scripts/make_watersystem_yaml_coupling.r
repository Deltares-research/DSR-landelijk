
library(readr)
library(dplyr)
source("R/slugify.R")

watersystemen <- read_delim(
  "config/koppeling_meetpunten_rijkswateren.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
) %>%
  distinct(watersysteem_id) %>%
  rename(
    watersysteem.naam = watersysteem_id
  ) %>%
  mutate(
    yaml_file = paste0(
      slugify(watersysteem.naam),
      ".yml"
    ),
    report_dir = slugify(watersysteem.naam)
  )

write_csv2(watersystemen, "config/watersysteem_yaml_koppeling.csv")
