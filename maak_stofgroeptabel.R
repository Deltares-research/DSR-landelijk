
require(tidyverse)

# read file with parameter groups
# the file originates from the "meetplan" of RWS
# filter out missing information rows

groeplijst <- readr::read_csv2("config/Parameter_groups2.csv")
groeplijst2 <- groeplijst %>% filter(!is.na(groep_))
groeplijst3 <- groeplijst %>% filter(!is.na(waarneminggroep))

# Check what is filtered out

groeplijst_verschil <- groeplijst2 %>% anti_join(groeplijst3)

# read WADAR metadata

md <- rwsapi::rws_metadata()
ddlparameters <- md$content$aquometadatalijst

# combineer groeplijst met ddlparameters

combitabel <- groeplijst3 %>% 
  select(
    waarneminggroep,
    Omschrijving,
    compartiment.code, 
    # paromshdh,
    ChemischeStof,
    monstercriterium
  ) %>%
  left_join(
    ddlparameters %>% 
      select(
        parameter_wat_omschrijving,
        grootheid.code,
        parameter.code,
        parameter.omschrijving,
        hoedanigheid.code, 
        compartiment.code,
        eenheid.code
      ),
    by = c(
      Omschrijving = "parameter.omschrijving",
      compartiment.code = "compartiment.code"
    )
  ) %>% 
  filter(
    !is.na(parameter_wat_omschrijving)
  )

# lees lijst met combinaties locaties en parameters in waar nog recent gerapporteerd is

recent_observations <- read_rds("data\\processed\\metadata\\all_latest_observations_wadar.rds")
recent_parameters <- recent_observations %>% 
  sf::st_drop_geometry() %>%
  select(CODE, PARAMETER_WAT_OMSCHRIJVING) %>% 
  nest(locaties = CODE,)

# combineer gecombineerde lijst met recente waarnemingen
# resultaat: lijst van parameters en groepen en een listkolom met locaties waarvoor recente waarnemingen zijn.

combitabel2 <- combitabel %>%
  select(
    waarneminggroep,
    parameter_wat_omschrijving,
    grootheid.code,
    parameter.code,
    hoedanigheid.code, 
    compartiment.code,
    eenheid.code
  ) %>%
  right_join(
    recent_parameters, 
    by = c(
      parameter_wat_omschrijving = "PARAMETER_WAT_OMSCHRIJVING"
    )
  ) %>%
  filter(!is.na(waarneminggroep))

str(combitabel2, max.level = 2)

contentTable = tribble(
  ~volgorde, ~type, ~url, ~subtype, ~caption,
  1, "plot", "", "Lowess", "Verloop van {parameter_rapport} in de tijd",
  2, "plot", "", "Sen", "Theil-Sen Trend voor  {parameter_rapport} over de gehele meetserie",
  3, "tekst", "", "intro", "Toestanden en trends voor {parameter_rapport} gebaseerd op gegevens van Rijkswaterstaat (Wadar). ",
  4, "tekst", "", "beschrijving", "Later in te vullen"
)

library(dplyr)
library(purrr)
library(glue)

combitabel3 <- combitabel2 %>%
  mutate(
    content = map(
      seq_len(n()),
      ~ contentTable
    )
  )

saveRDS(combitabel3, "config/content_table.rds")
