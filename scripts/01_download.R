# download data

# pak::pak("wstolte/rwsapi")
require(rwsapi)
require(tidyverse)
require(sf)

# load csf# load catalogue with recent observations

latestObs <- readRDS("data/processed/metadata/all_latest_observations_wadar.rds")%>%
  rename_with(tolower)

latestObs_simpl <- rwsapi::simplify_df(latestObs %>% st_drop_geometry(), outputFormat = "attributed")

koppeling_meetpunten_rijkswateren <- read_delim("config/koppeling_meetpunten_rijkswateren.csv")

md <- rwsapi::rws_metadata()

catalogue <- md$content$locatielijst %>%
  full_join(md$content$aquometadatalocatielijst) %>%
  full_join(md$content$aquometadatalijst) %>% 
  filter(parameter_wat_omschrijving %in% latestObs$parameter_wat_omschrijving,
         code %in% latestObs$code
         ) %>%
  left_join(
    koppeling_meetpunten_rijkswateren, 
    by = c(
      code = "locatie_code"
    )) %>%
  mutate(                                 # maak parameter.code gelijk aan grootheid.code als parameter.code == NVT
    parameter.code = case_when(
      parameter.code == "NVT" ~ grootheid.code,
      .default = parameter.code
    )
  )

# make structure and loop for section criteria based on external table csv 
# select mycatalobue based on that
# download dta
# save data in appropriate location

mycatalogue <- catalogue %>%  
  filter(
    grepl("", watersysteem_id, ignore.case = T),
    watersysteem_id %in% c('Bergsche Maas', 'Haringvliet', 'Markermeer'),
    compartiment.code %in% c("OW", "ZS"),
    parameter.code %in% c('T', 'Cl', 'Ntot', 'GELDHD', 'Ptot', 'As', 'Cu', 'imdcpd', 'PCB101', 'Flu')
  ) %>%
  st_drop_geometry()

l2 <- rwsapi::rws_observation_queries(
  metadata = mycatalogue,
  start_date = as.Date("2010-01-01"),
  end_date = as.Date("2025-12-31")
)

observations <- lapply(l2, rws_observations)

obs_df <- dplyr::bind_rows(
  Filter(Negate(is.null),
         lapply(observations, `[[`, "content"))
)

## WEgschrijven als parquet naar p:\11202493--systeemrap-grevelingen\1_data\Rijkswateren\

require(arrow)
arrow::write_parquet(obs_df, sink = "p:\\11202493--systeemrap-grevelingen\\1_data\\Rijkswateren\\test_data_2025.parquet")
write_csv(mycatalogue, file = "p:\\11202493--systeemrap-grevelingen\\1_data\\Rijkswateren\\test_metadata_2025.csv")

