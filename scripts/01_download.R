# download data

# pak::pak("wstolte/rwsapi")
require(rwsapi)
require(tidyverse)
require(sf)

# load csf# load catalogue with recent observations

latestObs <- readRDS("data/processed/metadata/all_latest_observations_wadar.rds")%>%
  rename_with(tolower)

latestObs_simpl <- rwsapi::simplify_df(latestObs %>% st_drop_geometry(), outputFormat = "attributed")

load("koppeling_meetpunten_rijkswateren.Rdata")
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
    compartiment.code == "OW",
    parameter.code %in% c('T', 'Cl', 'Ntot', 'GELDHD', 'Ptot', 'As', 'Cu', 'imdcpd', 'PCB101', 'Flu')
  ) %>%
  st_drop_geometry()

l2 <- rwsapi::rws_observation_queries(
  metadata = mycatalogue,
  start_date = as.Date("2020-01-01"),
  end_date = as.Date("2025-03-01")
)

observations <- lapply(l2, rws_observations)

obs_df <- dplyr::bind_rows(
  Filter(Negate(is.null),
         lapply(observations, `[[`, "content"))
)

obs_df %>% 
  left_join(
    koppeling_meetpunten_rijkswateren, 
    by = c(locatie.code = "locatie_code")) %>%
  filter(
  numeriekewaarde < 10000
) %>%
  mutate(tijdstip = lubridate::as_datetime(tijdstip)) %>%
  ggplot(aes(tijdstip, numeriekewaarde)) +
  geom_point(aes(color = watersysteem_id)) +
  theme(strip.text.x = element_text(angle = 45)) + 
  # geom_line(aes(color = watersysteem_id)) +
  facet_grid(parameter.code ~ locatie.code, scales = "free_y")
