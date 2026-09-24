# download data

# pak::pak("wstolte/rwsapi")
require(rwsapi)
require(tidyverse)
require(sf)

# load csf# load catalogue with recent observations

latestObs <- readRDS("data/processed/metadata/all_latest_observations_wadar.rds")%>%
  rename_with(tolower)

latestObs_simpl <- rwsapi::simplify_df(latestObs %>% st_drop_geometry(), outputFormat = "attributed")

load("data/processed/mappings/koppeling_meetpunten_rijkswateren.Rdata")

md <- rwsapi::rws_metadata()

catalogue <- md$content$locatielijst %>%
  full_join(md$content$aquometadatalocatielijst) %>%
  full_join(md$content$aquometadatalijst) %>% 
  filter(parameter_wat_omschrijving %in% latestObs$parameter_wat_omschrijving,
         code %in% latestObs$code
         ) %>%
  left_join(
    latObsAct_sf_rw %>% st_drop_geometry(), 
    by = c(
      code = "locatie_code"
    )) 

# make structure and loop for section criteria based on external table csv 
# select mycatalobue based on that
# download dta
# save data in appropriate location

mycatalogue <- catalogue %>%  
  filter(
    grepl("noordzee", watersysteemid, ignore.case = T),
    grepl("terschelling", code, ignore.case = T),
    compartiment.code == "OW",
    parameter.code == "NO3"
  ) %>%
  st_drop_geometry()

l2 <- rwsapi::rws_observation_queries(
  metadata = mycatalogue[1:5,],
  start_date = as.Date("2020-01-01"),
  end_date = as.Date("2025-03-01")
)

observations <- lapply(l2, rws_observations)

obs_df <- dplyr::bind_rows(
  Filter(Negate(is.null),
         lapply(observations, `[[`, "content"))
)

obs_df %>% 
  filter(
  numeriekewaarde < 10000
) %>%
  mutate(tijdstip = lubridate::as_datetime(tijdstip)) %>%
  ggplot(aes(tijdstip, numeriekewaarde)) +
  geom_point(aes(color = locatie.code)) +
  geom_line(aes(color = locatie.code))
