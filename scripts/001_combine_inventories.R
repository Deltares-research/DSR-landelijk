source("r/inventory.R")
library(sf)

targetparam_names = c("OW", "ZS", "BS", "OR")

# combine lists
allLatestObs <- map(
  targetparam_names,
  \(x){
    temp <- readRDS(paste0("data/raw/metadata/latestobservations_", x, "_sf.rds")) |>
      purrr::compact() |> 
      keep(\(x) nrow(x) > 0)
    
    template <- temp[[1]]
    temp2 <- map(
      temp,
      ~ mutate(.x,
               across(
                 names(st_drop_geometry(template)),
                 ~ as(.x, class(template[[cur_column()]])[1])
               )
      )
    )
    result <- bind_rows(temp2)
  }
) |>
  bind_rows()

saveRDS(allLatestObs, file = "data/processed/metadata/all_latest_observations_wadar.rds")

allLatestObs |>
  sf::st_transform(4326) |>
  leaflet::leaflet() |>
  addTiles() |>
  addCircleMarkers(
    group = "meetpunten", 
    radius = 4,
    color = "green",
    label = ~paste(NAAM, PARAMETER_WAT_OMSCHRIJVING, year(TIJDSTIP_LAATSTE_METING)),
    clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
  )


## Intersection with waterlichamen/watersystemen Rijkswateren

# urls staan in variables.yml
config <- RcppTOML::parseToml("_variables.TOML") %>% list_flatten
config$links_url_rijkswateren

rijkswateren <- st_read(config$links_url_rijkswateren)
# rijkswateren %>%
#   st_transform(4326) %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons()

waterbodies <- st_read(config$links_url_oppervlaktewaterlichamen)
# waterbodies %>%
#   st_transform(4326) %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons()

latObsAct_sf_rw <- allLatestObs  %>% st_transform(4326) %>%
  sf::st_intersection(rijkswateren %>% st_transform(4326)) %>%
  select(
    locatie_id = id,
    watersysteemid = identificatie,
    locatie_naam = NAAM,
    locatie_code = CODE,
    locatie_omschrijving = OMSCHRIJVING,
    rw_identificatie = identificatie,
    PARAMETER_WAT_OMSCHRIJVING,
    TIJDSTIP_LAATSTE_METING
  )

save(latObsAct_sf_rw, file = "koppeling_meetpunten_rijkswateren.Rdata")
# we hebben vast niet alle kolommen nodig - denk na over weg te gooien kolommen.

# selectere per watersysteem kan dan bijv zo

latObsAct_waddenzee_wh <- latObsAct_sf_rw %>% 
  filter(grepl("Waddenzee", watersysteemid, ignore.case = T) | grepl("Eems", watersysteemid, ignore.case = T)) %>%
  filter(grepl("waterhoogte", PARAMETER_WAT_OMSCHRIJVING, ignore.case = T))
write_csv(latObsAct_waddenzee_wh, "data/interim/md_ws_par/waddenzee_waterhoogtegemeten.csv")

latObsAct_wh <- latObsAct_sf_rw %>% 
  filter(grepl("waterhoogte", PARAMETER_WAT_OMSCHRIJVING, ignore.case = T))
write_csv(latObsAct_wh, "data/interim/md_ws_par/waterhoogtegemeten.csv")


# continue processing and checks

load("koppeling_meetpunten_rijkswateren.Rdata")

latObsAct_sf_rw %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = rijkswateren %>% st_transform(4326),
    label = ~identificatie
  ) %>%
  addCircleMarkers(
    group = "meetpunten", 
    radius = 4,
    color = "green",
    label = ~paste(locatie_naam, PARAMETER_WAT_OMSCHRIJVING, year(TIJDSTIP_LAATSTE_METING)),
    clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
  )
