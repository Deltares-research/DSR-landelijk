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

# saveRDS(allLatestObs, file = "data/processed/metadata/all_latest_observations_wadar.rds")

table(allLatestObs$COMPARTIMENTCODE)

allLatestObs |>
  sf::st_transform(4326) |>
  leaflet::leaflet() |>
  addTiles() |>
  addCircleMarkers(
    group = "meetpunten", 
    radius = 4,
    color = "green",
    label = ~paste(NAAM, PARAMETER_WAT_OMSCHRIJVING),
    clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
  )


## Intersection with waterlichamen/watersystemen Rijkswateren

# urls staan in variables.yml
variables <- RcppTOML::parseToml("config/variables.TOML") %>% list_flatten
variables$links_url_rijkswateren

rijkswateren <- st_read(variables$links_url_rijkswateren)
# rijkswateren %>%
#   st_transform(4326) %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolygons()

idx <- !duplicated(allLatestObs[c("NAAM", "CODE", "OMSCHRIJVING")])

latObsAct_sf_rw <- allLatestObs[idx, ] %>% st_transform(4326) %>%
  sf::st_intersection(rijkswateren %>% st_transform(4326)) %>%
  select(
    watersysteem_id = identificatie,
    locatie_naam = NAAM,
    locatie_code = CODE,
    locatie_omschrijving = OMSCHRIJVING
  )

write_delim(latObsAct_sf_rw, file = "config/koppeling_meetpunten_rijkswateren.csv", delim = ";")

# visualize coupling

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
    label = ~paste(locatie_naam),
    clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
  )
