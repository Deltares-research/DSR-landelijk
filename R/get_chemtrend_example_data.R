
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
rijkstwateren_selectie <- filter(rijkstwateren, identificatie %in% c('Nederrijn', 'Amsterdam-Rijnkanaal', 'Markermeer'))

sql <-
  glue::glue_sql(
    'SELECT 
      metingen.meetpunt_code_nieuw, 
      metingen.meetpunt_code_oud,
      locatie.x_rd, 
      locatie.y_rd, 
      waterbeheerder.waterbeheerder_omschrijving,
      metingen.jaar,
      metingen.datum,
      metingen.tijd, 
      parameter.parameter_code,
      parameter.parameter_omschrijving,
      parameter."CAS",
      eenheid.eenheid_code,
      metingen.limietsymbool,
      metingen.waarden,
      hoedanigheid.hoedanigheid_code,
      compartiment.compartiment_code,
      metingen.grootheid, 
      metingen.kwaliteitsoordeel_id,
      metingen.uniek_dag, 
      metingen.uniek_tijd
    FROM metingen
    LEFT JOIN parameter ON parameter.parameter_id = metingen.parameter_id
    LEFT JOIN eenheid ON eenheid.eenheid_id = metingen.eenheid_id
    LEFT JOIN waterbeheerder ON waterbeheerder.waterbeheerder_id = metingen.waterbeheerder_id
    LEFT JOIN compartiment ON compartiment.compartiment_id = metingen.compartiment_id
    LEFT JOIN hoedanigheid ON hoedanigheid.hoedanigheid_id = metingen.hoedanigheid_id
    LEFt JOIN locatie ON locatie.meetpunt_id = metingen.meetpunt_id
    WHERE waterbeheerder.waterbeheerder_omschrijving = {wb} AND metingen.kwaliteitsoordeel_id NOT IN (99) AND st_intersects(ST_GeomFromText({shape}, 28992), locatie.geometry)',
    # subs = cfg$input_variables$param_interest,
    # min_year = cfg$input_variables$min_year,
    wb = 'Rijkswaterstaat',
    shape = st_as_text(st_union(rijkstwateren_selectie$geometry)), #  
    .con = con
  )

# query #
data <- as_tibble(st_read(con, query = sql))
locs <- distinct(data, meetpunt_code_nieuw, x_rd, y_rd) |>
    st_as_sf(coords = c('x_rd', 'y_rd'), crs = 28992) |>
    st_join(select(rijkstwateren_selectie, identificatie))
data <- left_join(data, select(st_drop_geometry(locs), meetpunt_code_nieuw, identificatie))


ggplot() + 
  geom_sf(data = rijkstwateren_selectie, aes(geometry = geometry, col  = identificatie)) + 
  geom_sf(data = locs, aes(geometry = geometry), col = 'red')

ggplot(filter(data, parameter_code %in% c('pH', 'T', 'ZICHT', 'CHLFa')), aes(x = datum, y = waarden, col = identificatie, group = meetpunt_code_nieuw)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~parameter_code, scales = 'free')
