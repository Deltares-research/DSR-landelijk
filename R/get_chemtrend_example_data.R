
# loading libraries ------------------------------------------------------
library(tidyverse)
library(sf)
library(RPostgreSQL)

# config -----------------------------------------------------------------
variables <- RcppTOML::parseToml("config/variables.TOML") 
pars <- aquodom::dom('parameter')
grthd <- filter(pars, groep %in% c('Object', 'Typering', 'Grootheid'))
chem_stof <- filter(pars, groep %in% c('ChemischeStof'))
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
rijkstwateren_selectie <- filter(rijkstwateren, identificatie %in% c('Bergsche Maas', 'Haringvliet', 'Markermeer'))

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
pars <- c('T', 'Cl', 'Ntot', 'GELDHD', 'Ptot', 'As', 'Cu', 'imdcpd', 'PCB101', 'Flu')

# fix missing grootheden
jn_grhd <- filter(data, !is.na(grootheid)) |>
  distinct(parameter_code, eenheid_code, hoedanigheid_code, grootheid)
parm_grthd <- distinct(data, parameter_code, grootheid) |>
  filter(parameter_code == grootheid)

data <- rows_patch(
  data,
  jn_grhd,
  by = c('parameter_code', 'eenheid_code', 'hoedanigheid_code'),
  unmatched = 'ignore'
) |>
  rows_patch(
    distinct(jn_grhd, parameter_code, eenheid_code, grootheid),
    by = c('parameter_code', 'eenheid_code'),
    unmatched = 'ignore'
  ) |>
  mutate(
    grootheid = case_when(
      parameter_code %in% grthd$codes ~ parameter_code,
      is.na(grootheid) &
        parameter_code == 'CHLFa' &
        eenheid_code == '%' ~ 'VOLMFTE',
      is.na(grootheid) & eenheid_code == 'mBq/l' ~ 'ACTVTCCTTE',
      is.na(grootheid) & eenheid_code %in% c('mg/kg', 'umol/kg') ~ 'MASSFTE',
      is.na(grootheid) &
        eenheid_code %in% c('mg/l', 'ug/l') &
        parameter_code %in% chem_stof$codes ~ 'CONCTTE',
      is.na(grootheid) & eenheid_code %in% c('mg/l', 'ug/l') ~ 'CONCTTE',
      .default = grootheid
    ),
    wns = glue::glue('{parameter_code}_{eenheid_code}_{hoedanigheid_code}_{grootheid}_{compartiment_code}')
  )

ggplot() + 
  geom_sf(data = rijkstwateren_selectie, aes(geometry = geometry, col  = identificatie)) + 
  geom_sf(data = filter(locs, identificatie %in% rijkstwateren_selectie$identificatie), aes(geometry = geometry), col = 'red')

ggplot(filter(data, parameter_code %in% pars, identificatie %in% rijkstwateren_selectie$identificatie), aes(x = datum, y = waarden, col = identificatie, group = meetpunt_code_nieuw)) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~parameter_code, scales = 'free')


# write data to disk -----------------------------------------------------
qs2::qs_save(data, "data/interim/data_rijkswateren.qs2")
qs2::qs_save(data_trend, "data/interim/data_trends_rijkswateren.qs2")


## minIO tetst
Sys.setenv("AWS_ACCESS_KEY_ID" = "CGklAIqR8ayasqPDQwG9", # enter your credentials
       "AWS_SECRET_ACCESS_KEY" = "MeGIzB7NiWVRMNgteSeiNDDCL6CHwPCJ44A5aDu6", # enter your credentials
       "AWS_DEFAULT_REGION" = "eu-west-1",
       "AWS_S3_ENDPOINT" = "s3.deltares.nl:443")    # change it to your specific IP and port



get_bucket(
  bucket = "dsr",
  region = "eu-west-1",
  url_style = "path",
  use_https = TRUE,
  max = 5
)


s3 <- s3(
  config = list(
    credentials = list(
      creds = list(
        access_key_id = "CGklAIqR8ayasqPDQwG9",
        secret_access_key = "MeGIzB7NiWVRMNgteSeiNDDCL6CHwPCJ44A5aDu6"
      )
    ),
    endpoint =  "https://s3.deltares.nl",
    region = "eu-west-1",
    s3_force_path_style = TRUE
  )
)

tf <- tempfile(fileext = ".csv")
write.csv(mtcars, tf, row.names = FALSE)

s3$put_object(
  Bucket = "dsr",
  Key = "r-outputs/mtcars.csv",
  Body = tf
)
unlink(tf)
