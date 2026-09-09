# WFS inventarisatie


require(rwsapi)
require(lubridate)
require(tidyverse)
require(sf)
require(leaflet)

rws_wfsLatestObsMod <- function(
    parFilter = "Waterhoogte", 
    keepFromYear = 2025,
    outputFormat = "application/json"
    ){
  
  timeFilter = paste0("TIJDSTIP_LAATSTE_METING >= '", keepFromYear, "-01-01T00:00:00'")
  
  
  escape_cql <- function(x) {
    gsub("'", "''", x, fixed = TRUE)
  }
  
  parFilterEsc <- escape_cql(parFilter)
  
  url_list <- structure(
    list(
      scheme = "https",
      hostname = "geo.rijkswaterstaat.nl",
      port = NULL,
      path = "services/ogc/hws/DDAPI20/ows",
      
      query = list(
        SERVICE = "WFS",
        VERSION = "1.1.0",
        REQUEST = "GetFeature",
        TYPENAME = "locatiesmetlaatstewaarneming",
        CQL_FILTER = paste0(
          "PARAMETER_WAT_OMSCHRIJVING = '",
          parFilterEsc,
          "' AND TIJDSTIP_LAATSTE_METING >= '",
          keepFromYear,
          "-01-01T00:00:00'"
        ),
        outputFormat = outputFormat
      ),
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL),
    class = "url"
  )
  url <- httr::build_url(url_list)
  if(outputFormat == 'csv') {
    # cat(url)
    locs_with_latest_obs = readr::read_delim(
      url, 
      delim = ",",
      quote = "\"",
      col_types = cols(
        FID = col_character(),
        WAARNEMING_ID = col_character(),
        NAAM = col_character(),
        CODE = col_character(),
        OMSCHRIJVING = col_character(),
        STATUSWAARDE = col_character(),
        BEMONSTERINGSHOOGTE = col_number(),
        REFERENTIEVLAK = col_character(),
        OPDRACHTGEVENDE_INSTANTIE = col_character(),
        KWALITEITSWAARDE_CODE = col_character(),
        WAARDE_LAATSTE_METING = col_number(),
        TIJDSTIP_LAATSTE_METING = col_datetime(),
        PARAMETER_WAT_OMSCHRIJVING = col_character(),
        BEMONSTERINGSAPPARAATCODE = col_character(),
        BEMONSTERINGSMETHODECODE = col_character(),
        BEMONSTERINGSSOORTCODE = col_character(),
        BIOTAXONCODE = col_character(),
        BIOTAXONTYPE = col_character(),
        COMPARTIMENTCODE = col_character(),
        EENHEIDCODE = col_character(),
        GROOTHEIDCODE = col_character(),
        HOEDANIGHEIDCODE = col_character(),
        MEETAPPARAATCODE = col_character(),
        ORGAANCODE = col_character(),
        PARAMETERCODE = col_character(),
        TYPERINGCODE = col_character(),
        GROEPERINGCODE = col_character(),
        WAARDEBEPALINGSTECHNIEKCODE = col_character(),
        WAARDEBEPALINGSMETHODECODE = col_character(),
        WAARDEBEWERKINGSMETHODECODE = col_character(),
        GEOMETRY = col_character()
      ),
      show_col_types = FALSE
      )
  } else {
    locs_with_latest_obs = sf::st_read(url, quiet = TRUE)
  }
  return(locs_with_latest_obs)
}

safe_wfs <- function(par) {
  tryCatch(
    rws_wfsLatestObsMod(
      parFilter = par#,
      # outputFormat = "csv"
    ),
    error = function(e) {
      message("Failed: ", par)
      NULL
    }
  )
}


