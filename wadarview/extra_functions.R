
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



rws_observation_query <- function(
    metadata,
    start_date,
    end_date,
    filters = c("compartiment", "grootheid", "parameter", "hoedanigheid", "eenheid", "orgaan", "typering", "biotaxon", "groepering", "procestype"),
    tz = "Europe/Amsterdam"
) {
  
  stopifnot(is.data.frame(metadata))
  
  if (nrow(metadata) != 1) {
    stop(
      "metadata must contain exactly one row. ",
      "Use rws_observation_queries() for multiple rows."
    )
  }
  
  if (!inherits(start_date, "Date")) {
    stop("start_date must be a Date object")
  }
  
  if (!inherits(end_date, "Date")) {
    stop("end_date must be a Date object")
  }
  
  field_map <- list(
    compartiment = list(column = "compartiment.code", api_name = "Compartiment", value_name = "Code"),
    grootheid    = list(column = "grootheid.code",    api_name = "Grootheid",    value_name = "Code"),
    parameter    = list(column = "parameter.code",    api_name = "Parameter",    value_name = "Code"),
    hoedanigheid = list(column = "hoedanigheid.code", api_name = "Hoedanigheid", value_name = "Code"),
    eenheid      = list(column = "eenheid.code",      api_name = "Eenheid",      value_name = "Code"),
    orgaan       = list(column = "orgaan.code",       api_name = "Orgaan",       value_name = "Code"),
    typering     = list(column = "typering.code",     api_name = "Typering",     value_name = "Code"),
    biotaxon     = list(column = "biotaxon.code",     api_name = "Biotaxon",     value_name = "Code"),
    groepering   = list(column = "groepering.code",   api_name = "Groepering",   value_name = "Code"),
    procestype   = list(column = "procestype",        api_name = "ProcesType",   value_name = NULL)
  )
  
  if (identical(filters, "all")) {
    filters <- names(field_map)
  }
  
  aquo_metadata <- list()
  
  for (field in filters) {
    
    if (!field %in% names(field_map)) {
      warning(sprintf("Unknown filter ignored: %s", field))
      next
    }
    
    mapping <- field_map[[field]]
    
    col_name <- mapping$column
    
    if (!col_name %in% names(metadata)) {
      next
    }
    
    value <- metadata[[col_name]][1]
    
    if (is.na(value) || value %in% c("", "NVT")) {
      next
    }
    
    if (is.null(mapping$value_name)) {
      
      aquo_metadata[[mapping$api_name]] <- value
      
    } else {
      
      aquo_metadata[[mapping$api_name]] <- setNames(
        list(value),
        mapping$value_name
      )
      
    }
  }
  
  start_datetime <- format(
    as.POSIXct(start_date, tz = tz),
    "%Y-%m-%dT00:00:00.000%z"
  )
  
  end_datetime <- format(
    as.POSIXct(end_date, tz = tz),
    "%Y-%m-%dT23:59:59.999%z"
  )
  
  start_datetime <- sub(
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2",
    start_datetime
  )
  
  end_datetime <- sub(
    "([+-][0-9]{2})([0-9]{2})$",
    "\\1:\\2",
    end_datetime
  )
  
  list(
    AquoPlusWaarnemingMetadata = list(
      AquoMetadata = aquo_metadata
    ),
    Locatie = list(
      Code = metadata$code[1]
    ),
    Periode = list(
      Begindatumtijd = start_datetime,
      Einddatumtijd = end_datetime
    )
  )
}


rws_observation_queries <- function(
    metadata,
    start_date,
    end_date,
    filters = c(
      "compartiment",
      "grootheid",
      "parameter",
      "hoedanigheid",
      "eenheid",
      "orgaan",
      "typering",
      "biotaxon",
      "groepering",
      "procestype"
    ),
    tz = "Europe/Amsterdam"
) {
  
  stopifnot(is.data.frame(metadata))
  
  n <- nrow(metadata)
  
  if (n == 0) {
    stop("metadata contains no rows")
  }
  
  if (n > 1) {
    warning(sprintf(
      "metadata contains %s rows; returning a list of %s queries.",
      n,
      n
    ))
  }
  
  lapply(
    seq_len(n),
    function(i) {
      rws_observation_query(
        metadata = metadata[i, , drop = FALSE],
        start_date = start_date,
        end_date = end_date,
        filters = filters,
        tz = tz
      )
    }
  )
}

rws_metadata <- function(
    filterList = list(
      Compartimenten = T,
      Grootheden = T,
      Parameters = T,
      Eenheden = T,
      Hoedanigheden = T,
      Typeringen = T,
      BioTaxon = T,
      Organen = T,
      WaardeBewerkingsMethoden = T,
      ProcesTypes = T,
      Groeperingen = T
    )
) {
  
  # Helper function to translate list names to lower case when names are present
  # This strategy avoids issues due to inconsistent or altered naming conventions in terms of upper- and lower-case use.
  convert_names_to_lowercase <- function(lst) {
    # If the element is a list
    if (is.list(lst)) {
      # Convert names to lowercase
      if(!is.null(names(lst))) names(lst) <- tolower(names(lst))
      
      # Recursively apply to each element in the list
      lst <- purrr::map(lst, convert_names_to_lowercase)
      
    }
    return(lst)
  }
  
  ua <- httr::user_agent("https://github.com/wstolte/rwsapi")
  path <- "/METADATASERVICES/OphalenCatalogus"
  url <- paste0("https://ddapi20-waterwebservices.rijkswaterstaat.nl", path)
  
  l = list(CatalogusFilter=filterList)
  
  resp <- httr::POST(url, body = l, encode = "json")
  
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return application/json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = TRUE,
    flatten = TRUE
  )
  
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "RWS API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  res <- structure(
    list(
      content = convert_names_to_lowercase(parsed),
      path = path,
      response = resp
    )#,
    # class = "rws_api"
  )
  
  res$content$aquometadatalijst <- as.data.frame(res$content$aquometadatalijst)
  res$content$aquometadatalocatielijst <- as.data.frame(res$content$aquometadatalocatielijst)
  res$content$locatielijst <- as.data.frame(res$content$locatielijst)
  
  return(res)
}


rws_observations <- function (bodylist, trytimes = 3) {
  
  nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
  
  # Helper function to translate list names to lower case when names are present
  # This strategy avoids issues due to inconsistent or altered naming conventions in terms of upper- and lower-case use.
  convert_names_to_lowercase <- function(lst) {
    # If the element is a list
    if (is.list(lst)) {
      # Convert names to lowercase
      if(!is.null(names(lst))) names(lst) <- tolower(names(lst))
      
      # Recursively apply to each element in the list
      lst <- purrr::map(lst, convert_names_to_lowercase)
    }
    return(lst)
  }
  
  warnings = list()
  
  ua <- httr::user_agent("https://github.com/wstolte/rwsapi")
  path <- "/ONLINEWAARNEMINGENSERVICES/OphalenWaarnemingen"
  url <- paste0("https://ddapi20-waterwebservices.rijkswaterstaat.nl", path)
  
  resp <- httr::RETRY(verb = "POST",
                      url = url,
                      ua = ua,
                      body = jsonlite::toJSON(bodylist,
                                              auto_unbox = T,
                                              digits = NA
                      ),
                      httr::add_headers(.headers = c(`Content-Type` = "application/json",
                                                     `Ocp-Apim-Subscription-Key` = "my_subscription_key"
                      )
                      ),
                      times = trytimes
  )
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return application/json", call. = FALSE)
  }
  
  if(resp$status_code != 204) {
    response <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),simplifyVector = FALSE)
    #    parsed   <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"),simplifyVector = TRUE,flatten = T)
  } else {
    empty_content <- data.frame(
      locatie.message.id                   = c(), locatie.code                 = c(), locatie.naam                        = c(), coordinatenstelsel           = c(),
      geometriepunt.x                      = c(), geometriepunt.y              = c(), tijdstip                            = c(), statuswaarde                 = c(),
      bemonsteringshoogte                  = c(), referentievlak               = c(), opdrachtgevendeinstantie            = c(), kwaliteitswaarde.code        = c(),
      parameter.wat.omschrijving           = c(), bemonsteringsapparaat.code   = c(), bemonsteringsapparaat.omschrijving  = c(), bemonsteringssoort.code      = c(),
      bemonsteringssoort.omschrijving      = c(), biotaxon.code                = c(), biotaxon.omschrijving               = c(), biotaxontype.code            = c(),
      biotaxontype.omschrijving            = c(), compartiment.code            = c(), compartiment.omschrijving           = c(), eenheid.code                 = c(),
      eenheid.omschrijving                 = c(), grootheid.code               = c(), grootheid.omschrijving              = c(), typering.code                = c(),
      typering.omschrijving                = c(), hoedanigheid.code            = c(), hoedanigheid.omschrijving           = c(), meetapparaat.code            = c(),
      meetapparaat.omschrijving            = c(), orgaan.code                  = c(), orgaan.omschrijving                 = c(), parameter.code               = c(),
      parameter.omschrijving               = c(), groepering.code              = c(), groepering.omschrijving             = c(), waardebepalingstechniek.code = c(),
      waardebepalingstechniek.omschrijving = c(), waardebepalingsmethode.code  = c(), waardebepalingsmethode.omschrijving = c(), waardebewerkingsmethode.code = c(),
      waardebewerkingsmethode.omschrijving = c(), numeriekewaarde              = c(), alphanumeriekewaarde                = c()
    )
    
    
    return(structure(list(content = empty_content, path = path, response = resp)))
  }
  
  if (!response$Succesvol) {
    paste("request not successful", response$Foutmelding)
  }
  
  response <- convert_names_to_lowercase(response)
  
  df <- data.frame()
  for (ii in seq(1:length(response$waarnemingenlijst))) {
    print(paste("ii in response.waarnemingenlijst: ", ii))
    if (purrr::is_empty(response$waarnemingenlijst)) {
      next
    }
    if (!purrr::is_empty(purrr::map_dbl(response$waarnemingenlijst[[ii]]$metingenlijst,list("meetwaarde", "waarde_numeriek"), .default = NA))) {
      temp.l <- list(locatie.message.id          = response$waarnemingenlijst[[ii]]$locatie$locatie_messageid,
                     locatie.code                          = response$waarnemingenlijst[[ii]]$locatie$code,
                     locatie.naam                          = response$waarnemingenlijst[[ii]]$locatie$naam,
                     coordinatenstelsel                    = response$waarnemingenlijst[[ii]]$locatie$coordinatenstelsel,
                     geometriepunt.x                       = response$waarnemingenlijst[[ii]]$locatie$lon,
                     geometriepunt.y                       = response$waarnemingenlijst[[ii]]$locatie$lat,
                     tijdstip                              = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("tijdstip"), .default = NA),                                         # using names as accessors since indices are more difficult to debug when changed on the server
                     statuswaarde                          = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","statuswaarde"), .default = NA),
                     bemonsteringshoogte                   = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","bemonsteringshoogte"), .default = NA),
                     referentievlak                        = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","referentievlak"), .default = NA),
                     opdrachtgevendeinstantie              = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","opdrachtgevendeinstantie"), .default = NA),
                     kwaliteitswaarde.code                 = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("waarnemingmetadata","kwaliteitswaardecode"), .default = NA),
                     #                aquometadata.message.id = response$waarnemingenlijst[[ii]]$aquometadata$aquometadata_MessageID,
                     parameter.wat.omschrijving            = response$waarnemingenlijst[[ii]]$aquometadata$parameter_wat_omschrijving,
                     procestype                            = response$waarnemingenlijst[[ii]]$aquometadata$procestype,
                     bemonsteringsapparaat.code            = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringsapparaat$code,
                     bemonsteringsapparaat.omschrijving    = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringsapparaat$omschrijving,
                     bemonsteringssoort.code               = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringssoort$code,
                     bemonsteringssoort.omschrijving       = response$waarnemingenlijst[[ii]]$aquometadata$bemonsteringssoort$omschrijving,
                     biotaxon.code                         = response$waarnemingenlijst[[ii]]$aquometadata$biotaxon$code,
                     biotaxon.omschrijving                 = response$waarnemingenlijst[[ii]]$aquometadata$biotaxon$omschrijving,
                     biotaxontype.code                     = response$waarnemingenlijst[[ii]]$aquometadata$biotaxontype$code,
                     biotaxontype.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$biotaxontype$omschrijving,
                     #                biotaxoncompartiment.code = response$waarnemingenlijst[[ii]]$aquometadata$BioTaxon_compartiment$code,                                    # no longer present in API response
                     #                biotaxoncompartiment.omschrijving = response$waarnemingenlijst[[ii]]$aquometadata$BioTaxon_compartiment$omschrijving,                    # no longer present in API response
                     compartiment.code                     = response$waarnemingenlijst[[ii]]$aquometadata$compartiment$code,
                     compartiment.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$compartiment$omschrijving,
                     eenheid.code                          = response$waarnemingenlijst[[ii]]$aquometadata$eenheid$code,
                     eenheid.omschrijving                  = response$waarnemingenlijst[[ii]]$aquometadata$eenheid$omschrijving,
                     grootheid.code                        = response$waarnemingenlijst[[ii]]$aquometadata$grootheid$code,
                     grootheid.omschrijving                = response$waarnemingenlijst[[ii]]$aquometadata$grootheid$omschrijving,
                     typering.code                         = response$waarnemingenlijst[[ii]]$aquometadata$typering$code,
                     typering.omschrijving                 = response$waarnemingenlijst[[ii]]$aquometadata$typering$omschrijving,
                     hoedanigheid.code                     = response$waarnemingenlijst[[ii]]$aquometadata$hoedanigheid$code,
                     hoedanigheid.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$hoedanigheid$omschrijving,
                     meetapparaat.code                     = response$waarnemingenlijst[[ii]]$aquometadata$meetapparaat$code,
                     meetapparaat.omschrijving             = response$waarnemingenlijst[[ii]]$aquometadata$meetapparaat$omschrijving,
                     #                monsterbewerkingsmethode.code         = response$waarnemingenlijst[[ii]]$aquometadata$MonsterBewerkingsMethode$code,                    # no longer present in API response
                     #                monsterbewerkingsmethode.omschrijving = response$waarnemingenlijst[[ii]]$aquometadata$MonsterBewerkingsMethode$omschrijving,            # no longer present in API response
                     orgaan.code                           = response$waarnemingenlijst[[ii]]$aquometadata$orgaan$code,
                     orgaan.omschrijving                   = response$waarnemingenlijst[[ii]]$aquometadata$orgaan$omschrijving,
                     parameter.code                        = response$waarnemingenlijst[[ii]]$aquometadata$parameter$code,
                     parameter.omschrijving                = response$waarnemingenlijst[[ii]]$aquometadata$parameter$omschrijving,
                     #                plaatsbepalingsapparaat.code = response$waarnemingenlijst[[ii]]$aquometadata$PlaatsbepalingsApparaat$code,                             # no longer present in API response
                     #                plaatsbepalingsapparaat.omschrijving = response$waarnemingenlijst[[ii]]$aquometadata$PlaatsbepalingsApparaat$omschrijving,             # no longer present in API response
                     groepering.code                       = response$waarnemingenlijst[[ii]]$aquometadata$Groepering$code,
                     groepering.omschrijving               = response$waarnemingenlijst[[ii]]$aquometadata$Groepering$omschrijving,
                     waardebepalingstechniek.code          = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsTechniek$code,
                     waardebepalingstechniek.omschrijving  = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsTechniek$omschrijving,
                     waardebepalingsmethode.code           = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsMethode$code,
                     waardebepalingsmethode.omschrijving   = response$waarnemingenlijst[[ii]]$aquometadata$waardebepalingsmethode$omschrijving,
                     waardebewerkingsmethode.code          = response$waarnemingenlijst[[ii]]$aquometadata$waardebewerkingsmethode$code,
                     waardebewerkingsmethode.omschrijving  = response$waarnemingenlijst[[ii]]$aquometadata$waardebewerkingsmethode$omschrijving,
                     numeriekewaarde = purrr::map_dbl(response$waarnemingenlijst[[ii]]$metingenlijst,list("meetwaarde", "waarde_numeriek"), .default = NA),
                     alphanumeriekewaarde = purrr::map_chr(response$waarnemingenlijst[[ii]]$metingenlijst, list("meetwaarde", "waarde_alfanumeriek"), .default = NA)
      )
      temp.df <- as.data.frame(nullToNA(temp.l))
    }
    else temp.df <- data.frame()
    if (ii != 1) {
      df = dplyr::bind_rows(df, temp.df)
    }
    else {
      df = temp.df
    }
    if (httr::http_error(resp)) {
      stop(sprintf("RWS API request failed [%s]\n%s\n<%s>",
                   httr::status_code(resp), response$message, response$documentation_url),
           call. = FALSE)
    }
  }
  return(structure(list(content = df, path = path, response = resp)))
}

rws_wfsLocations <- function(){
  
  locationsurl = "https://geo.rijkswaterstaat.nl/services/ogc/hws/DDAPI20/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=locaties&outputFormat=application/json"
  locs = sf::st_read(locationsurl)
  return(locs)
  
}



rws_wfsLatestObs <- function(parFilter = "Waterhoogte", outputFormat = "application/json"){
  
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
        FILTER = paste0(" <Filter> <PropertyIsLike escape=\"!\" singleChar=\".\" wildCard=\"*\"> <PropertyName>PARAMETER_WAT_OMSCHRIJVING</PropertyName> <Literal>*", parFilter, "*</Literal> </PropertyIsLike> </Filter> "),
        outputFormat = outputFormat),
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL),
    class = "url"
  )
  
  url <- httr::build_url(url_list)
  
  if(outputFormat == 'csv') {
    locs_with_latest_obs = readr::read_csv(url)
  } else {
    locs_with_latest_obs = sf::st_read(url)
  }
  
  return(locs_with_latest_obs)
  
}


simplify_df <- function(df, outputFormat = c("nested", "attributed")) {
  
  # 1. Identify columns with only 1 unique non-NA value
  is_constant <- sapply(df, function(col) {
    length(unique(col[!is.na(col)])) == 1
  })
  
  # 2. Extract constant values as a named list
  const_vals <- lapply(df[is_constant], function(col) unique(col[!is.na(col)]))
  const_names <- names(const_vals)
  
  # 2.5 Nest dataframe based on constant columns
  
  df_nested <- df %>% dplyr::group_by(across(all_of(const_names))) %>% tidyr::nest()
  
  # 3. Remove constant columns from dataframe
  df_simplified <- df[!is_constant]
  
  # 4. Store constant metadata as attributes
  for (nm in names(const_vals)) {
    attr(df_simplified, nm) <- const_vals[[nm]]
  }
  
  # 5. Add attribute listing all metadata names (easy lookup)
  attr(df_simplified, "constant_columns") <- names(const_vals)
  
  if(outputFormat == "attributed"){
    return(df_simplified)} else{
      return(df_nested)
    }
}

