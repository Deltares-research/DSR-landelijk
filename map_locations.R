
library(httr2)
library(jsonlite)
library(rvest)
library(dplyr)
library(stringr)


resolve_wrd_id <- function(location) {
  
  if (grepl("^Id-[0-9a-f-]{36}$", location)) {
    return(location)
  }
  
  search_url <- paste0(
    "https://wrd.rwskennisbron.nl/index.php/Speciaal:Zoeken?search=",
    utils::URLencode(location)
  )
  
  html <- rvest::read_html(search_url)
  
  hits <- html |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("Id-")
  
  for (hit in unique(hits)) {
    
    id <- stringr::str_extract(
      hit,
      "Id-[0-9a-f-]{36}"
    )
    
    page <- tryCatch(
      rvest::read_html(
        paste0(
          "https://wrd.rwskennisbron.nl/index.php/",
          id
        )
      ),
      error = function(e) NULL
    )
    
    if (is.null(page))
      next
    
    txt <- page |>
      rvest::html_text2()
    
    if (grepl(
      paste0("\\b", location, "\\b"),
      txt,
      ignore.case = TRUE
    )) {
      return(id)
    }
  }
  
  stop("Could not resolve WRD id for: ", location)
}




get_wrd_relations <- function(location,
                              base_url = "https://wrd.rwskennisbron.nl/index.php",
                              timeout = 30) {
  
  locations <- unique(as.character(location))
  
  purrr::map_dfr(
    locations,
    function(loc) {
      
      loc <- resolve_wrd_id(loc)
      
      url <- paste0(
        base_url,
        "/",
        utils::URLencode(loc, reserved = TRUE)
      )
      
      out <- tryCatch({
        
        html <- httr2::request(url) |>
          httr2::req_user_agent("rwsapi2") |>
          httr2::req_timeout(timeout) |>
          httr2::req_perform() |>
          httr2::resp_body_html()
        
        tables <- html |>
          rvest::html_elements("table") |>
          rvest::html_table()
        
        rel_tables <- purrr::keep(
          tables,
          ~ all(
            c("Vertrekpunt", "Relatie", "Eindpunt") %in%
              names(.x)
          )
        )
        
        if (length(rel_tables) == 0) {
          return(NULL)
        }
        
        dplyr::bind_rows(rel_tables) |>
          dplyr::transmute(
            query_location = loc,
            vertrekpunt = trimws(Vertrekpunt),
            relatie = trimws(Relatie),
            eindpunt = trimws(Eindpunt)
          ) |>
          dplyr::mutate(
            bron_code = stringr::str_trim(
              stringr::str_remove(
                vertrekpunt,
                "\\s*\\([^\\)]*\\)$"
              )
            ),
            bron_type = stringr::str_match(
              vertrekpunt,
              "\\(([^\\)]*)\\)"
            )[,2]
          )
        
      }, error = function(e) {
        
        tibble::tibble(
          query_location = loc,
          vertrekpunt = NA_character_,
          relatie = NA_character_,
          eindpunt = NA_character_,
          bron_code = NA_character_,
          bron_type = NA_character_,
          error = conditionMessage(e)
        )
        
      })
      
      out
    }
  ) |>
    dplyr::distinct()
}

# voorbeeld
get_wrd_relations(
  c(
    "Id-0515b76e-0115-4650-9064-3b2fc5eb73ef",
    "Id-07120024-d0ea-42d9-b9f9-c01afbbf9e2c"
  )
)

get_wrd_relations(
  c(
    "almen"
  )
)


# voor gebruik in data flow: (wm_locs moet dan eerst opgehaald worden )
# wm_locs |>
#   dplyr::distinct(locatiecode) |>
#   dplyr::pull(locatiecode) |>
#   get_wrd_relations()
