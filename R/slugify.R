
slugify <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-|-$)", "")
}


slugify_ <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("(^-|-$)", "")
}

slugify_units <- function(x) {
  x |>
    gsub("%", "pct", x = _, fixed = TRUE) |> # % behouden als betekenis
    gsub("/", "", x = _, fixed = TRUE) |> # / kan geen bestandsnaam zijn
    gsub('[<>:"\\\\|?*]', "_", x = _) |> # overige ongeldige Windows-tekens
    gsub("\\s+", "_", x = _) |> # spaties -> _
    gsub("_+", "_", x = _) # dubbele underscores opruimen
}

