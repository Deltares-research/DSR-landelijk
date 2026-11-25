

get_plot_key <- function(
    subtype,
    watersysteem,
    parameter,
    eenheid,
    hoedanigheid,
    grootheid,
    compartiment
){
  paste0(
    "plots",
    "/",
    slugify(subtype),
    "/",
    slugify(watersysteem),
    "/",
    parameter,
    "_",
    slugify_units(eenheid),
    "_",
    hoedanigheid,
    "_",
    grootheid,
    "_",
    compartiment,
    "_",
    subtype,
    ".png"
  )
  
}
ñ
