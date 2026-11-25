render_parameter <- function(row) {
  
  content <- row$content[[1]]
  
  out <- character()
  
  for (i in seq_len(nrow(content))) {
    
    item <- content[i, ]
    
    caption <- glue(
      item$caption,
      parameter_rapport = row$parameter_wat_omschrijving
    ) |>
      as.character()
    
    if (item$type == "tekst") {
      
      out <- c(
        out,
        caption,
        ""
      )
      
    }
    
    if (item$type == "plot") {
      
      plot_url <- get_plot_key(
        subtype = item$subtype,
        watersysteem = row$watersysteem_id,
        parameter = row$parameter.code,
        eenheid = row$eenheid.code,
        hoedanigheid = row$hoedanigheid.code,
        grootheid = row$grootheid.code,
        compartiment = row$compartiment.code
        )
      
      out <- c(
        out,
        
        paste0(
          "![",
          caption,
          "](",
          plot_url,
          ")"
        ),
        
        "",
        caption,
        ""
      )
      
    }
    
  }
  
  out
  
}