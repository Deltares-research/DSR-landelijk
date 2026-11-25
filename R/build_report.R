

build_report <- function(
    watersysteem,
    cfg,
    metadata,
    koppeling_meetpunten_rijkswateren,
    output_file
) {
  
  qmd <- c(
    "---",
    glue::glue("title: '{cfg$title}'"),
    "format: html",
    "toc: true",
    "---",
    "",
    glue::glue("# {cfg$title}"),
    "",
    cfg$intro_text,
    ""
  )
  
  for (chapter in cfg$chapters) {
    
    qmd <- c(
      qmd,
      "",
      glue::glue("# {tools::toTitleCase(chapter)}"),
      ""
    )
    
    chapter_lines <- build_wq_chapter(
      chapter = chapter,
      watersysteem = watersysteem,
      metadata = metadata
    )
    
    qmd <- c(
      qmd,
      chapter_lines
    )
  }
  
  writeLines(
    qmd,
    output_file
  )
  
}

build_wq_chapter <- function(
    chapter,
    watersysteem,
    metadata
) {
  
  if (chapter != "waterkwaliteit") {
    return(
      c(
        "Nog niet geïmplementeerd.",
        ""
      )
    )
  }
  
  dat <- metadata |>
    unnest(locaties) %>%
    left_join(koppeling_meetpunten_rijkswateren, by = c(CODE = "locatie_code")) |>
    filter(
      watersysteem_id == watersysteem
    )
  
  groepen <- unique(
    dat$waarneminggroep
  )
  
  out <- character()
  
  for (groep in groepen) {
    
    out <- c(
      out,
      glue::glue("## {groep}"),
      ""
    )
    
    groep_dat <- dat %>%
      filter(
        waarneminggroep == groep
      )
    
    for (i in seq_len(nrow(groep_dat))) {
      
      row <- groep_dat[i, ]
      
      out <- c(
        out,
        glue::glue(
          "### {row$parameter_wat_omschrijving}"
        ),
        "",
        render_parameter(row)
      )
      
    }
    
  }
  
  out
  
}

render_wq_parameter <- function(row) {
  
  content <- row$content[[1]]
  
  out <- character()
  
  for (i in seq_len(nrow(content))) {
    
    item <- content[i, ]
    
    caption <- glue::glue(
      item$caption,
      parameter_rapport =
        row$parameter_wat_omschrijving
    )
    
    if (item$type == "tekst") {
      
      out <- c(
        out,
        as.character(caption),
        ""
      )
      
    }
    
    if (item$type == "plot") {
      
      out <- c(
        out,
        paste0(
          "![",
          caption,
          "](https://dummyimage.com/1200x400)"
        ),
        "",
        paste0(
          "*",
          caption,
          "*"
        ),
        ""
      )
      
    }
    
  }
  
  out
  
}
