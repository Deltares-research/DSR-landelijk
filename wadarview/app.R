library(shiny)
library(data.table)
library(rlist)
library(XML)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(shinyjs)
library(DT)
library(shinyAce)
library(glue)
library(jsonlite)
library(readr)
source("extra_functions.R")

## todo
## bij temperatuur, salinteit, misschien ook zuurstof? laat ook bemonsteringssoort.code kiezen
## bij bemonsteringssoort.code == VERTMNWS : toon ctd vertikale profielen voor
## 
## Bij waterhoogte keuzes toevoegen voor procestype, typering en groepering
## 

max_full_download_rows <- 250000

estimate_full_series_size <- function(
        dat,
        start_date = as.Date("1990-01-01")
){
    
    if(nrow(dat) < 2)
        return(NA_real_)
    
    ts <- sort(
        unique(
            as.numeric(
                lubridate::as_datetime(
                    dat$tijdstip
                )
            )
        )
    )
    
    if(length(ts) < 2)
        return(NA_real_)
    
    typical_interval_seconds <- median(
        diff(ts),
        na.rm = TRUE
    )
    
    full_period_seconds <- as.numeric(
        difftime(
            Sys.Date(),
            start_date,
            units = "secs"
        )
    )
    
    full_period_seconds / typical_interval_seconds
    
}

# Startup ==========================================================================

metadata_cache_file <- "data/metadata_cache.rds"
metadata_max_age_days <- 7

read_or_download_metadata <- function(
        cache_file = metadata_cache_file,
        max_age_days = metadata_max_age_days
) {
    
    cache_is_fresh <- file.exists(cache_file) &&
        difftime(
            Sys.time(),
            file.info(cache_file)$mtime,
            units = "days"
        ) <= max_age_days
    
    if (cache_is_fresh) {
        
        message("Reading metadata from cache")
        
        return(
            readRDS(cache_file)
        )
    }
    
    message("Downloading fresh metadata")
    
    md <- rws_metadata()
    
    # Atomisch schrijven: eerst tijdelijk bestand,
    # daarna vervangen. Dit voorkomt een kapotte cache.
    tmp_file <- paste0(
        cache_file,
        ".tmp"
    )
    
    saveRDS(
        md,
        tmp_file,
        compress = "xz"
    )
    
    file.rename(
        tmp_file,
        cache_file
    )
    
    md
}


md <- read_or_download_metadata()

catalogue <- md$content$locatielijst %>%
    full_join(md$content$aquometadatalocatielijst) %>%
    full_join(md$content$aquometadatalijst)

parameters <- catalogue %>%
    distinct(parameter_wat_omschrijving) %>%
    arrange(parameter_wat_omschrijving) %>%
    pull(parameter_wat_omschrijving)

coupling_cat_rw <- read_csv2("data/koppeling_meetpunten_rijkswateren.csv")

# ToDo
# Koppelen met Rijkswater
# Koppelen met groepen uit meetplan

#== UI ============================================

ui <- fluidPage(
    
    useShinyjs(),
    
    titlePanel("RWS DDL Explorer"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            div(
                style = "
            display: flex;
            align-items: center;
            margin-bottom: 18px;
        ",
                
                tags$img(
                    src = "deltares_logo_blauw.png",
                    alt = "Application logo",
                    style = "
                max-width: 180px;
                max-height: 80px;
                width: auto;
                height: auto;
            "
                )
            ),
            
            selectizeInput(
                "rijkswater",
                "Water system",
                choices = c(
                    "all water systems",
                    sort(
                        unique(
                            na.omit(
                                coupling_cat_rw$watersysteem_id
                            )
                        )
                    )
                ),
                selected = "all water systems",
                options = list(
                    placeholder = "Choose water system"
                )
            ),
            
            selectizeInput(
                "parameter",
                "Parameter",
                choices = NULL,
                selected = NULL,
                options = list(
                    placeholder = "Choose a parameter"
                )
            ),
            
            hr(),
            
            uiOutput("station_info"),
            
            br(),
            
            uiOutput("download_warning"),
            
            br(),
            
            uiOutput("location_status"),
            
            br(),
            
            actionButton(
                "download_full",
                "Download complete series"
            ),
            
            br(),
            br(),
            
            downloadButton(
                "export_csv",
                "Export CSV"
            ),
            
            hr(),
            
            div(
                class = "app-contact",
                
                tags$small(
                    strong("Contact"),
                    br(),
                    "Willem Stolte",
                    br(),
                    
                    tags$a(
                        href = "mailto:your.email@example.com",
                        "willem.stolte@deltares.nl"
                    )
                )
            )
        ),
        
        mainPanel(
            
            leafletOutput(
                "map",
                height = 400
            ),
            
            br(),
            
            tabsetPanel(
                
                tabPanel(
                    "Plot",
                    plotOutput(
                        "preview_plot",
                        height = 400
                    )
                ),
                
                tabPanel(
                    "Data",
                    DTOutput("data_table")
                ),
                
                tabPanel(
                    
                    "R code",
                    
                    br(),
                    
                    actionButton(
                        "copy_code",
                        "Copy code"
                    ),
                    
                    downloadButton(
                        "download_r_script",
                        "Save R script"
                    ),
                    
                    br(),
                    br(),
                    
                    aceEditor(
                        "download_code",
                        mode = "r",
                        theme = "github",
                        readOnly = TRUE,
                        height = "500px"
                    )
                    
                ),
                
                tabPanel(
                    "Metadata",
                    tableOutput("metadata_table")
                )
                
            )
            
        )
    )
)

# SERVER ======================================================================

server <- function(input, output, session) {
    
    display_data <- reactiveVal(NULL)
    
    full_data_cache <- reactiveVal(NULL)
    
    full_data_available <- reactiveVal(FALSE)
    
    selected_station_rv <- reactiveVal(NULL)
    
    disable("export_csv")
    
    observeEvent(input$parameter, {
        
        full_data_cache(NULL)
        
        full_data_available(FALSE)
        
        disable("export_csv")
        
    })
    
    observe({
        
        updateSelectizeInput(
            session,
            "parameter",
            choices = parameters,
            selected = "Massaconcentratie nitraat in Oppervlaktewater uitgedrukt in stikstof / opgeloste fractie in mg/l",
            server = TRUE
        )
        
    })
    
    # Empty map --------------------------------------------------------------
    
    output$map <- renderLeaflet({
        
        leaflet() %>%
            addTiles() %>%
            fitBounds(
                lng1 = 2.5,  # west
                lat1 = 50.5, # south
                lng2 = 7.5,  # east
                lat2 = 55.0  # north
            )
        
    })
    
    # Load locations only after parameter is selected --------------------------------------------------------------------------

    rijkswater_codes <- reactive({
        
        req(input$rijkswater)
        
        if (identical(input$rijkswater, "all water systems")) {
            return(NULL)
        }
        
        coupling_cat_rw |>
            dplyr::filter(
                watersysteem_id == input$rijkswater
            ) |>
            dplyr::pull(locatie_code) |>
            as.character() |>
            trimws() |>
            tolower() |>
            unique()
    })
    
    
    parameter_locations <- reactive({
        
        req(input$parameter)
        req(input$rijkswater)
        
        locs <- withProgress(
            message = paste(
                "Loading locations for",
                input$parameter
            ),
            value = 0,
            {
                
                rws_wfsLatestObsMod(
                    parFilter = input$parameter,
                    keepFromYear = 2024
                )
            }
        )
        
        # Geen bruikbaar WFS-resultaat
        if (
            is.null(locs) ||
            nrow(locs) == 0L ||
            !"CODE" %in% names(locs)
        ) {
            return(
                sf::st_sf(
                    CODE = character(),
                    geometry = sf::st_sfc(crs = 4326)
                )
            )
        }
        
        locs <- sf::st_transform(
            locs,
            4326
        )
        
        # Bij "all water systems" geen watersysteemfilter toepassen
        if (identical(input$rijkswater, "all water systems")) {
            return(locs)
        }
        
        codes <- rijkswater_codes()
        
        # Gekozen watersysteem heeft zelf geen gekoppelde codes
        if (
            is.null(codes) ||
            length(codes) == 0L
        ) {
            return(locs[0, ])
        }
        
        locs |>
            dplyr::filter(
                tolower(trimws(as.character(CODE))) %in% codes
            )
    })    
    
    # old_parameter_locations <- reactive({
    #     
    #     req(input$parameter)
    #     
    #     withProgress(
    #         message = paste("Loading locations for", input$parameter),
    #         value = 0,
    #         {
    #             
    #             rws_wfsLatestObsMod(
    #                 parFilter = input$parameter,
    #                 keepFromYear = 2026
    #             ) %>% 
    #                 sf::st_transform(4326)
    #             
    #         }
    #     )
    #     
    # })
    
    # Update map -------------------------------------------------------------------
    
    observeEvent(parameter_locations(), {
        
        locs <- parameter_locations()
        
        if (
            is.null(locs) ||
            nrow(locs) == 0L
        ) {
            
            leafletProxy("map") |>
                clearMarkers() |>
                clearControls()
            
            selected_station_rv(NULL)
            display_data(NULL)
            
            showNotification(
                paste(
                    "No locations found for",
                    input$parameter,
                    "in",
                    input$rijkswater
                ),
                type = "warning",
                duration = 6
            )
            
            return()
        }        
        # print(names(locs))
        
        # print(class(locs$WAARDE_LAATSTE_METING))
        
        # summary(locs$WAARDE_LAATSTE_METING)
        
        vals <- suppressWarnings(
            as.numeric(locs$WAARDE_LAATSTE_METING)
        )
        
        locs$plotvalue <- vals
        
        locs <- locs %>%
            filter(
                !is.na(plotvalue),
                is.finite(plotvalue)
            )
        
        if(nrow(locs) == 0){
            
            showNotification(
                "No numeric observation values available.",
                type = "warning"
            )
            
            return()
            
        }
        
        pal <- colorNumeric(
            palette = "viridis",
            domain = locs$plotvalue
        )        

        leafletProxy("map") %>%
            clearMarkers() %>%
            clearControls() %>%
            addCircleMarkers(
                data = locs,
                layerId = ~CODE,
                radius = 6,
                stroke = TRUE,
                weight = 1,
                color = "black",
                fillColor = ~pal(plotvalue),
                fillOpacity = 0.9,
                popup = ~paste0(
                    "<b>", NAAM, "</b><br>",
                    CODE, "<br>",
                    round(plotvalue, 2)
                )
            ) %>%
            addLegend(
                position = "bottomright",
                pal = pal,
                values = locs$plotvalue#,
                # title = input$parameter
            )        
    })
    
    #== rezoom map on rijkswater ========================================================
    
    observeEvent(parameter_locations(), {
        
        locs <- parameter_locations()
        
        if (nrow(locs) == 0) {
            
            leafletProxy("map") |>
                clearMarkers() |>
                clearControls()
            
            showNotification(
                "No locations found.",
                type = "warning"
            )
            
            return()
        }
        
        # Waarden voorbereiden
        locs <- locs |>
            dplyr::mutate(
                plotvalue = suppressWarnings(
                    as.numeric(WAARDE_LAATSTE_METING)
                )
            ) |>
            dplyr::filter(
                is.finite(plotvalue)
            )
        
        if (nrow(locs) == 0) {
            return()
        }
        
        # Kleuren
        pal <- leaflet::colorNumeric(
            palette = "viridis",
            domain = locs$plotvalue
        )
        
        # Markers tekenen
        proxy <- leafletProxy("map") |>
            clearMarkers() |>
            clearControls() |>
            addCircleMarkers(
                data = locs,
                layerId = ~CODE,
                radius = 6,
                stroke = TRUE,
                weight = 1,
                color = "black",
                fillColor = ~pal(plotvalue),
                fillOpacity = 0.9,
                label = ~sprintf(
                    "%s: %.2f",
                    CODE,
                    plotvalue
                ),
                popup = ~paste0(
                    "<b>", NAAM, "</b><br>",
                    CODE, "<br>",
                    round(plotvalue, 2)
                )
            ) |>
            addLegend(
                position = "bottomright",
                pal = pal,
                values = locs$plotvalue,
                title = sub(".*\\s", "", input$parameter)
            )
        
        # HIER komt de zoomcode
        if (nrow(locs) == 1) {
            
            xy <- sf::st_coordinates(locs)[1, ]
            
            proxy |>
                setView(
                    lng = xy[["X"]],
                    lat = xy[["Y"]],
                    zoom = 11
                )
            
        } else {
            
            bbox <- sf::st_bbox(locs)
            
            proxy |>
                fitBounds(
                    lng1 = unname(bbox["xmin"]),
                    lat1 = unname(bbox["ymin"]),
                    lng2 = unname(bbox["xmax"]),
                    lat2 = unname(bbox["ymax"])
                )
        }
        
    })    
    
    # Selected station --------------------------------------------------------------------------
    
    observeEvent(input$map_marker_click, {
        
        selected_station_rv(
            input$map_marker_click$id
        )
        
    })
    
    selected_station <- reactive({
        
        selected_station_rv()
        
    })
    
    
    output$station_info <- renderUI({
        
        req(selected_station())
        
        estimated_rows <- NA
        
        if(!is.null(display_data())){
            
            estimated_rows <- estimate_full_series_size(
                display_data()
            )
            
        }
        
        tagList(
            
            strong("Selected station"),
            br(),
            selected_station(),
            
            if(!is.na(estimated_rows)){
                
                tagList(
                    
                    br(),
                    br(),
                    
                    strong("Estimated full series size"),
                    br(),
                    
                    format(
                        round(estimated_rows),
                        big.mark = ","
                    ),
                    
                    "observations"
                    
                )
                
            }
            
        )
        
    })
    
    # Metadata for clicked station ------------------------------------------------------------------
    
    selected_metadata <- reactive({
        
        req(selected_station())
        
        catalogue %>%
            filter(
                parameter_wat_omschrijving == input$parameter,
                code == selected_station()
            ) %>%
            slice(1)
        
    })
    
    #===== Preview data =============================

    preview_data <- reactive({
        
        meta <- selected_metadata()
        
        req(nrow(meta) == 1)
        
        filters <- c(
            "compartiment",
            "grootheid",
            "parameter",
            "hoedanigheid",
            "eenheid",
            "typering",
            "groepering"
        )
        
        preview_periods <- c(7L, 30L, 365L, 1095)
        minimum_rows <- 10L
        
        withProgress(
            message = "Loading preview",
            value = 0,
            {
                
                for (i in seq_along(preview_periods)) {
                    
                    days <- preview_periods[[i]]
                    is_final_period <- i == length(preview_periods)
                    
                    incProgress(
                        amount = 1 / length(preview_periods),
                        detail = paste(
                            "Checking the last",
                            days,
                            "days"
                        )
                    )
                    
                    query <- rws_observation_query(
                        metadata = meta,
                        start_date = Sys.Date() - days,
                        end_date = Sys.Date(),
                        filters = filters
                    )
                    
                    response <- tryCatch(
                        rws_observations(
                            bodylist = query
                        ),
                        error = function(e) {
                            
                            showNotification(
                                paste(
                                    "Preview request failed:",
                                    conditionMessage(e)
                                ),
                                type = "error",
                                duration = 8
                            )
                            
                            NULL
                        }
                    )
                    
                    if (
                        is.null(response) ||
                        is.null(response$content) ||
                        !is.data.frame(response$content)
                    ) {
                        next
                    }
                    
                    n <- nrow(response$content)
                    
                    # For 7 and 30 days: require at least 10 rows.
                    # For 365 days: return any non-empty result.
                    enough_data <- if (is_final_period) {
                        n > 0L
                    } else {
                        n >= minimum_rows
                    }
                    
                    if (enough_data) {
                        
                        message(
                            "Preview returned ",
                            n,
                            " observations for the last ",
                            days,
                            " days."
                        )
                        
                        return(response)
                    }
                    
                    message(
                        "Only ",
                        n,
                        " observations found for the last ",
                        days,
                        " days; trying a longer period."
                    )
                }
                
                showNotification(
                    paste(
                        "No observations found for",
                        selected_station(),
                        "during the last year."
                    ),
                    type = "warning",
                    duration = 8
                )
                
                NULL
            }
        )
    })    
    
    #=== empty cache ===========================
    
    observeEvent(preview_data(), {
        
        display_data(
            preview_data()$content
        )
        
        full_data_cache(NULL)
        
        full_data_available(FALSE)
        
        disable("export_csv")
        
    })
    
    # Preview plot ---------------------------------------------------------------
    
    output$preview_plot <- renderPlot({
        
        # diagnostics
        # print("renderPlot")
        # print(class(display_data()))
        # str(display_data())

        dat <- display_data()
        
        req(dat)
        
        dat <- dat %>%
            filter(
                numeriekewaarde < 1e9,
                numeriekewaarde != 99999,
                trunc(numeriekewaarde) != 9999,
                trunc(numeriekewaarde) != -1000
                )
        
        # validate(
        #     need(nrow(dat) > 0, "No observations available")
        # )
        
        req(nrow(dat) > 0)
        
        period_start <- min(
            as_datetime(dat$tijdstip),
            na.rm = TRUE
        )
        
        period_end <- max(
            as_datetime(dat$tijdstip),
            na.rm = TRUE
        )
        
        subtitle_text <- paste(
            format(period_start, "%Y-%m-%d"),
            "to",
            format(period_end, "%Y-%m-%d"),
            "|",
            format(
                nrow(dat),
                big.mark = ","
            ),
            "observations"
        )
        
        if(nrow(dat) > 50000){
            
            p <- ggplot(
                dat,
                aes(
                    as_datetime(tijdstip),
                    numeriekewaarde
                )
            ) +
                geom_line(aes(color = referentievlak))
            
        } else {
            
            p <- ggplot(
                dat,
                aes(
                    as_datetime(tijdstip),
                    numeriekewaarde
                )
            ) +
                geom_point(
                    aes(color = referentievlak),
                    shape = 21,
                    size = 3,
                    fill = "white"
                ) +
                geom_smooth(
                    aes(color = referentievlak),
                    method = "gam",
                    formula = y ~ s(x, bs = "ps", k = 200, m = c(2,1)),
                    # span = 0.7,
                    se = TRUE
                ) 
            
        }
        
        p +
            theme_minimal(
                base_size = 16
            ) +
            scale_x_datetime(
                date_labels = "%Y-%m-%d"
            ) +
            labs(
                title = paste(
                    selected_station(),
                    "\n",
                    input$parameter
                ),
                subtitle = subtitle_text,
                x = NULL,
                y = unique(dat$eenheid.code)[1],
                color = "height | vert ref"
            )        
    })
    
    #== output data table ==================================
    
    output$data_table <- renderDT({
        
        req(display_data())
        
        dat <- simplify_df(
            display_data(),
            outputFormat = "attributed"
        )
        
        dat <- dat[, sapply(dat, function(x) {
            
            if(is.character(x)) {
                any(
                    !is.na(x) &
                        x != ""
                )
            } else {
                any(!is.na(x))
            }
            
        })]
        
        datatable(
            dat,
            filter = "top",
            options = list(
                pageLength = 25,
                scrollX = TRUE
            )
        )
    })
    
    #=== metadata ===============================
    
    output$metadata_table <- renderTable({
        
        req(display_data())
        
        dat <- simplify_df(
            display_data(),
            outputFormat = "attributed"
        )
        
        atts <- attributes(dat)
        
        tibble::tibble(
            
            Field = names(atts),
            
            Value = vapply(
                atts,
                function(x) paste(x, collapse = ", "),
                character(1)
            )
            
        ) %>%
            filter(
                !Field %in% c(
                    "names",
                    "row.names",
                    "class"
                )
            )
        
    })
    
    observe({
        
        if (is.null(input$parameter) ||
            is.null(selected_station())) {
            
            updateAceEditor(
                session,
                "download_code",
                value = ""
            )
            
            return()
        }
        
        updateAceEditor(
            session,
            "download_code",
            value = download_code_text()
        )
    })
    
    download_code_text <- reactive({
        
        glue(
            '
# remotes::install_github("wstolte/rwsapi")
# library(rwsapi)

md <- rws_metadata()

catalogue <- md$content$locatielijst |>
    dplyr::full_join(
        md$content$aquometadatalocatielijst
    ) |>
    dplyr::full_join(
        md$content$aquometadatalijst
    )

meta <- catalogue |>
    dplyr::filter(
        parameter_wat_omschrijving == "{input$parameter}",
        code == "{selected_station()}"
    ) |>
    dplyr::slice(1)

query <- rws_observation_query(
    metadata = meta,
    start_date = as.Date("1990-01-01"),
    end_date = Sys.Date(),
    filters = c(
        "compartiment",
        "grootheid",
        "parameter",
        "hoedanigheid",
        "eenheid"
    )
)

obs <- rws_observations(
    bodylist = query
)

data <- obs$content
'
        )

    })

observe({
    
    if (
        is.null(input$parameter) ||
        input$parameter == "" ||
        is.null(selected_station())
    ) {
        return()
    }
    
    txt <- tryCatch(
        download_code_text(),
        error = function(e) NULL
    )
    
    if (
        is.null(txt) ||
        length(txt) == 0
    ) {
        return()
    }
    
    # diagnostics
    # print("Updating Ace")
    # print(class(txt))
    # print(length(txt))
    # print(txt)
    
    updateAceEditor(
        session,
        "download_code",
        value = as.character(txt)
    )
    
})

observeEvent(input$copy_code, {
    
    runjs(
        sprintf(
            "navigator.clipboard.writeText(%s);",
            jsonlite::toJSON(
                download_code_text(),
                auto_unbox = TRUE
            )
        )
    )
    
})

output$download_r_script <- downloadHandler(
    
    filename = function() {
        
        paste0(
            selected_station(),
            "_download.R"
        )
        
    },
    
    content = function(file) {
        
        writeLines(
            download_code_text(),
            file
        )
        
    }
    
)

    output$download_warning <- renderUI({
        
        req(display_data())
        
        estimated_rows <- estimate_full_series_size(
            display_data()
        )
        
        if(
            !is.na(estimated_rows) &&
            estimated_rows > max_full_download_rows
        ){
            
            div(
                style = "color:red; padding:5px;",
                
                strong("Large dataset warning"),
                
                br(),
                
                paste(
                    "Estimated size:",
                    format(
                        round(estimated_rows),
                        big.mark = ","
                    ),
                    "observations."
                ),
                
                br(),
                
                "Full download is disabled."
            )
            
        }
        
    })
    
    observe({
        
        req(display_data())
        
        estimated_rows <- estimate_full_series_size(
            display_data()
        )
        
        if(
            !is.na(estimated_rows) &&
            estimated_rows > max_full_download_rows
        ){
            disable("download_full")
        } else {
            enable("download_full")
        }
        
    })

    observe({
        
        locs <- parameter_locations()
        
        if(nrow(locs) == 0){
            
            disable("download_full")
            
        } else {
            
            enable("download_full")
            
        }
        
    })
    
    output$location_status <- renderUI({
        
        req(input$parameter)
        req(input$rijkswater)
        
        locs <- parameter_locations()
        
        n_locations <- if (
            is.null(locs) ||
            !"CODE" %in% names(locs)
        ) {
            0L
        } else {
            nrow(locs)
        }
        
        if (n_locations == 0L) {
            
            div(
                style = "color:red;",
                paste(
                    "No active locations found for",
                    input$parameter,
                    "in",
                    input$rijkswater
                )
            )
            
        } else {
            
            div(
                style = "color:green;",
                paste(
                    n_locations,
                    "locations available"
                )
            )
        }
    })
    #=== Full data =========================
    
    full_data <- eventReactive(
        input$download_full,
        {
            req(selected_station())
            
            meta <- selected_metadata()
            
            req(nrow(meta) == 1)
            
            if(is.null(display_data())){
                
                showNotification(
                    "No preview data available.",
                    type = "warning"
                )
                
                return(NULL)
                
            }
            
            preview <- display_data()
            
            req(preview)
            
            estimated_rows <- estimate_full_series_size(
                preview
            )
            
            if(!is.na(estimated_rows) &&
               estimated_rows > max_full_download_rows){
                
                showNotification(
                    paste(
                        "Download blocked.",
                        "Estimated size:",
                        format(
                            round(estimated_rows),
                            big.mark = ","
                        ),
                        "observations."
                    ),
                    type = "error",
                    duration = 10
                )
                
                return(NULL)
                
            }
            
            meta <- selected_metadata()
            
            withProgress(
                message = "Downloading complete time series",
                value = 0,
                {
                    
                    incProgress(
                        0.1,
                        detail = "Building query"
                    )
                    
                    getList <- rws_observation_query(
                        metadata = meta,
                        start_date = as.Date("1970-01-01"),
                        end_date = Sys.Date(),
                        filters = c(
                            "compartiment",
                            "grootheid",
                            "parameter",
                            "hoedanigheid",
                            "eenheid"
                        )
                    )
                    
                    incProgress(
                        0.5,
                        detail = "Downloading observations"
                    )
                    
                    resp <- tryCatch(
                        
                        R.utils::withTimeout(
                            
                            rws_observations(
                                bodylist = getList
                            ),
                            
                            timeout = 30,
                            onTimeout = "error"
                            
                        ),
                        
                        TimeoutException = function(e){
                            
                            showNotification(
                                "Download stopped after 30 seconds. The series is probably too large.",
                                type = "error",
                                duration = 10
                            )
                            
                            return(NULL)
                            
                        }
                        
                    )
                    
                    if(is.null(resp))
                        return(NULL)
                    
                    resp$content
                    
                }
            )
            
        }
    )    
    
    output$download_allowed <- reactive({
        
        req(display_data())
        
        estimated_rows <- estimate_full_series_size(
            display_data()
        )
        
        is.na(estimated_rows) ||
            estimated_rows <= max_full_download_rows
        
    })
    
    outputOptions(
        output,
        "download_allowed",
        suspendWhenHidden = FALSE
    )
    
    observe({
        
        req(display_data())
        
        estimated_rows <- estimate_full_series_size(
            display_data()
        )
        
        if(!is.na(estimated_rows) &&
           estimated_rows > max_full_download_rows){
            
            disable("download_full")
            
        } else {
            
            enable("download_full")
            
        }
        
    })
    
    #== empty full data cache ======================
    
    observeEvent(full_data(), {
        
        full_data_cache(
            full_data()
        )
        
        display_data(
            full_data()
        )
        
        full_data_available(TRUE)
        
        enable("export_csv")
        
    })
    
    
    # CSV export --------------------------------------------------------------
    
    output$export_csv <- downloadHandler(
        
        filename = function() {
            
            paste0(
                selected_station(),
                "_",
                Sys.Date(),
                ".csv"
            )
        },
        
        content = function(file) {
            
            req(full_data_available())
            
            write.csv(
                full_data_cache(),
                file,
                row.names = FALSE
            )
        }
    )
}

# Run =============================================================

shinyApp(ui, server) 