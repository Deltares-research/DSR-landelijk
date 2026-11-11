# aggregate trend for every Rijkswater
# loading libraries ------------------------------------------------------
library(tidyverse)

##
device <- "png"
dpi <- 300
width <-  8.5-2
height <-  (11-2)/2
xmin <- as_date('1990-01-01')

# read data ---------------------------------------------------------------
df_lows <- arrow::read_parquet('data/processed/03_data_trend_info.parquet') |> 
  mutate(rw_wns =  glue::glue('{identificatie}_{wns}'))

# create summary per parameteron national scale
smry_rw <-
  df_lows %>% distinct(
    meetpunt_code_nieuw,
    identificatie,
    wns,
    parameter_code,
    eenheid_code,
    hoedanigheid_code,
    # grootheid,
    compartiment_code,
    median_waarde,
    ths_int,
    ths_slp,
    ths_y,
    trend_conclusie_loc = trend_conclusie
  ) %>%
  group_by(identificatie,
           wns,
           parameter_code,
           eenheid_code,
           hoedanigheid_code,
           compartiment_code,
           # grootheid
  ) %>% 
  summarise(
    # median_min_date = median(min_date, na.rm = TRUE),
    median_slope = median(ths_slp),
    median_intercept = median(ths_int),
    median_ths = median(ths_y),
    mean_slope = mean(ths_slp),
    aantal = length(ths_slp),
    RuN = floor(1 + ((aantal + 1.96 * sqrt(aantal)) / 2)),
    RlN = floor((aantal - 1.96 * sqrt(aantal)) / 2),
    sl95uN = ifelse(aantal >= RuN, sort(ths_slp)[RuN], sort(ths_slp)[aantal]),
    sl95lN = ifelse(RlN > 0, sort(ths_slp)[RlN], sort(ths_slp)[1]),
    int_95_up = ifelse(aantal >= RuN, sort(ths_int)[RuN], sort(ths_int)[aantal]),
    int_95_low = ifelse(RlN > 0, sort(ths_int)[RlN], sort(ths_int)[1]),
    low_95_slope_decenium = sl95lN * 365 * 10,
    median_slope_per_decenium = median_slope * 365 * 10,
    up_95_slope_decenium = sl95uN * 365 * 10,
    trend_conclusie = case_when(
      low_95_slope_decenium * up_95_slope_decenium <= 0 ~ 'niet significant',
      low_95_slope_decenium  > 0 ~ 'opwaarts significant',
      TRUE ~ 'neerwaarts significant'
    ), 
    median_waarde = median(median_waarde, na.rm = TRUE), 
    relatieve_trend_decenium = median_slope_per_decenium / median_waarde * 100,
    aantal_skendall_geen_trend = sum(trend_conclusie_loc == 'geen trend'),
    aantal_skendall_trend_opwaarts = sum(trend_conclusie_loc == 'trend opwaarts'),
    aantal_skendall_trend_neerwaarts = sum(trend_conclusie_loc == 'trend neerwaarts')
  ) |>  
  ungroup()

# plots -------------------------------------------------------------------

# per par
# maak data nested voor lowess model
par_nest <-
  df_lows %>%
  nest(
    .by = c(
      identificatie,
      wns,
      parameter_code,
      eenheid_code,
      hoedanigheid_code,
      compartiment_code,
      # grootheid
    )
  )

# voeg lowess toe #
par_nest <-
  par_nest |> mutate(lowess_par = map(data, ~ as_tibble(
    lowess(.x$datum_num, .x$y, f = 2 / 3, delta = 0)
  ))) |> 
  left_join(select(smry_wbh, identificatie, wns, median_slope, median_intercept, sl95uN, sl95lN, int_95_up, int_95_low, median_waarde, median_ths), by = c('identificatie', 'wns'))

# unnest de data uit het model #
par_unnested <- par_nest |>  unnest(lowess_par) |>
  select(-data) |>
  mutate(
    datum = as_date(x),
    p = '0',
    theilsen_95_low = (-sl95lN * median(x, na.rm = TRUE) + median_ths) + (sl95lN * x), # int_95_low
    med_theilsen_y = (-median_slope * median(x, na.rm = TRUE) + median_ths)  + (median_slope * x),  # median_intercept
    theilsen_95_up = (-sl95uN * median(x, na.rm = TRUE) + median_ths) + (sl95uN * x) # int_95_up
  ) |>
  ungroup() |>
  arrange(parameter_code, identificatie, x)


# model voor percentielen 25 en 75 #
df_lows <-
  left_join(df_lows,
            distinct(par_unnested, datum, identificatie, wns, parameter_code, lowess_par_y = y)) |>
  mutate(low_p = case_when(y > lowess_par_y ~ 75,
                           y < lowess_par_y ~ 25))

# filter data voor de 25 & 75 percentiel lijn #
lowess_p_75 <- filter(df_lows, low_p == 75) |>  
  nest(.by = c(
    identificatie,
    wns,
    parameter_code,
    eenheid_code,
    hoedanigheid_code,
    compartiment_code
  )) |> 
  mutate(lowess_mod = map(data, ~ as_tibble(lowess(
    .x$x, .x$y, f = 2 / 3, delta = 0
  )))) |>  
  select(-data) |>  
  unnest(lowess_mod) |> 
  mutate(datum = as_date(x), 
         p = '75') |> 
  ungroup()

lowess_p_25 <- filter(df_lows, low_p == 25) |>  
  nest(.by = c(
    identificatie,
    wns,
    parameter_code,
    eenheid_code,
    hoedanigheid_code,
    compartiment_code
  ))  |> 
  mutate(lowess_mod = map(data, ~ as_tibble(lowess(
    .x$x, .x$y, f = 2 / 3, delta = 0
  )))) |> 
  select(-data) |> 
  unnest(lowess_mod) |>  
  mutate(datum = as_date(x), 
         p = '25') |>  
  ungroup()


# combineer de  verschillende lowess lijnen # 
trends_par <- bind_rows(par_unnested, lowess_p_25, lowess_p_75) %>% 
  mutate(rw_wns = glue::glue('{identificatie}_{wns}'))

for (rwwns in unique(trends_par$rw_wns)) {
  par_filt <- filter(trends_par, rw_wns == rwwns)
  lows_filt <- filter(df_lows, rw_wns == rwwns)
  ehd <- unique(par_filt$eenheid_code[par_filt$rw_wns == rwwns])
  par = unique(par_filt$parameter_code)
  # xmin <- if_else(min(par_filt$datum, na.rm = TRUE) > xmin, min(par_filt$datum, na.rm = TRUE), xmin)
  
  #plot per par
  ggplot(data = lows_filt, aes(x = datum, y = y)) +
    geom_line(
      aes(group = meetpunt_code_nieuw),
      col = 'grey90',
      linewidth = 0.2,
      alpha = 1
    ) +
    geom_line(data = filter(par_filt, p == '0'),
              aes(y = y),
              col = 'red') +
    geom_line(
      data = filter(par_filt, p == '25'),
      aes(y = y),
      col = 'red',
      linetype = 'dashed'
    ) +
    geom_line(
      data = filter(par_filt, p == '75'),
      aes(y = y),
      col = 'red',
      linetype = 'dashed'
    ) +
    coord_cartesian(ylim = c(0, max(par_filt$y) * 1.05)) +
    labs(x = NULL, y = glue::glue('{par} [{ehd}]')) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.5,
      linetype = "dotted"
    ))
  ggsave(
    filename = glue::glue('{rwwns}_lowess.{device}'),
    path = 'plots/LOWESS/', 
    width = width,
    height = height,
    dpi = dpi,
    device = device
  )
  
  # theilsen
  ggplot(data = filter(par_filt, p == '0'), aes(x = datum, y = med_theilsen_y)) +
    geom_line(
      data = lows_filt,
      aes(y = ths_y, group = meetpunt_code_nieuw),
      col = 'grey90',
      linewidth = 0.2,
      alpha = 1
    ) +
    geom_ribbon(
      aes(ymin = theilsen_95_low, ymax = theilsen_95_up),
      fill = 'red',
      alpha = 0.3
    ) +
    geom_line(col = 'red') +
    # geom_smooth(method = 'theil_sen') + 
    # coord_cartesian(ylim = c(min(par_filt$ats_theilsen_95_up, na.rm = TRUE) * 0.8, max(par_filt$ats_theilsen_95_low, na.rm = TRUE) * 1.05)) +
    labs(x = NULL, y = glue::glue('{par} [{ehd}]')) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(
      colour = "grey90",
      linewidth = 0.5,
      linetype = "dotted"
    ))
  ggsave(
    filename = glue::glue(
      '{rwwns}_theilsen.{device}'
    ),
    path = 'plots/Kruisplots/',
    width = width,
    height = height,
    dpi = dpi,
    device = device
  )
}



# write to disk -----------------------------------------------------------
write_csv2(smry_rw, 'data/processed/trend_info_per_rijkswater.csv')
