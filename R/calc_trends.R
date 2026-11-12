
# loading libraries ------------------------------------------------------
library(tidyverse)

# read data --------------------------------------------------------------
df_trends <- qs2::qs_read("data/interim/data_trends_rijkswateren.qs2") |> 
  filter(jaar > 1989) |>  # parameter_code %in% 'Ntot'
  mutate(datum_num = as.numeric(datum), 
    datum_maand = ymd(floor_date(datum, 'month')))

trend_filt <- df_trends |>
  distinct(meetpunt_code_nieuw, wns, jaar) |>
  add_count(meetpunt_code_nieuw, wns) |>
  filter(n > 2)
# calc trends ------------------------------------------------------------
# nest data voor lowess #
df_nest <- df_trends |>
  nest(.by = c(meetpunt_code_nieuw, wns))

df_lows <- df_nest |>
  mutate(
    lowline = map(
      data,
      ~ as_tibble(
        lowess(
           .x$datum_num,
          .x$waarden,
          f = 2 / 3,
          delta = 0
        )
      )
    ),
    # theilsen_mod = map(
    #   data,
    #   ~ zyp::zyp.sen(
    #     formula = waarden ~ datum_num, dataframe = .x
    #   )
    # ),
    # theilsen_waarden = map(theilsen_mod, function(m) {
    #   tibble(
    #     x_val = m$x,
    #     y_val = m$y,
    #     # Bereken y-hat: intercept + (slope * x)
    #     theilsen_y = m$coefficients[["Intercept"]] + (m$coefficients[[2]] * m$x),
    #     # Optioneel: bereken ook direct het residu (y - y-hat)
    #     # residual = y_val - y_hat
    #   )
    # }),
    # theilsen_intercept = map_dbl(theilsen_mod, ~ .x$coefficients[1]),
    # theilsen_slope = map_dbl(theilsen_mod, ~ .x$coefficients[2]),
    median_waarde = map(data, ~ median(.x$waarden)),
    ths_mod = map(
      data,
      ~ robslopes::robslope(waarden ~ datum_num, data = .x, type = 'TheilSen', verbose = FALSE)
    ),
    ths_int = map_dbl(ths_mod, ~ .x$coefficients[[1]]),
    ths_slp = map_dbl(ths_mod, ~ .x$coefficients[[2]]),
    ths_y = map(ths_mod, ~ tibble(ths_y = .x$fitted.values))
    # extracted_data = map(ths_mod, function(m) {
    #   tibble(
    #     x_val = m$x,
    #     y_val = m$y,
    #     # Bereken y-hat: intercept + (slope * x)
    #     theilsen_y = m$coefficients[[1]] + (m$coefficients[[2]] * m$x),
    #     # Optioneel: bereken ook direct het residu (y - y-hat)
    #     # residual = y_val - y_hat
    #   )
    # })
  ) |> unnest(c(ths_y, lowline, median_waarde, data))  #theilsen_waarden

# create ts for kendall #
trends_ts <-
  df_nest |> mutate(ts = map(data, ~ tibble(
    datum = seq(
      from = make_date(
        year = year(min(.x$datum)),
        month = 1,
        day = 1
      ),
      to  = make_date(
        year = year(max(.x$datum)),
        month = 12,
        day = 1
      ),
      by = 'month'
    )
  ))) |> 
  unnest(ts) |>  
  select(-data) |>  
  ungroup()

trends_month_avg <- df_trends |> group_by(wns, meetpunt_code_nieuw, datum = datum_maand) |>  
  summarise(waarden = mean(waarden, na.rm = TRUE)) |>  
  ungroup()

df_trends_seq <- left_join(trends_ts, trends_month_avg) |>  
  arrange(datum)

df_kendall <- df_trends_seq |> 
  semi_join(trend_filt) |> 
  nest(.by = c(wns, meetpunt_code_nieuw)) |> 
  mutate(skendall =  map(data, ~Kendall::SeasonalMannKendall(ts(.x$waarden, start= year(min(.x$datum)), frequency = 12))), 
         tidy = map(skendall, broom::tidy)) |> 
  unnest(tidy) |> 
  select(wns, meetpunt_code_nieuw, statistic, p.value) |> 
  mutate(
    skendall_trend = case_when(
      p.value > 0.05 | is.na(p.value) | statistic == 0 ~ 'geen trend',
      statistic < 0 ~ 'trend neerwaarts',
      TRUE ~ 'trend opwaarts'
    )
  ) |>
  rename(p_value_skendall = p.value,
         statistic_skendall = statistic)

# join data #
df_lows <- left_join(
  df_lows,
  df_kendall,
  by = c('meetpunt_code_nieuw', 'wns')
) %>%
  mutate(
    # p_value_trend = if_else(!is.na(p_value_skendall), p_value_skendall, ats_pval),
    trend_conclusie = case_when(
      !is.na(skendall_trend) ~ skendall_trend,
      TRUE ~ 'niet genoeg data'
    )
  ) |> 
  select(-ths_mod)  #-theilsen_mod,

# trend gegevens per locatie
trend_smry_loc <-
  df_lows |> distinct(
    meetpunt_code_nieuw,
    parameter_code,
    wns, 
    median_waarde,
    ths_slp,
    ths_int,
    trend_conclusie
  ) |>
  mutate(
    theilsen_slope_per_decenium = ths_slp * 365 * 10,
    relatieve_trend_decenium = theilsen_slope_per_decenium / median_waarde * 100,
    trend_tov_mediaan = case_when(
      trend_conclusie == 'geen trend' ~ "Geen significante trend",
      relatieve_trend_decenium < -50 ~ "<-50%",
      relatieve_trend_decenium >= -50 &
        relatieve_trend_decenium < -25 ~ "-50 - -25%",
      relatieve_trend_decenium >= -25 &
        relatieve_trend_decenium < 0 ~ "-25 - 0%",
      relatieve_trend_decenium >= 0 &
        relatieve_trend_decenium < 25 ~ "0 - 25%",
      relatieve_trend_decenium > 25 ~ "> 25%",
      TRUE ~ 'rest'
    )
  )

# write to disk
arrow::write_parquet(df_lows, 'data/processed/03_data_trend_info.parquet')
vroom::vroom_write(df_lows, 'data/processed/03_data_trend_info.csv', delim = ',', na = '')
write_csv2(trend_smry_loc, 'data/processed/trend_info_per_locatie.csv')
