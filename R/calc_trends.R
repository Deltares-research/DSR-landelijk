
# loading libraries ------------------------------------------------------
library(tidyverse)

# read data --------------------------------------------------------------
df_trends <- qs2::qs_read("data/interim/data_trends_rijkswateren.qs2") |> 
  filter(jaar > 1989, parameter_code %in% 'Ntot') |> 
  mutate(datum_num = as.numeric(datum))


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
    theilsen_mod = map(
      data,
      ~ zyp::zyp.sen(
        formula = waarden ~ datum_num, dataframe = .x
      )
    ),
    theilsen_waarden = map(theilsen_mod, function(m) {
      tibble(
        x_val = m$x,
        y_val = m$y,
        # Bereken y-hat: intercept + (slope * x)
        theilsen_y = m$coefficients[["Intercept"]] + (m$coefficients[[2]] * m$x),
        # Optioneel: bereken ook direct het residu (y - y-hat)
        # residual = y_val - y_hat
      )
    }),
    theilsen_intercept = map_dbl(theilsen_mod, ~ .x$coefficients[1]),
    theilsen_slope = map_dbl(theilsen_mod, ~ .x$coefficients[2]),
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
  ) |> unnest(c(ths_y, lowline, theilsen_waarden, median_waarde, data))
