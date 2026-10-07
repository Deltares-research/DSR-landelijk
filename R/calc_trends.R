
# loading libraries ------------------------------------------------------
library(tidyverse)

# read data --------------------------------------------------------------
df_trends <- qs2::qs_read("data/interim/data_trends_rijkswateren.qs2") |> 
  mutate(datum_num = as.numeric(datum))


# calc trends ------------------------------------------------------------
# nest data voor lowess #
df_nest <- df_trends |>
  nest(.by = c(meetpunt_code_nieuw, wns))

df_lows <- df_nest |>  mutate(
    lowline = map(data, ~ as_tibble(
      lowess(
        as.numeric(.x$datum),
        .x$waarden,
        f = 2 / 3,
        delta = 0
      )
    )),
    # theilsen_mod = map(data, ~
    #                      zyp.sen(
    #                        formula = waarde_dl ~ datum_num, dataframe = .x
    #                      )),
    # theilsen_xy = map(data, ~ as_tibble(
    #   zyp.sen(formula = waarde_dl ~ datum_num, dataframe = .x)[c(6, 7)]
    # )),
    # theilsen_intercept = map(
    #   data,
    #   ~ as_tibble(
    #     zyp.sen(formula = waarde_dl ~ datum_num, dataframe = .x)$coefficients[[1]]
    #   )
    # ),
    # theilsen_slope = map(
    #   data,
    #   ~ as_tibble(
    #     zyp.sen(formula = waarde_dl ~ datum_num, dataframe = .x)$coefficients[[2]]
    #   )
    # ),
    # median_waarde = map(data, ~ 
    #                       median(.x$waarde_dl)
    # )
    # theilsen_waarden = pmap(list(theilsen_xy, theilsen_intercept, theilsen_slope), 
    #                         ~tibble(theilsen_x = .x$x, theilsen_y = .y + (.z * .x$x)))
  )
