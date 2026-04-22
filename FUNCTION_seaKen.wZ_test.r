seaKen.wZ_test <- function (x, params,
                            #season_months = list(
                            #  summer = 6:9,
                            #  other = c(10:12, 1:5)
                            )#)
{
### Prep Data
  x2 <- x |>
  mutate(
    Date = as.Date(Date),
    month = month(Date),
    season = if_else(month %in% 6:9, "summer", "other"), # Define seasons
    wy = if_else(month >= 10, year(Date) + 1, year(Date)) # Water year (Oct–Sep)
  )

### Aggregate to season
x_seasonal <- x2 %>%
  group_by(Station, wy, season) %>%
  summarise(
    across(all_of(params), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop") |>
  complete(Station, wy, season = c("other", "summer")) |>
  arrange(Station, wy, season) |>
  mutate(season = factor(season, levels = c("other", "summer")))

### Run seaKen per station
results <- x_seasonal %>%
  group_by(Station) %>%
  group_modify(~{
    
    # Build matrix of parameters
    mat <- as.matrix(.x[params])
    
    ts_mat <- ts(
      mat,
      start = c(min(.x$wy), 1),
      frequency = 2
    )
    
    sk <- seaKen(ts_mat)
    
    tibble(
      parameter = params,
      Z = sk$Z,
      slope = sk$sen.slope,
      slope_pct = sk$sen.slope.pct,
      p_value = sk$p.value
    )
  })

return(results)
}
