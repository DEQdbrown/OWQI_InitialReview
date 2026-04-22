seaKen.wZ_test <- function (chem, params) {

params <- c("temp", "d_o", "do_sat", "bod", "ph", "nh3", "no2", "p", "ecoli") 

### Prep Data
  chem2 <- chem |>
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),
    month = month(Date),
    season = if_else(month %in% 6:9, "summer", "other"), # Define seasons
    wy = if_else(month >= 10, year(Date) + 1, year(Date)) # Water year (Oct–Sep)
  )

### Aggregate to season
chem_seasonal <- chem2 %>%
  group_by(Station, wy, season) %>%
  dplyr::summarise(
    across(all_of(params), 
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop") |>
  complete(Station, wy, season = c("other", "summer")) |>
  arrange(Station, wy, season) |>
  mutate(season = factor(season, levels = c("other", "summer")))

### Run seaKen per station
results <- chem_seasonal %>%
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
      slope = sk[,  "sen.slope"],
      slope_pct = sk[, "sen.slope.pct"],
      p_value = sk[, "p.value"]
    ) |>
      mutate(
        trend = case_when(
          p_value <= 0.1 & slope > 0 ~ "increasing",
          p_value <= 0.1 & slope < 0 ~ "decreasing",
          TRUE ~ "no trend"
        )
      )
  })

return(results)
}
