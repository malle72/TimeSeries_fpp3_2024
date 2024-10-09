
library(fpp3)

gdpc <- global_economy |>
  mutate(GDP_per_capita = GDP/Population) |>
  select(Year,Country, GDP, Population, GDP_per_capita)
gdpc

gdpc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita)

fit <- gdpc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))
fit

fit |> forecast(h="3 years")

fit |> 
  forecast(h="3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdpc)
