# ==== 3.1 Transformations and Adjustments ====
# GDP per capita
global_economy |>
  filter(Country=="Australia") |>
  autoplot(GDP/Population)

# Retail CPI
print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

print_retail |>
  autoplot(Turnover)

aus_economy <- global_economy |>
  filter(Code =='AUS')

print_retail |>
  left_join(aus_economy, by='Year') |>
  mutate(Adjusted_turnover = Turnover / CPI *100) |>
  pivot_longer(c(Turnover, Adjusted_turnover), values_to = 'Turnover') |>
  mutate(name = factor(name,levels = c('Turnover','Adjusted_turnover'))) |>
  ggplot(aes(x=Year,y=Turnover)) +
  geom_line()+
  facet_grid(name ~ ., scales = 'free_y') +
  labs(title = "Turnover: Australian preint media industry", y="$AU")


food <- aus_retail |>
  filter(Industry == 'Food retailing') |>
  summarise(Turnover = sum(Turnover))
food |> autoplot(Turnover)
food |> autoplot(sqrt(Turnover))
food |> autoplot(Turnover^1/3)
food |> autoplot(log(Turnover))
food |> autoplot(-1/Turnover)

# Selecting a lambda
food |>
  features(Turnover, features = guerrero)

# Using the selected lambda for transformation
food |> autoplot(box_cox(Turnover, 0.0895))

# Selecting a lambda
lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)

# Using the selected lambda for transformation
aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))


# ==== 3.2 Time Series Components ====
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# Save models table
dcmp <- us_retail_employment |>
  model(stl = STL(Employed))

# get decomposition table
components(dcmp)

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail (Trend)"
  )

components(dcmp) |> autoplot()

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail (Seasonally Adj.)")

components(dcmp) |> gg_subseries(season_year)

# ==== 3.3 Moving Averages ====
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

# Create moving average column ()
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports and moving average")


beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)
beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE)
  )

beer_ma |>
  autoplot(Beer) +
  geom_line(aes(y = `4-MA`), colour = "#D55E00") +
  geom_line(aes(y=`2x4-MA`), colour = "#0072B2")



us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


# ==== 3.4 Classical Decomposition ====
us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")

# ==== 3.5 Methods used by official statistics agencies ====
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")

seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")

