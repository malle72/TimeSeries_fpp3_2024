# Chapter 5 Exercises

# ====1.) Produce forecasts for the following series using whichever of ====
#  NAIVE(y), SNAIVE(y) or RW(y ~ drift()) is more appropriate in each case:

# ==Australian population (global_economy)==
# Create dataset
aus_pop <-global_economy |>
  filter(Country == 'Australia') |>
  select(-GDP,-Growth,-CPI,-Imports,-Exports)
# Fit model (Drift likely best)
aus_pop_mod <- aus_pop |>
  model(Drift=RW(Population~drift()))
# Run forecast
aus_pop_fc <- aus_pop_mod |>
  forecast(h=5)
# Plot data and forecast
aus_pop_fc |>
  autoplot(aus_pop, level=80)

# ==Bricks (aus_production)==
brick_fit <- aus_production |>
  filter(!is.na(Bricks)) |>
  model(Seasonal_naive = SNAIVE(Bricks)) # Seasonal naive probably best

brick_fc <- brick_fit |>
  forecast(h='5 years')

brick_fc |>
  autoplot(aus_production, level=80) +
  labs(title='Clay brick production in Australia',
       y='Millions of Bricks')+
  guides(color=guide_legend(title='Forecast'))

# ==NSW Lambs (aus_livestock)
lambs <- aus_livestock |>
  filter(Animal=='Lambs', State=='New South Wales')

lambs |> autoplot(Count)

lambs_mod <- lambs |>
  model(Seasonal_Naive = SNAIVE(Count)) # none very good, but snaive probably best
                               # maybe filtering to a more recent dataset would help

lambs_fc <- lambs_mod |>
  forecast(h=10)

lambs_fc |>
  autoplot(lambs,level=80)

# ==Household Wealth (hh_budget)
hhw <- hh_budget |>
  filter(Country=='USA') |>
  select(Country,Year,Wealth)

hhw |> autoplot(Wealth)

hhw_mod <- hhw |>
  model(Drift = RW(Wealth ~ drift()))  # Can't do seasonal

hhw_fc <- hhw_mod |>
  forecast(h=5)

hhw_fc |> autoplot(hhw,level=80)

# ==Australian takeaway food turnover (aus_retail)
take <- aus_retail |>
  filter(Industry == 'Takeaway food services', State=='New South Wales')

take |> autoplot(Turnover)

take_mod <- take|>
  model(Drift = RW(Turnover~drift()))

take_fc <- take_mod |>
  forecast(h=10)

take_fc |>
  autoplot(take)


