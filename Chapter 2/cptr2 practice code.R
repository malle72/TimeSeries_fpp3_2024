
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

# ===== Pharmacy Data =====
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10


a10 |>
  autoplot() + 
  geom_point() +
  labs(y= "$ (millions)",
                    title = "australian antidiabetic drug sales")

# ===== Airline Data =====
ansett |>
  autoplot(Passengers)

ansett |> distinct(Class)
ansett |> distinct(Airports)


melsyd_economy <- ansett |>
  filter(Airports=="MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000) |>
  select(-Airports)

melsyd_economy |> 
  autoplot(Passengers) +
  labs(y = "Passengers (Thousands)",
       title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney")


# ==== Patterns ====

aus_production |>
  filter(year(Quarter) >= 1980) |>
  autoplot(Electricity)

aus_production |>
  autoplot(Bricks)

pelt |>
  autoplot(Lynx) + 
  labs(y = "Number Trapped", title="Annual Canadian Lynx Trappings")


# ====Seasonal Plots====
a10 |> 
  gg_season(Cost, labels='both') +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")


beer <- aus_production |>
  select(Quarter, Beer) |>
  filter(year(Quarter) >= 1992)

beer |> autoplot(Beer) +geom_point()

beer |> gg_season(Beer,labels='right')


vic_elec |> gg_season(Demand,period='day')
vic_elec |> gg_season(Demand,period='week')


# ====Subseasonal Plots====
a10 |>
  gg_subseries(Cost) + 
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )


holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")


holidays |>
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

# ====Scatterplots====
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia")

vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")
visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)

# ====Lag Plots====
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

# ====Autocorrelation====
recent_production |> ACF(Beer, lag_max = 9)
# ^ Seasonal autocorrelations. 
# The same quarters each year are similar to previous years
recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title="Australian beer production")

# Trend and seasonal
# adjacent observations are similar and there's seasonality
a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

# ====White Noise====
# And interpreting ACF plots

# Create white noise data and plot it as time series
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")

# ACF plot of white noise. None of the peaks go outside blue lines
y |>
  ACF(wn) |>
  autoplot() + labs(title = "White noise")


