
crashes |>
  group_by(HighwayClass) |>
  summarise(countCrash=sum(Crashes)) |>
  arrange(countCrash)|>
  print(n=23)

# convert tibble to tsibble (daily)
crashes_d <- crashes |>
  mutate(CrashDate = ymd(CrashDate)) |>
  as_tsibble(index = CrashDate, key = HighwayClass)

# convert tibble to tsibble (monthly)
crashes_m <- crashes |>
  mutate(Month = yearmonth(CrashDate)) |>
  group_by(Month, HighwayClass) |>
  summarise(
    Crashes = sum(Crashes),
    Pedestrian = sum(Pedestrian),
    Bicycle = sum(Bicycle),
    NonMotorist = sum(NonMotorist),
    Motorcycle = sum(Motorcycle),
    Fatal = sum(Fatal)
    ) |>
  as_tsibble(index = Month, key = HighwayClass)

# renaming column Crashes -> crashCount
crashes_d <- mutate(crashes_d, crashCount = Crashes)
crashes_m <- mutate(crashes_m, crashCount = Crashes)

# removing old instance of renamed column
crashes_d <- select(crashes_d, -Crashes)
crashes_m <- select(crashes_m, -Crashes)

# Summarise total crashes
summarise(crashes_d, TotalCrashes = sum(crashCount))

# Create calculated field
crashes_d = crashes_d |>
  mutate(PercentFatal = Fatal/crashCount)
crashes_m = crashes_m |>
  mutate(PercentFatal = Fatal/crashCount)

crashes_m |>
  autoplot(PercentFatal)


# need to filter down to fewer hwy classes
#  remove ones with very few crashes
# hwy classes to remove: 6,31,42,29,23,11,9,19,33,4,NULL


crashes_d |>
  filter(! HighwayClass %in% c(6,31,42,29,23,11,9,19,33,4,'NULL')) |>
  fill_gaps() |>
  gg_season(crashCount)


crashes_m |> 
  filter(! HighwayClass %in% c(6,31,42,29,23,11,9,19,33,4,'NULL')) |>
  fill_gaps() |>
  gg_season(crashCount)


# Lag
crashes_d |>
  filter(HighwayClass == 20) |>
  gg_lag(crashCount, geom = 'point')

crashes_m |>
  filter(HighwayClass == 20) |>
  ungroup() |>
  gg_lag(crashCount, geom = 'point', lags = 1:12)

# Autocorrelation 
crashes_d |>
  filter(HighwayClass == 20) |>
  fill_gaps() |>
  ACF(crashCount) |>
  autoplot()

crashes_m |>
  filter(HighwayClass == 20) |>
  fill_gaps() |>
  ACF(crashCount, lag_max = 12) |>
  autoplot()


