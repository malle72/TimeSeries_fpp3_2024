
# convert tibble to tsibble (daily)
crashes_d <- crashes |>
  mutate(CrashDate = mdy(CrashDate)) |>
  as_tsibble(index = CrashDate)

# convert tibble to tsibble (monthly)
crashes_m <- crashes |>
  mutate(Month = yearmonth(CrashDate)) |>
  group_by(Month) |>
  summarise(
    Crashes = sum(Crashes),
    Pedestrian = sum(Pedestrian),
    Bicycle = sum(Bicycle),
    NonMotorist = sum(NonMotorist),
    Motorcycle = sum(Motorcycle),
    Fatal = sum(Fatal)
    ) |>
  as_tsibble(index = Month)

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


crashes_d |>
  gg_season(Fatal, period='year')

crashes_m |>
  gg_subseries(crashCount)







