
cp <- crash_parish |>
  mutate(CrashDate = mdy(CrashDate), crashCount=Crashes) |>
  as_tsibble(index = CrashDate, key=Parish)

cp <- fill_gaps(cp)

cp |> 
  group_by(Parish) |>
  gg_season(crashCount)
