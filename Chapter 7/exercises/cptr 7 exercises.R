

jan14_vic_elec <- vic_elec |>
  filter(yearmonth(Time) == yearmonth("2014 Jan")) |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

jan14_vic_elec |> autoplot()
plot(jan14_vic_elec$Temperature,jan14_vic_elec$Demand)
# Very linear positive relationship

fit <- jan14_vic_elec |>
  model(
    lm = TSLM(Demand ~ Temperature)
)
report(fit)

# seems that as temp goes up, so does demand

# residuals
aug <-augment(fit) 

aug |> autoplot(.innov)

aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram(bins=16) +
  labs(title = "Histogram of residuals")

fit |>
  gg_tsresiduals()

# There seems to be outliers and a trend in the residuals. 
#  They also seem non-normal
# The model doesn't seem adequate

# Forecast for next day; Two scenarios Temp=15 and Temp=35

temp_scen <- scenarios(
  "Temp 15" = new_data(jan14_vic_elec, 1) |>
    mutate(Temperature = 15),
  "Temp 35" = new_data(jan14_vic_elec, 1) |>
    mutate(Temperature = 35),
  names_to = "Scenario"
)

fc <- forecast(fit, temp_scen)

jan14_vic_elec |> autoplot(Demand) +
  autolayer(fc)

fc |> hilo()
# 15 [117908.1, 184888.6]80 [100179.4, 202617.3]95
# 35 [242088.4, 306880.1]80 [224939.1, 324029.4]95


# Plotting all of the data instead of initial subset.
# Total Demand and Max Temp
all_vic_elec <- vic_elec |>
  index_by(Date = as_date(Time)) |>
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

plot(all_vic_elec$Temperature, all_vic_elec$Demand)

# There's a curvilinear relationship here.
# there's a minimum demand point where the temp is likely comfortable
# and people aren't using either AC or Heating.
# Original model is not very generalizable. Only good for forecasting within January. 
# The real relationship is quadratic 

