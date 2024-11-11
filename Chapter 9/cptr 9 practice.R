# Chapter 9

# ==== 9.1 Stationarity and Differencing ====

google_2018 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018)

google_2018 |>
  autoplot(Close)

google_2018 |> ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")

google_2018 |>
  autoplot(difference(Close))

google_2018 |> ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Changes in Google closing stock price")

a10 <- PBS |>
  filter(ATC2 == 'A10') |>
  summarise(Cost = sum(Cost)/1e6)  

a10 |> autoplot(Cost)
a10 |> autoplot(log(Cost))

a10 |> autoplot(
  log(Cost) |> difference(12)
)


h02 <- PBS |>
  filter(ATC2 == 'H02') |>
  summarise(Cost = sum(Cost)/1e6)  

# Seasonal difference
h02 |> autoplot(log(Cost) |> difference(12))

# Seasonal difference then 1st order difference
h02 |> autoplot(log(Cost) |> difference(12) |> difference(1)) 

# = Book Code = 
google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015)

google_2015 |>
  autoplot(Close)

google_2015 |> ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")
google_2015 |>
  autoplot(difference(Close))

google_2015 |> ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Changes in Google closing stock price")

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)

# = Random Walk Model Video =
# Note: white noise is inherently stationary
#       Stationarity does not imply white noise
# Random walks can have long periods of up/down trend
# Sudden/unpredictable changes in direction
# Forecasts are equal to the last observed (naive)

# Single differenced models are the difference between an obs and the obs prior
# Forecasts from this are equivalent to the drift method

# Second Order differencing shows the change in the changes

# Seasonal differencing shows the difference between an obs and the previous obs 
#  in the same season (same season last year)
# Forecasts from this are the seasonal naive forecasts


PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6) |>
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) |>
  pivot_longer(-Month, names_to="Type", values_to="Sales") |>
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) |>
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)

# When data has a strong seasonal pattern, it's recommended to always do 
#  seasonal differencing first, to see if it's useful by itself.

# = ARIMA Models video =

google_2018 |>
  features(Close,unitroot_kpss)  # test for if data are stationary
                                 # 

# Seasonal strength index cutoff: Fs > 0.64
# If true, do one seasonal difference

h02 |>
  mutate(log_sales = log(Cost)) |>
  features(log_sales,feat_stl)
# Seasonal strength from above output = 0.955 > 0.64
# unitroot_nsdiffs feature will directly tell how many seasonal diffs to take
# unitroot_ndiffs feature will directly tell how many other diffs to take

google_2015 |>
  features(Close, unitroot_kpss)  # Not stationary
google_2015 |>
  mutate(diff_close = difference(Close)) |> # Apply difference
  features(diff_close, unitroot_kpss) # Stationary
google_2015 |>
  features(Close, unitroot_ndiffs) # Shows that one diff is needed to make data stationary

aus_total_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))
aus_total_retail |>
  mutate(log_turnover = log(Turnover)) |>
  features(log_turnover, unitroot_nsdiffs)
aus_total_retail |>
  mutate(log_turnover = difference(log(Turnover), 12)) |>
  features(log_turnover, unitroot_ndiffs)

# ==== 9.3 Autoregressive Models ====
# Multiple regression with lagged values of Yt as predictors

# ==== 9.4 Moving Average Models ====
# Multiple regression with past forecast errors as predictors

# ==== 9.5 Non-seasonal ARIMA Models ====
global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)
fit |> forecast(h=10) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian exports")
global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()
global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()
global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(plot_type = 'partial')

fit2 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)


# ==== 9.6 Estimation and Order Selection ====
# See figure 9.12 for workflow of fitting ARIMA models

global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")
global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type='partial')
# Output suggests either an ARIMA(2,1,0) or an ARIMA(0,1,3)

caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit |> pivot_longer(!Country, names_to = "Model name",
                        values_to = "Orders")

glance(caf_fit) |> arrange(AICc) |> select(.model:BIC)
# suggested, stepwise, and search models all performed relatively the same

caf_fit |>
  select(search) |>
  gg_tsresiduals()

augment(caf_fit) |>
  filter(.model=='search') |>
  features(.innov, ljung_box, lag = 10, dof = 3)  # Suggests residuals are white noise

caf_fit |>
  forecast(h=5) |>
  filter(.model=='search') |>
  autoplot(global_economy)
# mean forecast appears similar to random walk
# lot of extra work without much benefit
# however, the prediction intervals are much narrower than the RW would have had

# A constant can be included or excluded explicitly when calling the ARIMA model
# ARIMA(y ~ 1 + ...) will include the constant
# ARIMA(y ~ 0 + ...) will exclude the constant


gg_arma(caf_fit |> select(Country, search))

# ==== 9.9 Seasonal ARIMA Models ====

leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure |>
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")


fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

glance(fit) |>
  arrange(AICc) |>
  select(.model:BIC)

fit |>
  select(auto) |>
  gg_tsresiduals(lag=36)

augment(fit) |>
  filter(.model == 'auto') |>
  features(.innov, ljung_box, lag=24, dof=4)

forecast(fit,h=36) |>
  filter(.model == 'auto') |>
  autoplot(leisure) +
  labs(title='US employment: leisure & hospitality', y='People (millions)')


h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6)

h02 |>
  autoplot(log(Cost) |> difference(12))

h02 |>
  mutate(log(Cost)) |>
  pivot_longer(-Month) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Corticosteroid drug scripts (H02)")

h02 |> gg_tsdisplay(difference(log(Cost), 12),
                    plot_type='partial', lag_max = 24)

fit <- h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
fit |> gg_tsresiduals(lag_max=36)

augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)

fit <- h02 |> model(auto = ARIMA(log(Cost)))
report(fit)
augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 3)

h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) |>
  forecast() |>
  autoplot(h02) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")
