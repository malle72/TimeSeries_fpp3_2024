library(fpp3)

# ==== 7.1 The Linear Model ====

# Code from video
fit_cons <- us_change |>
  model(lm = TSLM(Consumption ~ Income))

report(fit_cons)

us_change |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none")

us_change |>
  GGally::ggpairs(columns = 2:6)

# Textbook Code

# Line plot of Consumption and Income
us_change |>
  pivot_longer(c(Consumption, Income), names_to="Series") |>
  autoplot(value) +
  labs(y = "% change")

# Scatter plot with Regression Line
us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Make model and get diagnostics
us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

# Plotting multiple variables 
us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

# Correlation Matrix
us_change |>
  GGally::ggpairs(columns = 2:6)


# ==== 7.2 Least Squares Estimation ====
fit_consMR <-us_change |>
  model(lm=TSLM(Consumption ~ Income + Production + Unemployment + Savings))

report(fit_consMR)

augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)


# ==== 7.3 Evaluating the Regression Model ====

gg_tsresiduals(fit_consMR)

augment(fit_consMR) |>            # augment adds fitted values and residuals (response and innov)
  features(.innov, ljung_box, lag = 10)


us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,               # pivots data such that there is a row per regressor/value
               names_to = "regressor", values_to = "x") |>  
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# ==== 7.4 Some Useful Predictors ====

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter,Beer)

recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

fit_beer <- recent_production |> model(TSLM(Beer ~ trend() + season()))
report(fit_beer)


augment(fit_beer) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

fourier_beer <- recent_production |> model(TSLM(Beer ~ trend() + fourier(K=2)))
report(fourier_beer)

aus_cafe <- aus_retail |>
  filter(Industry == "Cafes, restaurants and takeaway food services",
         year(Month) %in% 2004:2018) |>
  summarise(Turnover = sum(Turnover))
aus_cafe |> autoplot(Turnover)

fit <- aus_cafe |>
  model(
    K1 = TSLM(log(Turnover) ~ trend() + fourier(K=1)),
    K2 = TSLM(log(Turnover) ~ trend() + fourier(K=2)),
    K3 = TSLM(log(Turnover) ~ trend() + fourier(K=3)),
    K4 = TSLM(log(Turnover) ~ trend() + fourier(K=4)),
    K5 = TSLM(log(Turnover) ~ trend() + fourier(K=5)),
    K6 = TSLM(log(Turnover) ~ trend() + fourier(K=6))
  )

glance(fit) |>
  select(.model, r_squared, adj_r_squared, CV, AICc)

# TODO: Put together forecast plots of these


# ==== 7.5 Selecting Predictors ====









