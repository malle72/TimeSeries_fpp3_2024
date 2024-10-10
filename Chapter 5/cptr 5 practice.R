
library(fpp3)

# ==== 5.1 A Tidy Workflow ====
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

# ==== Some Simple Forecasting Methods ====

# Video Code
brick_fit <- aus_production |>
  filter(!is.na(Bricks)) |>
  model(
    Seasonal_naive = SNAIVE(Bricks),
    Naive = NAIVE(Bricks),
    Drift = RW(Bricks ~ drift()),
    Mean = MEAN(Bricks)
  )

brick_fc <- brick_fit |>
  forecast(h='5 years')

brick_fc |>
  autoplot(aus_production, level=NULL) +  # level=NULL to turn off prediction intervals
  labs(title='Clay brick production in Australia',
       y='Millions of Bricks')+
  guides(color=guide_legend(title='Forecast'))
  

# Extract Training Data
fb_stock <- gafa_stock |>
  filter(Symbol == 'FB') |>
  mutate(trading_day = row_number()) |>  # since weekends are skipped, use arbitrary row numbers as date index
  update_tsibble(index = trading_day, regular=T)  # this removes actual dates from subsequent steps.

# Specify, Estimate, and Forecast
fb_stock |>
  model(
    Mean = MEAN(Close),
    Naive = NAIVE(Close),
    Drift = RW(Close ~ drift())
  ) |>
  forecast(h=42) |>
  autoplot(fb_stock, level=NULL) + 
  labs(title = 'Facebook stock price', y='$US') +
  guides(color=guide_legend(title='Forecast'))

# Text Code
bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)

# Mean method
bricks |> 
  model(MEAN(Bricks)) |>
  forecast(h='5 years') |>
  autoplot(bricks) +
  labs(title='Bricks Mean Forecasting')

# Naive method
bricks |> 
  model(NAIVE(Bricks)) |>
  forecast(h='5 years') |>
  autoplot(bricks) +
  labs(title='Bricks Naive Forecasting')

# Seasonal Naive method
bricks |> 
  model(SNAIVE(Bricks ~ lag("year"))) |>
  forecast(h='5 years') |>
  autoplot(bricks) +
  labs(title='Bricks Seasonal Naive Forecasting')

# Drift method
bricks |> 
  model(RW(Bricks ~ drift())) |>
  forecast(h='5 years') |>
  autoplot(bricks) +
  labs(title='Bricks Drift Forecasting')

# Example: Australian Quarterly Beer Production
# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )

# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> 
  forecast(h = 14)

# Plot forecasts against actual values
beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Example: Google's Daily Stock Price
# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock |> 
  filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)  # giving the forecast function new data

# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))
