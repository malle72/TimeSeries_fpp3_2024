library(fpp3)
library(ggplot2)
library(feasts)
library(glue)
library(GGally)
library(broom)


tourism |>
  features(Trips, list(mean=mean)) |>
  arrange(mean)

tourism |>
  features(Trips,quantile)

tourism |> features(Trips, feat_acf)

# Manually checking sum of squares of first 10 ACs.
tourism |>
  filter(Region == 'Adelaide', Purpose=='Business') |>
  ACF(Trips)
  

# ==== 4.3 STL Features ====

# Finding time series with strongest trend 
tourism |>
  features(Trips, feat_stl) |>
  arrange(desc(trend_strength))
# Aus Coral Coast, Western Aus, Business

west_aus_biz <- tourism |>
  filter(Region=="Australia's Coral Coast",State=='Western Australia', Purpose=='Business')

west_aus_biz_model <- west_aus_biz |>
  model(
    STL(Trips ~ trend(window=7) + season(window='periodic'),
        robust=T))
wab_dcmp <- west_aus_biz_model |>
  components()

wab_dcmp |>
  autoplot()
# Has strong variation in trend, but it's mostly due to bump at the end. 
#  Not due to an actual long term trend. 

# Back to notes

tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

# Finding the most seasonal series 
tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

# ==== 4.5 Exploring Australian Tourism Data ====

# Calculate all of the features available in the feasts package
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features # this is a LOT of calculations


tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>  # selects only the season features
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))

# Principle Components breakdown by Purpose
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

# Identify outliers in PC analysis
outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers

# Display data from outlier observations
outliers |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  mutate(Series = glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")
