library(fpp3)
library(ggplot2)
library(feasts)
library(glue)
library(GGally)
library(broom)



tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))

tourism_features |>
  filter(Purpose == "Holiday") |>
  select_at(vars(contains("season"))) |>
  mutate(
  seasonal_peak_year = seasonal_peak_year +
    4*(seasonal_peak_year==0),
  seasonal_trough_year = seasonal_trough_year +
    4*(seasonal_trough_year==0),
  seasonal_peak_year = glue("Q{seasonal_peak_year}"),
  seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs()

# 2
# What is the peak quarter for holidays in each state?



# 3
pbs_features <- PBS |>
  features(Cost,feature_set(pkgs='feasts'))
pbs_features

pbs_pcs <- pbs_features |>
  select(-Concession, -Type, -ATC1, -ATC2, -`...26`) |>
  select(where(~!any(is.na(.)))) |>
  prcomp(scale=T) |>
  augment(pbs_features)
pbs_pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Type)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pbs_pcs |>
  filter(abs(.fittedPC1) > 10 | abs(.fittedPC2) > 10) |>
  select(Concession, Type, ATC1, ATC2, .fittedPC1, .fittedPC2)
outliers

# Display data from outlier observations
outliers |>
  left_join(tourism, by = c("Concession", "Type", "ATC1", "ATC2"), multiple = "all") |>
  mutate(Series = glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) |>
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")

PBS |>
  filter(Concession == 'Concessional' & Type=='Co-payments' & ATC1 == 'C' & ATC2 == 'C10') |>
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")
