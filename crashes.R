library(fpp3)
library(readxl)
crashes <- read_excel("EBR Daily by Hwy Class.xlsx")



# convert tibble to tsibble (monthly)
crashes_w <- crashes |>
  mutate(Week = yearweek(CrashDate)) |>
  group_by(Week, HighwayClass) |>
  summarise(
    crashCount = sum(crashCount),
    Pedestrian = sum(Pedestrian),
    Bicycle = sum(Bicycle),
    NonMotorist = sum(NonMotorist),
    Motorcycle = sum(Motorcycle),
    Fatal = sum(Fatal)
  ) |>
  as_tsibble(index = Week, key = HighwayClass)


crashes_w = crashes_w |>
  mutate(PercentFatal = Fatal/crashCount)


crashes_w |>
  filter(! HighwayClass %in% c(6,31,42,29,23,11,9,19,33,4,'NULL')) |>
  fill_gaps() |>
  gg_season(crashCount)


crashes_w |>
  filter(HighwayClass == 20) |>
  autoplot(crashCount)

# Lag
crashes_w |>
  filter(HighwayClass == 20) |>
  ungroup() |>
  gg_lag(crashCount, geom = 'point', lags = c(5,10,15,20,25,30,35,40,45,50,52,60))

# Autocorrelation
crashes_w |>
  filter(HighwayClass == 20) |>
  fill_gaps() |>
  ACF(crashCount, lag_max = 60) |>
  autoplot()

# Transformation????


# Moving Average
crashes_w_20 <- crashes_w |>
  filter(HighwayClass == 20) |>
  fill_gaps() |> replace_na(list(crashCount=0)) |>
  ungroup() |>
  mutate(
    `5-MA` = slider::slide_dbl(crashCount, mean, 
                               .before = 2, .after = 2, .complete = TRUE),
    `4-MA` = slider::slide_dbl(crashCount, mean,
                               .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                                 .before = 1, .after = 0, .complete = TRUE),
    `26-MA` = slider::slide_dbl(crashCount, mean,
                                 .before = 12, .after = 13, .complete = TRUE)
  )


crashes_w_20 |>
  autoplot(crashCount, color='grey') +
  geom_line(aes(y = `5-MA`, color = "5-MA")) +
  geom_line(aes(y = `4-MA`, color = "4-MA")) +
  geom_line(aes(y = `2x4-MA`, color = "2x4-MA")) +
  geom_line(aes(y = `26-MA`, color = "26-MA")) +
  scale_color_manual(values = c("5-MA" = "#D55E00", "4-MA" = "blue", "2x4-MA" = "green","26-MA"="purple")) +
  theme(legend.position = c(0.7, 0.2)) +
  labs(y = 'Crashes',
       title = 'EBR Hwy Class 20 Moving Avg Crashes',
       color = 'Moving Averages')

crashes_w_20 |>
  ungroup() |>
  model(
    classical_decomposition(crashCount, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Classical additive decomposition of EBR Hwy Class 20 crashes")


# Try de-noising time series
# do decomp, then subtract the noise/remainder from data

dcmp <- crashes_w_20 |>
  ungroup() |>
  model(classical_decomposition(crashCount, type = 'multiplicative'))

dn_crashes_w_20 <- crashes_w_20$crashCount - components(dcmp)$random

crashes_w_20 <- crashes_w_20 |>
  mutate(denoise = dn_crashes_w_20)

crashes_w_20 |>
  ungroup() |>
  model(
    classical_decomposition(denoise, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Denoised additive decomposition of EBR Hwy Class 20 crashes")



