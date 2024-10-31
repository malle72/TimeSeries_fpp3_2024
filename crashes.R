
library(fpp3)
library(readxl)
crashes <- read_excel("Datasets/EBR Daily by Hwy Class.xlsx")
games <- read.csv('Datasets/lsu-schedule-scrape-18-23.csv')
covid <- read_excel('Datasets/covid variable.xlsx')

# ==== Data Import ====

# convert tibble to tsibble (weekly)
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
  as_tsibble(index = Week, key = HighwayClass) |>
  ungroup()

# convert games to weekly tsibble
games_w <- games |>
  mutate(Week = yearweek(date),
         home = ifelse(location == 'Home',1,0)) |>
  group_by(Week) |>
  summarise(home = sum(home)) |>
  as_tsibble(index=Week) |>
  ungroup()

# convert covid to weekly tsibble
covid_w <- covid |>
  mutate(Week = yearweek(date)) |>
  group_by(Week) |>
  summarise(covid = max(covid)) |>
  as_tsibble(index=Week) |>
  ungroup()


# ==== Data Combination ====
crashes_w <- merge(crashes_w,games_w,all = TRUE)
crashes_w <- merge(crashes_w,covid_w,all = TRUE)
crashes_w[is.na(crashes_w)] <- 0

crashes_w <- crashes_w |>
  as_tsibble(index = Week, key = HighwayClass)

# ==== Filtering ====
crashes_w = crashes_w |>
  mutate(PercentFatal = Fatal/crashCount)


crashes_w |>
  filter(! HighwayClass %in% c(6,31,42,29,23,11,9,19,33,4,'NULL')) |>
  fill_gaps() |>
  gg_season(crashCount) # Big slow graph; caution running


crashes_w |>
  filter(HighwayClass == 20) |>
  autoplot(crashCount)

# Lag
crashes_w |>
  filter(HighwayClass == 20) |>
  ungroup() |>
  gg_lag(crashCount, geom = 'point', lags = c(5,10,15,20,26,30,35,40,45,50,52,60))

# Autocorrelation
crashes_w |>
  filter(HighwayClass == 20) |>
  fill_gaps() |>
  ACF(crashCount, lag_max = 60) |>
  autoplot()

# Transformation????

crashes_w_20 <- crashes_w |>
  filter(HighwayClass == 20)

# Moving Average
crashes_w_20 <- crashes_w_20|>
  fill_gaps() |> replace_na(list(crashCount=0)) |>
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



# Plotting Moving Averages
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

# Decomposition
crashes_w_20 |>
  model(
    classical_decomposition(crashCount, type = 'multiplicative')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Classical multiplicative decomposition of EBR Hwy Class 20 crashes")


# ==== De-noising ====
# do decomp, then subtract the noise/remainder from data

dcmp <- crashes_w_20 |>
  model(classical_decomposition(crashCount, type = 'additive'))

# Create array of crashCount minus random component from decomp
dn_crashes_w_20 <- crashes_w_20$crashCount - components(dcmp)$random

# Insert denoised column to tsibble
crashes_w_20 <- crashes_w_20 |>
  mutate(denoise = dn_crashes_w_20)

# Redo model with denoised data
crashes_w_20 |>
  model(
    classical_decomposition(denoise, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Denoised additive decomposition of EBR Urban 2-Lane crashes")

# ===== De-noise with STL =====
dcmp_stl <- crashes_w_20 |>
  model(STL(crashCount ~ trend() + season(), robust=T)) # this model by itself didn't look bad

crashes_w_20 <- crashes_w_20 |>
  mutate(denoise_stl = crashes_w_20$crashCount - components(dcmp_stl)$remainder)

crashes_w_20 |>
  model(STL(denoise_stl ~ trend() + season(), robust=T)) |>
  components() |>
  autoplot() + 
  labs(title="Denoised STL decomposition of EBR Urban 2-Lane crashes")

# ==== De-trending ====

# Create array of crashCount minus trend component from decomp
# Insert detrended column to tsibble
crashes_w_20 <- crashes_w_20 |>
  mutate(detrend = crashes_w_20$crashCount - components(dcmp)$trend)

# Redo model with denoised data
crashes_w_20 |>
  model(
    classical_decomposition(detrend, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="De-trended additive decomposition of EBR Hwy Class 20 crashes")

# ==== Seasonal Adjustment ====

crashes_w_20 <- crashes_w_20 |>
  mutate(season_adjust = components(dcmp)$season_adjust)

crashes_w_20 |>
  model(
    classical_decomposition(season_adjust, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Seasonally Adjusted additive decomposition of EBR Hwy Class 20 crashes")


# ==== Decomp Forecasting ====

fit_dcmp <- crashes_w_20 |>
  model(stlf = decomposition_model(
    STL(crashCount ~ season(window = "periodic") + trend(window = 26), robust = FALSE),
    SNAIVE(crashCount)
  ))


fc <- fit_dcmp |>
  forecast(h=10) 

fc|>
  autoplot(crashes_w_20)+
  labs(y = "Crashes",
       title = "EBR Urban 2-Lane Crashes")

fit_dcmp |> gg_tsresiduals()

augment(fit_dcmp) |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "crashes",
       title = "EBR Urban 2-Lane Crashes") +
  guides(colour = guide_legend(title = "Series"))



# ==== Fourier Analysis ====
fourier_crash <- crashes_w_20 |>
  model(TSLM(crashCount ~ trend() + fourier(K = 2)))
report(fourier_crash)

fcfc <- fourier_crash |>
  forecast(h=10)

fcfc |> 
  autoplot(crashes_w_20)

fourier_crash |> gg_tsresiduals()

augment(fourier_crash) |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "crashes",
       title = "EBR Urban 2-Lane Crashes Fourier Model") +
  guides(colour = guide_legend(title = "Series"))

# ==== Exigent Variables ====

fit_game <- crashes_w_20 |> 
  model(TSLM(crashCount ~ trend() + season_adjust + home + covid))

report(fit_game)

fit_game |> gg_tsresiduals()

augment(fit_game) |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "crashes",
       title = "EBR Urban 2-Lane Crashes") +
  guides(colour = guide_legend(title = "Series"))








