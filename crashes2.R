
library(urca)
library(fpp3)
library(readxl)
crashes <- read_excel("Datasets/EBR Daily by Hwy Class.xlsx")
games <- read.csv('Datasets/lsu-schedule-scrape-18-23.csv')
covid <- read_excel('Datasets/covid variable.xlsx')

# ==== Data Import and Weekly Conversion ====

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


# ==== Data Selection and Mutation====
# Select a Highway Class (integer between 1 and 43)
# Note: Not all have good data or create good forecasts

hwy = 20
crashes_w_hwy <- crashes_w |> 
  filter(HighwayClass == hwy) |>
  fill_gaps() |> replace_na(list(crashCount=0))

# ==== Exploration ====

# Plot crashes
crashes_w_hwy |> autoplot(crashCount) +
  labs(title='Crashes on Urban 2-lane in EBR')
# Crashes across seasons
crashes_w_hwy |> 
  gg_season(crashCount)
# Lagged data
crashes_w_hwy |>
  gg_lag(crashCount, geom = 'point', lags = c(5,10,15,20,26,30,35,40,45,50,52,60))
# Subseries
crashes_w_hwy |>
  gg_subseries(crashCount)
# Autocorrelation
crashes_w_hwy |>
  ACF(crashCount,lag_max = 52) |>
  autoplot()
# White noise testing
crashes_w_hwy |> features(crashCount,ljung_box) # Try a specifc lag value if the default is bad. 
# Stationarity testing
crashes_w_hwy |> features(crashCount,list(unitroot_kpss,unitroot_ndiffs,unitroot_nsdiffs))

# ==== Moving Average ====

crashes_w_hwy <- crashes_w_hwy |>
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

crashes_w_hwy |>
  autoplot(crashCount, color='grey') +
  geom_line(aes(y = `5-MA`, color = "5-MA")) +
  geom_line(aes(y = `4-MA`, color = "4-MA")) +
  geom_line(aes(y = `2x4-MA`, color = "2x4-MA")) +
  geom_line(aes(y = `26-MA`, color = "26-MA")) +
  scale_color_manual(values = c("5-MA" = "#D55E00", "4-MA" = "blue", "2x4-MA" = "green","26-MA"="purple")) +
  theme(legend.position = c(0.9, 0.12)) +
  labs(y = 'Crashes',
       title = 'EBR Hwy Class 20 Moving Avg Crashes',
       color = 'Moving Averages')

# ==== Decomposition ====
crashes_w_hwy |>
  model(
    classical_decomposition(crashCount, type = 'multiplicative')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Classical multiplicative decomposition of EBR Hwy Class 20 crashes")
crashes_w_hwy |>
  model(
    classical_decomposition(crashCount, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Classical additive decomposition of EBR Hwy Class 20 crashes")

dcmp <- crashes_w_hwy |> model(classical_decomposition(crashCount,type='additive'))

# Add decomp related vars to the dataset
crashes_w_hwy <- crashes_w_hwy |>
  mutate(dn_crashes = crashCount - components(dcmp)$random,
         season_adjust = components(dcmp)$season_adjust)

# ==== Train-Test Split ====
p = 0.20
k = round(nrow(crashes_w_hwy)*p)
l=nrow(crashes_w_hwy)-k
train = crashes_w_hwy[1:l,]
test = crashes_w_hwy[(l+1):nrow(crashes_w_hwy),]
freq=365.25

# ==== Benchmarks ====
bnch <- train |> 
  model(naive = NAIVE(crashCount),
        snaive = SNAIVE(crashCount),
        drift = RW(crashCount ~ drift()))

bnch |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
bnch |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

bnch_fc <- bnch |>
  forecast(test)

accuracy(bnch_fc,test)

# ======== Model Fit - TSLM w/vars ========
fit_tslm <- train |>
  model(TSLM(crashCount ~ trend() + season() + home + covid))

report(fit_tslm)

fit_tslm |> gg_tsresiduals()

# -=-=-=- Forecast and Test -=-=-=-
fit_tslm |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_tslm |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

tslm_fc <- fit_tslm |>
  forecast(test)
accuracy(tslm_fc,test)


# ======== Model Fit - Plain ARIMA ========
fit_train <- train |> 
  model(stepwise = ARIMA(crashCount),
        search = ARIMA(crashCount, stepwise = FALSE))

glance(fit_train) |> arrange(AICc) |> select(.model:BIC)

fit_train |> select(search) |> gg_tsresiduals()
fit_train |> select(search) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_train |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_train |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_train <- fit_train |>
  forecast(test)
accuracy(fc_fit_train,test)


# ======== Model Fit - ARIMA + Vars ========
fit_ar_vars <- train |> 
  model(stepwise = ARIMA(crashCount ~ season_adjust + home + covid),
        search = ARIMA(crashCount ~ season_adjust + home + covid, stepwise = FALSE),
        stepwise_trend = ARIMA(crashCount ~ trend() + season_adjust + home + covid),
        search_trend = ARIMA(crashCount ~ trend() + season_adjust + home + covid, stepwise = FALSE))

glance(fit_ar_vars) |> arrange(AICc) |> select(.model:BIC)

fit_ar_vars |> select(search) |> gg_tsresiduals()
fit_ar_vars |> select(search) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_ar_vars |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_ar_vars |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_ar_vars <- fit_ar_vars |>
  forecast(test)
accuracy(fc_fit_ar_vars,test)

augment(fit_ar_vars) |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )





