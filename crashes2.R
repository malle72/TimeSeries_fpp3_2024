#==================  
library(urca)
library(fpp3)
library(readxl)
crashes <- read_excel("Datasets/EBR Daily by Hwy Class.xlsx")
games <- read.csv('Datasets/lsu-schedule-scrape-18-23.csv')
covid <- read_excel('Datasets/covid variable.xlsx')
holidays <- read.csv('Datasets/holiday_dates_baton_rouge.csv')

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
    Fatal = sum(Fatal),
    iceCrashes = sum(iceCrashes),
    icePresent = ifelse(iceCrashes > 0,1,0)
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

# convert holiday to weekly tsibble
holiday_w <- holidays |>
  mutate(Week = yearweek(date)) |>
  group_by(Week) |>
  summarise(holiday = max(holiday)) |>
  as_tsibble(index=Week) |>
  ungroup()

# ==== Data Combination ====
crashes_w <- merge(crashes_w,games_w,all = TRUE)
crashes_w <- merge(crashes_w,covid_w,all = TRUE)
crashes_w <- merge(crashes_w,holiday_w,all = TRUE)

crashes_w[is.na(crashes_w)] <- 0


# ==== Data Selection and Mutation====
# Select a Highway Class (integer between 1 and 43)
# Note: Not all have good data or create good forecasts

hwy = 20
crashes_w_hwy <- crashes_w |> 
  filter(HighwayClass == hwy) |>
  replace_na(list(crashCount=0))

crashes_w_hwy <- crashes_w_hwy |>
  as_tsibble(index = Week)

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
  ACF(crashCount,lag_max = 104) |>
  autoplot()
# White noise testing
crashes_w_hwy |> features(crashCount,ljung_box,lag=26) # Try a specifc lag value if the default is bad. 
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
# Classical Decomp Multiplicative
crashes_w_hwy |>
  model(
    classical_decomposition(crashCount, type = 'multiplicative')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Classical multiplicative decomposition of EBR Hwy Class 20 crashes")
# Classical Decomp Additive
crashes_w_hwy |>
  model(
    classical_decomposition(crashCount, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  labs(title="Classical additive decomposition of EBR Hwy Class 20 crashes")
# STL Decomp
crashes_w_hwy |>
  model(
    STL(crashCount)
  ) |>
  components() |>
  autoplot() + 
  labs(title="STL decomposition of EBR Hwy Class 20 crashes")

dcmp_add <- crashes_w_hwy |> model(classical_decomposition(crashCount,type='additive'))
dcmp_multi <- crashes_w_hwy |> model(classical_decomposition(crashCount,type='multiplicative'))
stl_dcmp <- crashes_w_hwy |> model(STL(crashCount))

# Add decomp related vars to the dataset
crashes_w_hwy <- crashes_w_hwy |>
  mutate(dn_crashes = crashCount - components(dcmp_add)$random,         # denoised data (additive)
         season_adjust = components(dcmp_add)$season_adjust,            # seasonally adjusted (additive)
         dn_multi = crashCount - components(dcmp_multi)$random,         # denoised data (multi)
         season_adjust_multi = components(dcmp_multi)$season_adjust,    # seasonally adjusted (multi)
         stl_dn_crashes = crashCount - components(stl_dcmp)$remainder,  # STL denoised data
         stl_season_adjust = components(stl_dcmp)$season_adjust)        # STL seasonally adjusted

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

#===========
bnch |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
bnch |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

bnch_fc <- bnch |>
  forecast(test)

Ac1=accuracy(bnch_fc,test)
Ac1

# ======== Model Fit - TSLM w/vars ========
fit_tslm <- train |>
  model(tslm_base = TSLM(crashCount ~ trend() + season() + home + covid + icePresent),
        tslm_inter = TSLM(crashCount ~ trend() * season() + home + covid + icePresent)
)

#===============
glance(fit_tslm) |> arrange(AICc) |> select(.model:BIC)

fit_tslm |> select(tslm_base) |> gg_tsresiduals()
fit_tslm |> select(tslm_inter) |> gg_tsresiduals()


# -=-=-=- Forecast and Test -=-=-=-
fit_tslm |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_tslm |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

tslm_fc <- fit_tslm |>
  forecast(test)
Ac2=accuracy(tslm_fc,test)
Ac2

# ======== Model Fit - Plain ARIMA ========
fit_train <- train |> 
  model(stepwise = ARIMA(crashCount),
        search = ARIMA(crashCount, stepwise = FALSE))

glance(fit_train) |> arrange(AICc) |> select(.model:BIC)
# Determine which model is best and explore it further
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
Ac3=accuracy(fc_fit_train,test)
Ac3

fit_train |> select(search) |>
augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )



# ======== Model Fit - ARIMA w/ Vars ========
fit_ar_vars <- train |> 
  model(searchFull_ic = ARIMA(crashCount ~ trend() + season() + home + covid + iceCrashes),
        searchFull_ip = ARIMA(crashCount ~ trend() + season() + home + covid + icePresent))

glance(fit_ar_vars) |> arrange(AICc) |> select(.model:BIC)

fit_ar_vars |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_ar_vars |> select(searchFull_ic) |> gg_tsresiduals()
fit_ar_vars |> select(searchFull_ic) |>
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
Ac4=accuracy(fc_fit_ar_vars,test)
Ac4


fit_ar_vars |> select(searchFull_ic) |>
augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )

# ======== Model Fit - Seasonal Difference ARIMA ========
fit_sdif <- train |> 
  model(stepwise_sdif = ARIMA(crashCount ~ PDQ(0,0,0) + trend() + season() + home + covid),
        search_sdif = ARIMA(crashCount ~ PDQ(0,0,0) + trend() + season() + home + covid, stepwise = FALSE))

glance(fit_sdif) |> arrange(AICc) |> select(.model:BIC)

fit_sdif |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_sdif |> select(search_sdif) |> gg_tsresiduals()
fit_sdif |> select(search_sdif) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_sdif |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_sdif |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_sdif <- fit_sdif |>
  forecast(test)
Ac5=accuracy(fc_fit_sdif,test)
Ac5

# ======== Model Fit - ARIMA | denoise ~ Vars ========
fit_ar_dn <- train |> 
  model(stepwise_dn = ARIMA(dn_crashes ~ trend() + season() + home + covid),
        search_dn = ARIMA(dn_crashes ~ trend() + season() + home + covid, stepwise = FALSE),
        stepwise_dn_multi = ARIMA(dn_crashes ~ trend() + season() + home + covid),
        search_dn_multi = ARIMA(dn_crashes ~ trend() + season() + home + covid, stepwise = FALSE))

glance(fit_ar_dn) |> arrange(AICc) |> select(.model:BIC)

fit_ar_dn |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_ar_dn |> select(search_dn) |> gg_tsresiduals()
fit_ar_dn |> select(search_dn) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_ar_dn |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_ar_dn |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_ar_dn <- fit_ar_dn |>
  forecast(test)
accuracy(fc_fit_ar_dn,test)

augment(fit_ar_dn) |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = dn_crashes, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )

# ======== Model Fit - ARIMA | stl denoise ~ Vars ========
fit_ar_stl_dn <- train |> 
  model(stepwise_stl_dn = ARIMA(stl_dn_crashes ~ 1 + trend() + season() + home + covid),
        search_stl_dn = ARIMA(stl_dn_crashes ~ 1 + trend() + season() + home + covid, stepwise = FALSE),
        search_stl_dn_full = ARIMA(stl_dn_crashes ~ 1 + trend() + season() + home + covid + holiday, stepwise = FALSE),
        search_stl_dn_no_cov = ARIMA(stl_dn_crashes ~ 1 + trend() + season() + home, stepwise = FALSE))

glance(fit_ar_stl_dn) |> arrange(AICc) |> select(.model:BIC)

fit_ar_stl_dn |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_ar_stl_dn |> select(search_stl_dn_no_cov) |> gg_tsresiduals()
fit_ar_stl_dn |> select(search_stl_dn_no_cov) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_ar_stl_dn |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_ar_stl_dn |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_ar_stl_dn <- fit_ar_stl_dn |>
  forecast(test)
accuracy(fc_fit_ar_stl_dn,test)

fit_ar_stl_dn |> select(search_stl_dn_no_cov) |>
augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = stl_dn_crashes, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )


# ======== Model Fit - Neural Net ========
fit_nn <- train |> 
  model(nn_base = NNETAR(crashCount))

glance(fit_nn) |> arrange(AICc) |> select(.model:BIC)

fit_nn |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_nn |> select(nn_base) |> gg_tsresiduals()
fit_nn |> select(nn_base) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_nn |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_nn |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_nn <- fit_nn |>
  forecast(test)
accuracy(fc_fit_nn,test)

fit_nn |> select(searchFull_ic) |>
  augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )


