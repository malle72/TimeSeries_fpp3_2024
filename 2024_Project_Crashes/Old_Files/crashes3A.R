# ---- Regular Crash Count ----
# Forecasting Regular Crash Counts/comparing multiple models/one train test split/

library(urca)
library(fpp3)
library(readxl)
crashes <- read_excel("Datasets/EBR Daily by Hwy Class.xlsx")
games <- read.csv('Datasets/lsu-schedule-scrape-18-23.csv')
covid <- read_excel('Datasets/covid variable.xlsx')
holidays <- read.csv('Datasets/holiday_dates_baton_rouge.csv')
graph_path3a <- "graphs/3a/"

graph_save <- function(graph,graph_name,graph_path) {
  ggsave(paste0(graph_path,graph_name,".jpg"),graph,width=15,height=8)
}

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

cc_byDay <- crashes |> filter(HighwayClass == 20) |>
  mutate(CrashDate = date(CrashDate)) |>
  as_tsibble(index=CrashDate) |>
  autoplot(crashCount)
cc_byDay
graph_save(cc_byDay,'CrashCountByDay',graph_path3a)
# Plot crashes
tot_crash<-crashes_w_hwy |> autoplot(crashCount) +
  labs(title='Crashes on Urban 2-lane in EBR')
tot_crash
graph_save(tot_crash,'TotalCrashes',graph_path3a)
# Lagged data
crashes_w_hwy |>
  gg_lag(crashCount, geom = 'point', lags = c(5,10,15,20,26,30,35,40,45,50,52,60))
# Autocorrelation
cc_ac <- crashes_w_hwy |>
  ACF(crashCount,lag_max = 104) |>
  autoplot()
cc_ac
graph_save(cc_ac, 'CrashCountAutoCorrelation',graph_path3a)
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

cc_ma <- crashes_w_hwy |>
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
cc_ma
graph_save(cc_ma,'MovingAvgs',graph_path3a)

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
bnch3a <- train |> 
  model(naive = NAIVE(crashCount),
        snaive = SNAIVE(crashCount),
        drift = RW(crashCount ~ drift()))

bnch3a |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
bnch3a |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

bnch_fc_3a <- bnch3a |>
  forecast(test)

ac_bnch_3a<-accuracy(bnch_fc_3a,test)
ac_bnch_3a
# ======== Model Fit - TSLM w/vars ========

fit_tslm_3a <- train |>
  model(tslm_base = TSLM(crashCount ~ trend() + season()),
        tslm_inter = TSLM(crashCount ~ trend() + season() + trend() * season() + home + covid + icePresent),
        tslm_vars = TSLM(crashCount ~ trend() + season() + home + covid + holiday + icePresent)
)

glance(fit_tslm_3a) |> arrange(AICc) |> select(.model:BIC)

fit_tslm_3a |> select(tslm_base) |> gg_tsresiduals()
fit_tslm_3a |> select(tslm_inter) |> gg_tsresiduals()
fit_tslm_3a |> select(tslm_vars) |> gg_tsresiduals()



# -=-=-=- Forecast and Test -=-=-=-
fit_tslm_3a |> select(tslm_vars) |>
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_tslm_3a |> select(tslm_vars) |>
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

tslm_fc_3a <- fit_tslm_3a |>
  forecast(test)
ac_tslm_3a <- accuracy(tslm_fc_3a,test) |> arrange(RMSE)
ac_tslm_3a

# ======== Model Fit - Plain ARIMA ========
fit_train_3a <- train |> 
  model(stepwise = ARIMA(crashCount),
        search = ARIMA(crashCount, stepwise = FALSE))

glance(fit_train_3a) |> arrange(AICc) |> select(.model:BIC)
# Determine which model is best and explore it further
fit_train_3a |> select(search) |> gg_tsresiduals()
fit_train_3a |> select(search) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_train_3a |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_train_3a |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_train_3a <- fit_train_3a |>
  forecast(test)
accuracy(fc_fit_train_3a,test)

fit_train_3a |> select(search) |>
augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )



# ======== Model Fit - ARIMA w/ Vars ========
fit_ar_vars_3a <- train |> 
  model(searchFull_ip = ARIMA(crashCount ~ trend() + season() + home + covid + holiday + icePresent))

glance(fit_ar_vars_3a) |> arrange(AICc) |> select(.model:BIC)

fit_ar_vars_3a |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_ar_vars_3a |> select(searchFull_ip) |> gg_tsresiduals()
fit_ar_vars_3a |> select(searchFull_ip) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_ar_vars_3a |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_ar_vars_3a |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_ar_vars_3a <- fit_ar_vars_3a |>
  forecast(test)
accuracy(fc_fit_ar_vars_3a,test)

fit_ar_vars_3a |> select(searchFull_ip) |>
augment() |>
  ggplot(aes(x = Week)) +
  geom_line(aes(y = crashCount, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  )

# ======== Model Fit - Seasonal Difference ARIMA ========
fit_sdif_3a <- train |> 
  model(stepwise_sdif = ARIMA(crashCount ~ PDQ(0,0,0) + trend() + season() + home + covid + holiday, stepwise = TRUE),
        search_sdif = ARIMA(crashCount ~ PDQ(0,0,0) + trend() + season() + home + covid + holiday, stepwise = FALSE))

glance(fit_sdif_3a) |> arrange(AICc) |> select(.model:BIC)

fit_sdif_3a |> augment() |>
  features(.innov,ljung_box)
# Determine which model is best and explore it further
fit_sdif_3a |> select(search_sdif) |> gg_tsresiduals()
fit_sdif_3a |> select(search_sdif) |>
  augment() |>
  features(.innov,ljung_box,lag=26)

# -=-=-=- Forecast and Test -=-=-=-
fit_sdif_3a |> 
  forecast(test) |>
  autoplot(train,level=NULL)  # plot ahead from training
fit_sdif_3a |> 
  forecast(test) |>
  autoplot(bind_rows(train,test),level=NULL)  # plot with full data

fc_fit_sdif_3a <- fit_sdif_3a |>
  forecast(test)
accuracy(fc_fit_sdif_3a,test)


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


