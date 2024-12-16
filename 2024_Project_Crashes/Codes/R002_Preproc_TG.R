#=================== Loading Packages and Data, Setting paths ==========================
library(easypackages)
libraries("urca","fpp3","tidyverse","readxl","lubridate")

Crashes <- read_excel("./Data/EBR Daily by Hwy Class.xlsx")
Games <- read.csv('./Data/lsu-schedule-scrape-18-23.csv')
Covid <- read_excel('./Data/covid variable.xlsx')
Holidays <- read.csv('./Data/holiday_dates_baton_rouge.csv')

graph_save <- function(graph,graph_name,graph_path) {
  ggsave(paste0(graph_path,graph_name,".png"),graph,width=15,height=8)
}

#=================== Data Pre-processing ==============================================

# Convert to Weekly Data Tibbles

#===== Crashes 
Crashes_w <- Crashes |>
  mutate(CrashDate = as.Date(CrashDate, format = "%Y-%m-%d"))|> 
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
  separate(Week, into = c("Year", "Week01"), sep = " ", remove = FALSE)|>
  as_tsibble(index = Week, key = HighwayClass) |>
  ungroup()

#===== Games
Games_w <- Games |>
  mutate(Week = yearweek(date),
         home = ifelse(location == 'Home',1,0)) |>
  group_by(Week) |>
  summarise(home = sum(home)) |>
  as_tsibble(index=Week) |>
  ungroup()

#===== Covid 
Covid_w <- Covid |>
  mutate(Week = yearweek(date)) |>
  group_by(Week) |>
  summarise(covid = max(covid)) |>
  as_tsibble(index=Week) |>
  ungroup()

#===== Holiday 
Holiday_w <- Holidays |>
  mutate(Week = yearweek(date)) |>
  group_by(Week) |>
  summarise(holiday = max(holiday)) |>
  as_tsibble(index=Week) |>
  ungroup()

#===== Data Combination
tables_list=list(Crashes_w,Games_w,Covid_w,Holiday_w)
Crashes_w <- tables_list |> 
  reduce(left_join, by = c("Week"))|>
  mutate(across(everything(), ~ replace_na(., 0)))


#===== Data Selection and Mutation
# Select a Highway Class (integer between 1 and 43)
# Note: Not all have good data or create good forecasts
hwy = 20

Crashes_w_hwy <- Crashes_w |> 
  filter(HighwayClass == hwy) |>
  as_tsibble(index = Week)

#===== Moving Average
Crashes_w_hwy <- Crashes_w_hwy |>
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

#=================== Decompositions ====================================================
graph_path="./Results/Graphs/Decomposition/"
Crashes_w_hwy <- Crashes_w_hwy |>
  mutate(
    Date = yearweek(Week, week_start = 1) # Convert to yearweek format
  )
year_breaks <- Crashes_w_hwy |>
  filter(str_detect(Week, "W01")) |>
  pull(Date)

CDM_Crashes_w_hwy <- Crashes_w_hwy |>
  model(
    classical_decomposition(crashCount, type = 'multiplicative')
  ) |>
  components() |>
  autoplot() +
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +
  labs(title = "Classical multiplicative decomposition of EBR Hwy Class 20 crashes")+
  theme_bw()

CDM_Crashes_w_hwy

graph_save(CDM_Crashes_w_hwy,'Crashes_DecompMult_w_hwy',graph_path)

# Classical Decomp Additive
CDA_Crashes_w_hwy=Crashes_w_hwy |>
  model(
    classical_decomposition(crashCount, type = 'additive')
  ) |>
  components() |>
  autoplot() + 
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +
  labs(title="Classical additive decomposition of EBR Hwy Class 20 crashes")+
  theme_bw()
CDA_Crashes_w_hwy
graph_save(CDA_Crashes_w_hwy,'Crashes_DecompAdd_w_hwy',graph_path)

# STL Decomp
CDSTL_Crashes_w_hwy=Crashes_w_hwy |>
  model(
    STL(crashCount)
  ) |>
  components() |>
  autoplot() + 
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +
  labs(title="STL decomposition of EBR Hwy Class 20 crashes")+
  theme_bw()
graph_save(CDSTL_Crashes_w_hwy,'Crashes_DecompSTL_w_hwy',graph_path)

dcmp_add <- Crashes_w_hwy |> model(classical_decomposition(crashCount,type='additive'))
dcmp_multi <- Crashes_w_hwy |> model(classical_decomposition(crashCount,type='multiplicative'))
stl_dcmp <- Crashes_w_hwy |> model(STL(crashCount))

# Add decomp related vars to the dataset
Crashes_w_hwy <- Crashes_w_hwy |>
  mutate(dn_crashes = crashCount - components(dcmp_add)$random,         # denoised data (additive)
         season_adjust = components(dcmp_add)$season_adjust,            # seasonally adjusted (additive)
         dn_multi = crashCount - components(dcmp_multi)$random,         # denoised data (multi)
         season_adjust_multi = components(dcmp_multi)$season_adjust,    # seasonally adjusted (multi)
         stl_dn_crashes = crashCount - components(stl_dcmp)$remainder,  # STL denoised data
         stl_season_adjust = components(stl_dcmp)$season_adjust)        # STL seasonally adjusted

#=================== Data Exploration Including Graphs =================================
graph_path="./Results/Graphs/Exploration/"

cc_byDay <- Crashes |> filter(HighwayClass == 20) |>
  mutate(CrashDate = date(CrashDate)) |>
  as_tsibble(index=CrashDate) |>
  autoplot(crashCount)+ 
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +theme_bw()
cc_byDay
graph_save(cc_byDay,'CrashCountByDay',graph_path)

Dn_Graph <- Crashes_w_hwy |> filter(HighwayClass == 20) |> filter(!is.na(dn_crashes))|>
  autoplot(dn_crashes)+ 
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +theme_bw()
Dn_Graph
graph_save(Dn_Graph,'Denoised_Crashes',graph_path)

# Plot crashes
tot_crash<-Crashes_w_hwy |> autoplot(crashCount) +
  labs(title='Crashes on Urban 2-lane in EBR')+ 
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +theme_bw()
tot_crash
graph_save(tot_crash,'TotalCrashes',graph_path)

# Lagged data
Lagged_Crashes=Crashes_w_hwy |>
  gg_lag(crashCount, geom = 'point', lags = c(5,10,15,20,26,30,35,40,45,50,52,60))
Lagged_Crashes
graph_save(Lagged_Crashes,'LaggedCrashes',graph_path)

# Autocorrelation
cc_ac <- Crashes_w_hwy |>
  ACF(crashCount,lag_max = 104) |>
  autoplot()+theme_bw()
cc_ac
graph_save(cc_ac, 'CrashCountAutoCorrelation',graph_path)

# White noise testing
Crashes_w_hwy |> features(crashCount,ljung_box,lag=26) # Try a specifc lag value if the default is bad. 
# Stationarity testing
Crashes_w_hwy |> features(crashCount,list(unitroot_kpss,unitroot_ndiffs,unitroot_nsdiffs))

# Moving average graphs
cc_ma <- Crashes_w_hwy |>
  autoplot(crashCount, color='grey') +
  geom_line(aes(y = `5-MA`, color = "5-MA")) +
  geom_line(aes(y = `4-MA`, color = "4-MA")) +
  geom_line(aes(y = `2x4-MA`, color = "2x4-MA")) +
  geom_line(aes(y = `26-MA`, color = "26-MA")) +
  scale_color_manual(values = c("5-MA" = "#D55E00", "4-MA" = "blue", "2x4-MA" = "green","26-MA"="purple")) +
  theme(legend.position.inside  = c(0.9, 0.12)) +
  labs(y = 'Crashes',
       title = 'EBR Hwy Class 20 Moving Avg Crashes',
       color = 'Moving Averages')+ 
  scale_x_yearweek(
    name = "Year",
    breaks = year_breaks,   # Use only first weeks of each year
    labels = year(year_breaks) # Extract only the year for labeling
  ) +theme_bw()
cc_ma
graph_save(cc_ma,'MovingAvgs',graph_path)

#=================== Saving Combined tibble ===============

write.csv(Crashes_w_hwy,paste0("./Data/Preprocessed/Crash_hwy_",hwy,"_Full.csv"),row.names = F)
