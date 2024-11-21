# Chapter 2 Exercises

# 1
#Aus prod

aus_production |>
  autoplot(Bricks)

?aus_production

# 2
# gafa stock
?gafa_stock
gafa_stock |>
  filter(Symbol == 'AAPL') |>
  autoplot(Close)

# 3
# (remember to set working directory)
tute1 <- readr::read_csv("tute1.csv")
View(tute1)

mytimeseries <- tute1 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(index = Quarter)

mytimeseries |>
  pivot_longer(-Quarter) |>
  ggplot(aes(x = Quarter, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y")

# 4 
us_ts <- us_total |>
  as_tsibble(index=year, key=state)

us_ne <- us_ts |>
  filter(state %in% c('Maine', 'Vermont', 'New Hampshire', 'Massachusetts', 'Connecticut', 'Rhode Island'))

us_ne |>
  autoplot(y)

# 5
tourism2 <- read_excel('exercises/tourism.xlsx')

tourism2 <- tourism2 |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(key=c(Region, State, Purpose), index=Quarter)


# 6
aus_arrivals |>
  autoplot(Arrivals)
# Overall arrivals from japan began decreasing around the late 90s
# Arrivals from UK peak in mid 2000s
# Arrivals from NZ continue upward trend

aus_arrivals |>
  gg_season(Arrivals)
# huge dip in UK arrivals in Q2 and Q3. 
#   Likely because people are vacationing during inverted summer

aus_arrivals |>
  gg_subseries(Arrivals)
# Q2 and Q4 from japan have odd dips in early 2000s
# Q2 and Q3 from the US in 2000 spike due to the summer Olympics in Sydney
# Q2 from US continues to spike in 2001, likely due to latent tourism from Olympics

# 7
set.seed(8675309)
my_retail <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
# Takeaway food services in Queensland

my_retail |>
  autoplot(Turnover) +
  geom_vline(xintercept=yearmonth('2012 Jan', abbr=TRUE))
# Steady upward trend 
# Trend may have leveled off by early/mid 2010s 
# Variance increases at leveling off point
# Seasonal pattern might be emerging at end of data.

my_retail |>
  gg_season(Turnover) 


my_retail |>
  gg_subseries(Turnover)

my_retail |>
  gg_lag(Turnover,lags=1:12)

my_retail |>
  ACF(Turnover) |>
  autoplot()
# Very obvious trend with maybe a little bit of seasonality. 

my_retail |>
  filter(year(Month) >= 2012) |>
  gg_lag(Turnover,lags=1:12)

my_retail |>
  filter(year(Month) >= 2012) |>
  ACF(Turnover) |>
  autoplot()
# compared to the ACF for the full dataset, this looks more like a seasonal ACF

# 9

# 1 -> B
# 2 -> A
# 3 -> D
# 4 -> C



