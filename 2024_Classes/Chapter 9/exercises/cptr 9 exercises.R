
# 1a)
#  All figures shown appear to be white noise.
# 1b) 
#  The critical value gets adjusted based on the number of observations (Bonferroni)
#  The autocorrelations are different because they're random data. 

# 2) 
amzn <- gafa_stock |>
  filter(Symbol == 'AMZN')

amzn |>
  autoplot(Close)
# there is a clear trend in this data, therefor, not stationary

amzn |> 
  ACF(Close) |>
  autoplot()
amzn |>
  PACF(Close) |>
  autoplot()
  
