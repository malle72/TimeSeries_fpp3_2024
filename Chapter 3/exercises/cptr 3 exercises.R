# Chapter 3 Exercises

# 3
# Why is a Box-Cox transformation unhelpful for the canadian_gas data?

canadian_gas |> autoplot(Volume)

cg_lambda <- canadian_gas |>
  features(Volume, features = guerrero)|>
  pull(lambda_guerrero)

canadian_gas |>
  autoplot(box_cox(Volume,cg_lambda))

dcmp <- canadian_gas |>
  model(classical_decomposition(Volume))

components(dcmp) |>
  autoplot()

# Very high seasonality in the data, which is not addressed by Box-Cox

# 7
# Last 5 years of gas data
gas <- tail(aus_production, 5*4) |> select(Gas)
gas_full <- aus_production |> select(Gas)

# a
gas_full |>
  autoplot(Gas)
# Strong seasonality
# Upward trend-cycle

# b
dcmp_gas <- gas |>
  model(classical_decomposition(Gas,type='multiplicative'))

# c
components(dcmp_gas) |>
  autoplot()
# Results from this graph reinforce the interpretations from #a

# d
components(dcmp_gas) |>
  autoplot(season_adjust)
# Showing the trend in the data with the seasonal component removed

# e
gas_out <- gas |>
  mutate(Gas = ifelse(row_number() == 1, Gas+300,Gas))
# Selected an arbitrary observation to turn into an outlier

# Recompute decomposition
dcmp_gas_out <- gas_out |>
  model(classical_decomposition(Gas,type='multiplicative'))

# Plot new seasonal adj data
components(dcmp_gas_out) |>
  autoplot(season_adjust)
components(dcmp_gas_out) |>
  autoplot()
# Large spike in the middle of the data. 
# Outlier clearly masks trend and effect of rest of the data.
# Outliers at indexes 1, 12, 19, and 20 were observed to see if 
#   position within data had an effect on how the outlier affected
#   the observable trend.
# Conclusion: It did not matter where in the data the outlier was placed.
#   This specific outlier (obs+300) masked the overall trend in the data
#   no matter where it was placed. 

# 9
# There is a very strong upward trend as well as a strong 
#   seasonal component across each year. The seasons might
#   also be sub-yearly. Jan, Aug, Oct, and Nov seem to contain 
#   most of the seasonal job losses. Mar, Sept, and Dec seem
#   to have most of the seasonal job gains. 

# The recession of 1991/1992 is very evident in the remainder 
#   component graph. It is also apparent that it affected the 
#   seasonal graphs as several of the months have sharp dips 
#   around the early 90s. 





