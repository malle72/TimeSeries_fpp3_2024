#=================== Loading Packages and Data, Setting paths ==========================
library(easypackages)
libraries("urca","fpp3","tidyverse")

hwy=20
Crashes_w_hwy=read.csv(paste0("./Data/Preprocessed/Crash_hwy_",hwy,"_Full.csv"))|>
  mutate(Week = yearweek(Week)) |>
  as_tsibble(index = Week, key = HighwayClass)

ttsplit=function(p,dataset) {
    # Ensure p is between 0 and 1
    if (p < 0 || p > 1) {
      stop("Proportion 'p' must be between 0 and 1.")
    }
    k <- floor((1 - p) * nrow(dataset))
    Train <- dataset[1:k, ]
    Test <- dataset[(k + 1):nrow(dataset), ]
    return(list(Train = Train, Test = Test))
  }
  

#=================== Train-Test Split ==================================================

P=seq(0.05,0.3,0.05)
Ac_List=list()

for (i in (1:length(P))){
p = P[i]
splits=ttsplit(p,Crashes_w_hwy)
Train <- splits$Train
Test <- splits$Test

# ==== Model Fits ====

fit_full <- Train |>
  model(naive = NAIVE(crashCount),
        snaive = SNAIVE(crashCount),
        drift = RW(crashCount ~ drift()),
        tslm_base = TSLM(crashCount ~ trend() + season()),
        tslm_inter = TSLM(crashCount ~ trend() + season() + trend() * season() + home + covid + icePresent),
        tslm_vars = TSLM(crashCount ~ trend() + season() + home + covid + holiday + icePresent),
        arima_plain = ARIMA(crashCount, stepwise = TRUE),
        arima_full = ARIMA(crashCount ~ trend() + season() + home + covid + holiday + icePresent, stepwise = TRUE),
        arima_sdif = ARIMA(crashCount ~ PDQ(0,0,0) + trend() + season() + home + covid + holiday, stepwise = TRUE),
        nn_base = NNETAR(crashCount, n_networks = 10),
        nn_vars = NNETAR(crashCount~ home + covid + holiday + icePresent, n_networks = 10)
  )


# ==== Model Evaluation ====
glance(fit_full) |> arrange(AICc) |> select(.model:BIC)
accuracy(fit_full)

for_full <- fit_full |> forecast(Test)
ac_full <- accuracy(for_full,Test) |> arrange(RMSE)
ac_full=ac_full|>mutate(P=p)



Ac_List[[i]]=ac_full|>select(.model,RMSE,P)
dir.create(paste0("./Results/Accuracies/Full/Hwy_",hwy))
write.csv(ac_full,paste0("./Results/Accuracies/Full/Hwy_",hwy,"/TTsp_",p,".csv"))
print(paste0("Finished p=",p))
}

