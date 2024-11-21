#=================== Loading Packages and Data, Setting paths ==========================
library(easypackages)
libraries("urca","fpp3","tidyverse")

hwy=20
Crashes_w_hwy=read.csv(paste0("./Data/Preprocessed/Crash_hwy_",hwy,"_Full.csv"))|>
  mutate(Week = yearweek(Week)) |>
  as_tsibble(index = Week, key = HighwayClass)

#=================== Functions =========================================================
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
graph_save <- function(graph,graph_name,graph_path) {
  ggsave(paste0(graph_path,graph_name,".jpg"),graph,width=15,height=8)
}

#=================== Main Analysis =====================================================

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

#=================== Accuracy collection/ Presentation =================================

Combined_tibble <- bind_rows(Ac_List)|> rename(
  Model = .model,
  Proportion = P
)|>mutate(
    Group = case_when(
      Model %in% c("nn_base", "nn_vars") ~ "Neural Networks",
      Model %in% c("arima_full", "arima_plain", "arima_sdif") ~ "ARIMA",
      Model %in% c("tslm_base", "tslm_inter", "tslm_vars") ~ "Regression",
      Model %in% c("drift", "naive", "snaive") ~ "Baseline",
      TRUE ~ "Other"
    )
  ) |>
  mutate(Group = factor(Group, levels = c("Neural Networks", "ARIMA", "Regression", "Baseline")))|>
  mutate(Model = factor(Model, levels = c(
    "nn_base", "nn_vars",
    "arima_full", "arima_plain", "arima_sdif",
    "tslm_base", "tslm_inter", "tslm_vars",
    "drift", "naive", "snaive"
  )))

custom_colors <- c(
  "nn_base" = "#1f77b4", "nn_vars" = "#74a9cf",
  "arima_full" = "#31a354", "arima_plain" = "#74c476", "arima_sdif" = "#a1d99b",
  "tslm_base" = "#ffcc00", "tslm_inter" = "#ffdd55", "tslm_vars" = "#ffee99",
  "drift" = "#de2d26", "naive" = "#fc9272", "snaive" = "#fcbba1"
)

min_rmse_points <- Combined_tibble |>
  group_by(Proportion) |>
  slice_min(order_by = RMSE, n = 1) |>
  ungroup()

#=================== Plotting the accuracies ============================================

p1=ggplot(Combined_tibble, aes(x = Proportion, y = RMSE, color = Model)) +
  geom_point(size = 3) +             
  geom_line(size = 1) +
  geom_point(data = min_rmse_points, aes(x = Proportion, y = RMSE), 
             shape = 4, size = 4, color = "black", inherit.aes = FALSE) +
  geom_text(data = min_rmse_points, aes(x = Proportion, y = RMSE, label = sprintf("%.2f", RMSE)),
            vjust = -0.5, color = "black", inherit.aes = FALSE) +
  scale_color_manual(values = custom_colors) + 
  scale_x_continuous(
    breaks = unique(Combined_tibble$Proportion)
  ) +
  facet_wrap(~ Group, scales = "free", ncol = 1) +  
  labs(
    title = "RMSE by Train-Test Split Proportion",
    subtitle = "Grouped by Model Type",
    x = "Percentage of Test",
    y = "Root Mean Square Error (RMSE)",
    color = "Model"
  ) +
  guides(
    color = guide_legend(
      ncol = 1,                          
      byrow = TRUE,                      
      title = "Model Types",             
      override.aes = list(size = 4)      
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
p1
graph_save(p1,"RMSE_vs_proportion",paste0("./Results/Graphs/RMSE/Crash_Full_hwy_",hwy))

