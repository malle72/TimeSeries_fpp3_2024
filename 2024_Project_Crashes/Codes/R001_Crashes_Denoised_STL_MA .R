#=================== Loading Packages and Data, Setting paths ==========================
library(easypackages)
libraries("urca","fpp3","tidyverse")

hwy=20
Crashes_w_hwy=read.csv(paste0("./Data/Preprocessed/Crash_hwy_",hwy,"_Full.csv"))|>
  mutate(Week = yearweek(Week)) |>
  as_tsibble(index = Week, key = HighwayClass)|>filter(!is.na(dn_crashes))

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
Mod_List_stl_dn=list()
dir.create(paste0("./Results/Accuracies/Denoised_STL/Hwy_",hwy))


for (i in (1:length(P))){
  p = P[i]
  splits=ttsplit(p,Crashes_w_hwy)
  Train <- splits$Train
  Test <- splits$Test

# ==== Model Fits ====
  print(paste0("Working on p=",p))

  fit_full <- Train |>
    model(naive = NAIVE(stl_dn_crashes),
          snaive = SNAIVE(stl_dn_crashes),
          drift = RW(stl_dn_crashes ~ drift()),
          tslm_base = TSLM(stl_dn_crashes ~ trend() + season()),
          tslm_inter = TSLM(stl_dn_crashes ~ trend() + season() + trend() * season() + home + covid + icePresent),
          tslm_vars = TSLM(stl_dn_crashes ~ trend() + season() + home + covid + holiday + icePresent),
          tslm_vars_no_comp = TSLM(stl_dn_crashes ~ home + covid + holiday + icePresent),
          arima_plain = ARIMA(stl_dn_crashes, stepwise = TRUE),
          arima_full = ARIMA(stl_dn_crashes ~ trend() + season() + home + covid + holiday + icePresent, stepwise = TRUE),
          arima_no_comp = ARIMA(stl_dn_crashes ~ trend() + season() + home + covid + holiday + icePresent, stepwise = TRUE),
          arima_sdif = ARIMA(stl_dn_crashes ~ PDQ(0,0,0) + trend() + season() + home + covid + holiday, stepwise = TRUE),
          # nn_base = NNETAR(stl_dn_crashes, n_networks = 10),
          # nn_vars = NNETAR(stl_dn_crashes~ home + covid + holiday + icePresent, n_networks = 10)
    )
  Mod_List_stl_dn[[as.character(p)]] <- fit_full
  

  # ==== Model Evaluation ====
  glance(fit_full) |> arrange(AICc) |> select(.model:BIC)
  accuracy(fit_full)
  
  print(paste0("Forecasting p=",p))
  for_full <- fit_full |> forecast(Test)
  ac_full <- accuracy(for_full,Test) |> arrange(RMSE)
  ac_full=ac_full|>mutate(P=p)
  
  
  Ac_List[[i]]=ac_full|>select(.model,RMSE,P)
  write.csv(ac_full,paste0("./Results/Accuracies/Denoised_STL/Hwy_",hwy,"/TTsp_",p,".csv"))
  print(paste0("Finished p=",p))
}

#=================== Accuracy collection/ Presentation =================================

Combined_tibble <- bind_rows(Ac_List)|> rename(
  Model = .model,
  Proportion = P
)|>mutate(
    Group = case_when(
      Model %in% c("nn_base", "nn_vars") ~ "Neural Networks",
      Model %in% c("arima_full", "arima_plain", "arima_sdif",'arima_no_comp') ~ "ARIMA",
      Model %in% c("tslm_base", "tslm_inter", "tslm_vars","tslm_vars_no_comp") ~ "Regression",
      Model %in% c("drift", "naive", "snaive") ~ "Baseline",
      TRUE ~ "Other"
    )
  ) |>
  mutate(Group = factor(Group, levels = c("Neural Networks", "ARIMA", "Regression", "Baseline")))|>
  mutate(Model = factor(Model, levels = c(
    "nn_base", "nn_vars",
    "arima_full", "arima_plain", "arima_sdif", "arima_no_comp",
    "tslm_base", "tslm_inter", "tslm_vars", "tslm_vars_no_comp",
    "drift", "naive", "snaive"
  )))

custom_colors <- c(
  "nn_base" = "#1f77b4", "nn_vars" = "#74a9cf",
  "arima_full" = "#31a354", "arima_plain" = "#74c476", "arima_no_comp" = "#a1d99b",
  "tslm_base" = "#ffcc00", "tslm_inter" = "#ffdd55", "tslm_vars" = "#ffee99", "tslm_vars_no_comp" = "#ffff00",
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
graph_save(p1,"RMSE_vs_proportion",paste0("./Results/Graphs/RMSE/Crash_Denoised_STL_hwy_",hwy))


# =================== Residual Analysis ===================

for (p in names(Mod_List_stl_dn)) {
  cat("Processing p=",p," models \n")
  for (mod in names(Mod_List_stl_dn[[p]])) {
    if(mod=='HighwayClass') next
    cat("  - Processing model:", mod, "\n")
    curr_model <- Mod_List_stl_dn[[p]] |> select(mod)
    curr_resid_graph <- curr_model |> gg_tsresiduals() + labs(title=paste(mod,'Residuals'), subtitle=paste0("Denoised STL ",'(p=',p,")"))
    graph_save(graph=curr_resid_graph, 
               graph_name=paste0("Residuals",mod,p), 
               graph_path=paste0("./Results/Graphs/Residuals/Crash_Denoised_STL_hwy_",hwy,"/"))
  }
}


for_full

