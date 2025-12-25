# Script to generate heavy computation results for Project 2
# Run this once to create CSV and RDS files

library(quantmod)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readr)
library(tseries)
library(MSGARCH)
library(gridExtra)
library(zoo)
library(tidyverse)
library(doParallel)
library(foreach)
library(kableExtra)

# Load data
asx_df <- read_csv("ASX200_Fixed.csv")
asx_df$Date <- as.Date(asx_df$Date)
asx_df <- asx_df %>%
  mutate(Return = c(NA, diff(log(ASX200.Adjusted))) * 100) %>%
  na.omit()

# 1. Model Bake-off
cat("Running model bake-off...\n")
candidates <- expand.grid(
  Model = c("sGARCH", "gjrGARCH"),
  Dist  = c("norm", "sstd"),
  K     = c(1, 2),
  stringsAsFactors = FALSE
)

run_bakeoff <- function(data_vector, candidates) {
  results <- list()

  for(i in 1:nrow(candidates)) {
    mod_type <- candidates$Model[i]
    dist_type <- candidates$Dist[i]
    k_regimes <- candidates$K[i]

    spec <- CreateSpec(
      variance.spec = list(model = rep(mod_type, k_regimes)),
      distribution.spec = list(distribution = rep(dist_type, k_regimes))
    )

    tryCatch({
      fit <- FitML(spec = spec, data = data_vector)

      results[[i]] <- data.frame(
        Regimes = k_regimes,
        Vol_Model = mod_type,
        Distribution = dist_type,
        AIC = AIC(fit),
        BIC = BIC(fit)
      )
    }, error = function(e) {
      results[[i]] <- data.frame(
        Regimes = k_regimes,
        Vol_Model = mod_type,
        Distribution = dist_type,
        AIC = NA,
        BIC = NA
      )
    })
  }

  final_table <- do.call(rbind, results)
  return(final_table)
}

bakeoff_results <- run_bakeoff(asx_df$Return, candidates)
write.csv(bakeoff_results, "bakeoff_results.csv", row.names = FALSE)

# 2. Main Model Fitting
cat("Fitting main model...\n")
spec_best <- CreateSpec(
  variance.spec = list(model = c("gjrGARCH", "gjrGARCH")),
  distribution.spec = list(distribution = c("sstd", "sstd"))
)

fit_best <- FitML(spec = spec_best, data = asx_df$Return)

# Extract key results for visualization
state_probs <- MSGARCH::State(fit_best)
smooth_probs <- state_probs$SmoothProb[-nrow(state_probs$SmoothProb), 1, ]

# Save key dataframes instead of the full model
model_results <- data.frame(
  Date = asx_df$Date,
  Return = asx_df$Return,
  Prob_HighVol = smooth_probs[, 2]  # Probability of Regime 2
)

coef_df <- fit_best$Inference$MatCoef %>%
  as.data.frame() %>%
  rownames_to_column(var = "ParamLabel")

trans_df <- coef_df %>%
  filter(str_detect(ParamLabel, "^P_"))

p11 <- trans_df$Estimate[trans_df$ParamLabel == "P_1_1"]
p21 <- trans_df$Estimate[trans_df$ParamLabel == "P_2_1"]
p22 <- 1 - p21

P_mat <- matrix(c(p11, 1-p11, p21, p22), 2, 2, byrow = TRUE)
stable_dist <- eigen(t(P_mat))$vectors[,1]
stable_dist <- stable_dist / sum(stable_dist)

dur1 <- 1 / (1 - p11)
dur2 <- 1 / (1 - p22)

transition_data <- data.frame(
  Metric = c("Transition Probability (P_ii)", "Expected Duration (Days)", "Unconditional Probability"),
  Regime_1 = c(p11, dur1, stable_dist[1]),
  Regime_2 = c(p22, dur2, stable_dist[2])
)

# Save dataframes
write.csv(model_results, "model_results.csv", row.names = FALSE)
write.csv(coef_df, "coef_df.csv", row.names = FALSE)
write.csv(transition_data, "transition_data.csv", row.names = FALSE)

# Save the fitted model for diagnostics
saveRDS(fit_best, "fit_best_model.rds")

# Note: Diagnostics are now computed in the Quarto document from model_results.csv
# This avoids MSGARCH compatibility issues with saved/loaded models

# Also save basic model info
model_info <- list(
  AIC = AIC(fit_best),
  BIC = BIC(fit_best),
  convergence = fit_best$convergence
)
saveRDS(model_info, "model_info.rds")

# 3. 2-Regime Cross-Validation
cat("Running 2-regime cross-validation...\n")
n_total <- length(asx_df$Return)
min_train_size <- 500
refit_every <- 5
n_test <- n_total - min_train_size

n_cores <- parallel::detectCores() - 1
registerDoParallel(cores = n_cores)

refit_indices <- seq(from = 1, to = n_test, by = refit_every)

results_list <- foreach(i = refit_indices, .packages = c('MSGARCH')) %dopar% {
  current_pos <- min_train_size + i - 1
  train_data <- asx_df$Return[1:current_pos]

  spec_local <- CreateSpec(
    variance.spec = list(model = c("gjrGARCH", "gjrGARCH")),
    distribution.spec = list(distribution = c("sstd", "sstd"))
  )

  fit <- FitML(spec = spec_local, data = train_data)
  pred <- predict(fit, nahead = refit_every, do.return.draw = FALSE)

  data.frame(
    Step_Index = i:(min(i + refit_every - 1, n_test)),
    Predicted_Vol = pred$vol[1:min(refit_every, n_test - i + 1)]
  )
}

final_results <- do.call(rbind, results_list)
stopImplicitCluster()

final_results <- final_results[order(final_results$Step_Index), ]
final_results <- final_results[!duplicated(final_results$Step_Index), ]

test_indices <- (min_train_size + 1):(min_train_size + nrow(final_results))
final_results$Actual_Return <- asx_df$Return[test_indices]
final_results$Date <- asx_df$Date[test_indices]

cv_metrics_2regime <- data.frame(
  MSE = mean((final_results$Predicted_Vol - abs(final_results$Actual_Return))^2),
  RMSE = sqrt(mean((final_results$Predicted_Vol - abs(final_results$Actual_Return))^2)),
  MAE = mean(abs(final_results$Predicted_Vol - abs(final_results$Actual_Return))),
  Model = "2-Regime MS-GJR-GARCH"
)

write.csv(final_results, "cv_results_2regime.csv", row.names = FALSE)
write.csv(cv_metrics_2regime, "cv_metrics_2regime.csv", row.names = FALSE)

# 4. 1-Regime Cross-Validation
cat("Running 1-regime cross-validation...\n")
n_cores <- parallel::detectCores() - 1
registerDoParallel(cores = n_cores)

refit_indices <- seq(from = 1, to = n_test, by = 5)

results_1reg_list <- foreach(i = refit_indices, .packages = c('MSGARCH')) %dopar% {
  current_pos <- min_train_size + i - 1
  train_data <- asx_df$Return[1:current_pos]

  spec_1reg <- CreateSpec(
    variance.spec = list(model = "gjrGARCH"),
    distribution.spec = list(distribution = "sstd")
  )

  fit <- FitML(spec = spec_1reg, data = train_data)
  pred <- predict(fit, nahead = refit_every, do.return.draw = FALSE)

  data.frame(
    Step_Index = i:(min(i + refit_every - 1, n_test)),
    Predicted_Vol = pred$vol[1:min(refit_every, n_test - i + 1)]
  )
}

results_1reg <- do.call(rbind, results_1reg_list)
stopImplicitCluster()

results_1reg <- results_1reg[order(results_1reg$Step_Index), ]
results_1reg <- results_1reg[!duplicated(results_1reg$Step_Index), ]

test_indices_1reg <- (min_train_size + 1):(min_train_size + nrow(results_1reg))
results_1reg$Actual_Return <- asx_df$Return[test_indices_1reg]
results_1reg$Date <- asx_df$Date[test_indices_1reg]

cv_metrics_1regime <- data.frame(
  MSE = mean((results_1reg$Predicted_Vol - abs(results_1reg$Actual_Return))^2),
  RMSE = sqrt(mean((results_1reg$Predicted_Vol - abs(results_1reg$Actual_Return))^2)),
  MAE = mean(abs(results_1reg$Predicted_Vol - abs(results_1reg$Actual_Return))),
  Model = "1-Regime GJR-GARCH"
)

write.csv(results_1reg, "cv_results_1regime.csv", row.names = FALSE)
write.csv(cv_metrics_1regime, "cv_metrics_1regime.csv", row.names = FALSE)

cat("All computations completed! Files saved.\n")