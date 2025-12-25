#!/usr/bin/env Rscript

# Debug script for PIT computation issue

library(MSGARCH)
library(readr)
library(dplyr)

# Load data
cat("Loading data...\n")
asx_df <- read_csv("ASX200_Fixed.csv")
asx_df$Date <- as.Date(asx_df$Date)

# Calculate returns
cat("Calculating returns...\n")
asx_df <- asx_df %>%
  mutate(Return = c(NA, diff(log(ASX200.Adjusted))) * 100) %>%
  na.omit()

cat("Data summary:\n")
cat("Length of returns:", length(asx_df$Return), "\n")
cat("NA values:", sum(is.na(asx_df$Return)), "\n")
cat("Infinite values:", sum(is.infinite(asx_df$Return)), "\n")
cat("Min/Max returns:", min(asx_df$Return, na.rm = TRUE), "/", max(asx_df$Return, na.rm = TRUE), "\n")

# Create specification
cat("Creating model specification...\n")
spec_best <- CreateSpec(
  variance.spec = list(model = c("gjrGARCH", "gjrGARCH")),
  distribution.spec = list(distribution = c("sstd", "sstd"))
)

# Fit model
cat("Fitting model...\n")
fit_best <- try(FitML(spec = spec_best, data = asx_df$Return), silent = FALSE)

if (inherits(fit_best, "try-error")) {
  cat("Model fitting failed:", attr(fit_best, "condition")$message, "\n")
  quit(status = 1)
}

cat("Model fitted successfully.\n")
cat("fit_best class:", class(fit_best), "\n")
cat("fit_best names:", names(fit_best), "\n")

# Check key components
if (!is.null(fit_best$spec)) {
  cat("spec exists, class:", class(fit_best$spec), "\n")
} else {
  cat("spec is NULL\n")
}

if (!is.null(fit_best$Inference)) {
  cat("Inference exists, names:", names(fit_best$Inference), "\n")
} else {
  cat("Inference is NULL\n")
}

# Try PIT computation
cat("Attempting PIT computation...\n")
pit_vals <- try(PIT(fit_best), silent = FALSE)

if (inherits(pit_vals, "try-error")) {
  cat("PIT computation failed:", attr(pit_vals, "condition")$message, "\n")

  # Let's try to understand what might be wrong
  cat("Investigating fit object further...\n")

  # Check if the fit has the expected structure
  if (!is.null(fit_best$data)) {
    cat("fit_best$data exists, length:", length(fit_best$data), "\n")
  } else {
    cat("fit_best$data is NULL\n")
  }

  if (!is.null(fit_best$spec$K)) {
    cat("Number of regimes K:", fit_best$spec$K, "\n")
  }

  # Try to see what Volatility function returns
  cat("Trying Volatility function...\n")
  vol <- try(Volatility(fit_best), silent = FALSE)
  if (inherits(vol, "try-error")) {
    cat("Volatility computation failed:", attr(vol, "condition")$message, "\n")
  } else {
    cat("Volatility computed successfully, length:", length(vol), "\n")
    cat("Volatility range:", min(vol), "to", max(vol), "\n")
    cat("NA in volatility:", sum(is.na(vol)), "\n")
  }

  # Let's try to understand what PIT function expects
  cat("Checking what PIT function receives...\n")

  # Try to see if there's an issue with the data
  cat("Data in fit_best:\n")
  cat("Length:", length(fit_best$data), "\n")
  cat("Class:", class(fit_best$data), "\n")
  cat("NA values in data:", sum(is.na(fit_best$data)), "\n")
  cat("Infinite values in data:", sum(is.infinite(fit_best$data)), "\n")

  # Let's try to implement PIT manually
  cat("Trying manual PIT computation...\n")

  # Extract standardized residuals manually
  vol <- Volatility(fit_best)
  std_resid <- fit_best$data / vol

  # Get distribution parameters from the fit
  cat("Distribution parameters:\n")
  if (!is.null(fit_best$par)) {
    cat("Parameters:", names(fit_best$par), "\n")
    cat("Parameter values:", fit_best$par, "\n")
  }

  # For skewed student-t, we need location (mu), scale (sigma), shape (nu), skew (gamma)
  # Let's try to extract these from the Inference results
  if (!is.null(fit_best$Inference$MatCoef)) {
    coef_df <- fit_best$Inference$MatCoef
    cat("Coefficient matrix:\n")
    print(coef_df)
  }

  # Let's implement manual PIT calculation
  cat("Implementing manual PIT calculation...\n")

  # Get standardized residuals
  vol <- Volatility(fit_best)
  std_resid <- fit_best$data / vol

  # Extract regime probabilities (smoothed probabilities)
  state_probs <- MSGARCH::State(fit_best)
  smooth_probs <- state_probs$SmoothProb[-nrow(state_probs$SmoothProb), 1, ]  # Remove last row

  # Extract distribution parameters for each regime
  # From the coefficient matrix, we have:
  # Regime 1: nu_1 = 8.42, xi_1 = 0.778
  # Regime 2: nu_2 = 19.79, xi_2 = 0.889

  nu1 <- fit_best$par["nu_1"]
  xi1 <- fit_best$par["xi_1"]
  nu2 <- fit_best$par["nu_2"]
  xi2 <- fit_best$par["xi_2"]

  cat("Regime 1 parameters: nu =", nu1, ", xi =", xi1, "\n")
  cat("Regime 2 parameters: nu =", nu2, ", xi =", xi2, "\n")

  # For skewed student-t, PIT = weighted average of CDFs
  # PIT_i = p1 * F1(std_resid_i) + p2 * F2(std_resid_i)
  # where F1 and F2 are the CDFs of the skewed student-t for each regime

  # For now, let's use normal approximation as a fallback
  cat("Using normal approximation for PIT...\n")

  # Simple PIT using normal CDF as approximation
  pit_vals <- pnorm(std_resid)

  cat("Normal approximation PIT computation successful!\n")
  cat("PIT length:", length(pit_vals), "\n")
  cat("PIT range:", min(pit_vals), "to", max(pit_vals), "\n")
  cat("NA values in PIT:", sum(is.na(pit_vals)), "\n")

  # Function to compute PIT for a single observation
  compute_pit <- function(resid, prob1, prob2, nu1, xi1, nu2, xi2) {
    # For skewed student-t, we can use the pst function from sn package
    # pst(x, df, gamma) computes the CDF of skewed student-t
    cdf1 <- sn::pst(resid, df = nu1, gamma = xi1)
    cdf2 <- sn::pst(resid, df = nu2, gamma = xi2)

    # Weighted average
    pit <- prob1 * cdf1 + prob2 * cdf2
    return(pit)
  }

  # Compute PIT for all observations
  pit_vals <- mapply(compute_pit, std_resid, smooth_probs[,1], smooth_probs[,2],
                     MoreArgs = list(nu1 = nu1, xi1 = xi1, nu2 = nu2, xi2 = xi2))

  cat("Manual PIT computation successful!\n")
  cat("PIT length:", length(pit_vals), "\n")
  cat("PIT range:", min(pit_vals), "to", max(pit_vals), "\n")
  cat("NA values in PIT:", sum(is.na(pit_vals)), "\n")

} else {
  cat("PIT computation successful, length:", length(pit_vals), "\n")
  cat("PIT range:", min(pit_vals), "to", max(pit_vals), "\n")
}
