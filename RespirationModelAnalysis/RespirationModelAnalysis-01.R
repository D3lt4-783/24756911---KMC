library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

data_path <- "IRSensorData-03.xlsx"

raw <- read_excel(data_path, col_names = FALSE)

raw_data <- raw %>%
  separate(1, into = c("timestamp_ms", "raw_adc", "voltage_v", "distance_mm"),
           sep = ",")

#Drop header row
raw_data <- raw_data[-1, ]

#Force everything to numeric
raw_data <- raw_data %>%
  mutate(across(everything(), as.numeric))

#Convert timestamp to seconds, starting from zero.
raw_data$t_s <- (raw_data$timestamp_ms - raw_data$timestamp_ms[1]) / 1000

# Remove measurements below 90 mm (treated as noise)
raw_data <- raw_data %>%
  filter(distance_mm >= 90)

# Reset time so it starts at zero again after filtering
raw_data$t_s <- raw_data$t_s - raw_data$t_s[1]

#Respiration model
# Model parameters - Same as MATLAB/Arduino 

t1     <- 2         #inhale duration (s)
t2     <- 3         #exhale duration (s)
t_cycle <- t1 + t2

a2     <- -5        # inhale shaping parameter
a1     <- 10        # pressure coefficient
a0     <- 0         # starting pressure
tau_rs <- 0.5       # Rrs * Crs
Rrs    <- 2         # respiratory resistance
tau    <- 7.5       #exhale shaping parameter
V0     <- 0         #initial volume/displacement

SCALE  <- 10        #same amplitude scaling factor used in the Arduino code

#Intermediate coefficients

C1 <- a2
C2 <- a1-2 *a2 * tau_rs
C3 <- a0 -a1 *tau_rs + 2 * a2 * tau_rs^2

P_t1 <- a2 *t1^2 + a1 * t1 + a0
exh_denom <-Rrs * (1 /tau_rs -1 / tau)

V_t1 <-(tau_rs/ Rrs) * (C1 *t1^2 + C2*t1 + C3 *(1 - exp(-t1 / tau_rs))) +
  V0 *exp(-t1/tau_rs)

# piece wise displacement function, single breathing cycle;
#t must already be wrapped into [0, t_cycle)
breath_displacement <- function(t) {
  inhale <- t <= t1
  
  v_inhale <- (tau_rs /Rrs) * (C1 *t^2+ C2 *t+C3*(1 -exp(-t / tau_rs))) +
    V0 * exp(-t/ tau_rs)
  
  te <- t -t1
  v_exhale<-(P_t1/exh_denom)*(exp(-te / tau)-exp(-te / tau_rs)) +
    V_t1*exp(-te /tau_rs)
  
  result <- ifelse(inhale,v_inhale, v_exhale) *SCALE
  return(result)
}

#Fit the model to the measured data
sign_flip <- 1

model_predict <- function(t_s, offset, amp_scale, phase_shift) {
  t_wrapped <- (t_s - phase_shift) %% t_cycle
  offset + sign_flip * abs(amp_scale) * breath_displacement(t_wrapped)
}

sse <- function(par) {
  offset <- par[1]
  amp_scale <- par[2]
  phase_shift <- par[3]
  predicted <- model_predict(raw_data$t_s, offset, amp_scale, phase_shift)
  sum((raw_data$distance_mm - predicted)^2)
}

# Reasonable starting guesses: offset = mean distance, amp_scale = 1,
# phase_shift = 0. Nelder-Mead is derivative-free, so it copes fine with
# the function's kink at t = t1 and doesn't need parameter bounds.
start_par <- c(
  offset = mean(raw_data$distance_mm),
  amp_scale = 1,
  phase_shift = 0
)

fit <- optim(
  par = start_par,
  fn = sse,
  method = "Nelder-Mead",
  control = list(maxit = 5000, reltol = 1e-10)
)

fit_offset      <- fit$par[1]
fit_amp_scale   <- abs(fit$par[2])
fit_phase_shift <- fit$par[3] %% t_cycle

cat("Fitted offset (mm):     ", round(fit_offset, 3), "\n")
cat("Fitted amplitude scale: ", round(fit_amp_scale, 3), "\n")
cat("Fitted phase shift (s): ", round(fit_phase_shift, 3), "\n")

#Goodness of fit

raw_data$predicted_mm <- model_predict(raw_data$t_s, fit_offset, 1, fit_phase_shift)
residuals <- raw_data$distance_mm -raw_data$predicted_mm

rmse <- sqrt(mean(residuals^2))
r_squared <- 1 -sum(residuals^2)/sum((raw_data$distance_mm-mean(raw_data$distance_mm))^2)

cat("RMSE (mm): ",round(rmse, 3),"\n")
cat("R-squared: ", round(r_squared, 3), "\n")

# Plot measured vs. fitted model

# Dense model curve for a smooth overlay line
model_curve <- data.frame(t_s = seq(min(raw_data$t_s), max(raw_data$t_s), length.out = 1000))
model_curve$predicted_mm <- model_predict(model_curve$t_s, fit_offset, 1, fit_phase_shift)

#All Data and fitted plot over 100 resp cycles. 
# Shows that we must break the data up into individual cycles. 
ggplot() +
  geom_line(data = raw_data,
            aes(x = t_s, y = distance_mm),
            color = "blue",
            linewidth = 0.7,
            linetype = "dashed") +
  geom_point(data = raw_data, 
              aes(x = t_s, y = distance_mm), 
              color = "black",
              size = 1.5, 
              alpha = 0.7) +
  geom_line(data = model_curve,
            aes(x = t_s, y = predicted_mm), 
            color = "red", 
            linewidth = 1) +
  labs(
    title = "Measured Mattress Displacement vs. Fitted Respiration Model",
    subtitle = paste0("RMSE = ", round(rmse, 2), " mm, R\u00b2 = ", round(r_squared, 3)),
    x = "Time (s)",
    y = "Distance (mm)"
  ) +
  theme_minimal(base_size = 13)

# First 20 seconds only
#Purely to understand the data a bit better 
raw_20 <- raw_data %>%
  filter(t_s <= 20)

model_curve_20 <- model_curve %>%
  filter(t_s <= 20)

ggplot() +
  geom_line(data = raw_20,
            aes(x = t_s, y = distance_mm),
            color = "blue",
            linewidth = 0.7,
            linetype = "dashed") +
  geom_point(data = raw_20,
             aes(x = t_s, y = distance_mm),
             color = "black",
             size = 1.5,
             alpha = 0.7) +
  geom_line(data = model_curve_20,
            aes(x = t_s, y = predicted_mm),
            color = "red",
            linewidth = 1) +
  labs(
    title = "First 20 s of Mattress Displacement vs. Fitted Respiration Model",
    subtitle = paste0("RMSE = ", round(rmse, 2), " mm, R² = ", round(r_squared, 3)),
    x = "Time (s)",
    y = "Distance (mm)"
  ) +
  theme_minimal(base_size = 13)

####################
raw_data_5s <- raw_data %>%
  filter(t_s >= 2)

# Reset time again
raw_data_5s$t_s <- raw_data_5s$t_s - raw_data_5s$t_s[1]

raw_data_5s <- raw_data_5s %>%
  mutate(
    cycle = floor(t_s / t_cycle) + 1,
    t_cycle_local = t_s %% t_cycle
  )

cycle_fits <- raw_data_5s %>%
  group_split(cycle)

results <- lapply(cycle_fits, function(df) {
  
  sse <- function(par) {
    pred <- model_predict(df$t_cycle_local,
                          offset = par[1],
                          amp_scale = par[2],
                          phase_shift = par[3])
    sum((df$distance_mm - pred)^2)
  }
  
  fit <- optim(
    par = c(mean(df$distance_mm), 1, 0),
    fn = sse,
    method = "Nelder-Mead"
  )
  
  df$predicted_mm <- model_predict(
    df$t_cycle_local,
    fit$par[1],
    abs(fit$par[2]),
    fit$par[3] %% t_cycle
  )
  
  df
})

raw_data_5s <- bind_rows(results)

#basic Stats of the cycles 
cycle_stats <- raw_data_5s %>%
  group_by(cycle) %>%
  summarise(
    RMSE = sqrt(mean((distance_mm - predicted_mm)^2)),
    MAE  = mean(abs(distance_mm - predicted_mm)),
    Bias = mean(distance_mm - predicted_mm),
    SD_Error = sd(distance_mm - predicted_mm),
    R2 = 1 - sum((distance_mm - predicted_mm)^2) /
      sum((distance_mm - mean(distance_mm))^2),
    Peak_Error = max(abs(distance_mm - predicted_mm)),
    n = n()
  )

cycle_stats

ggplot(cycle_stats, aes(x = "",y = RMSE)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "RMSE Across Breathing Cycles",
    y = "RMSE (mm)"
  ) +
  theme_minimal()

#All cycles plotted on the same axis to understand the data better. 
ggplot(raw_data_5s, aes(x = t_cycle_local)) +
  geom_line(aes(y = distance_mm, group = cycle),
            colour = "black",
            alpha = 0.35) +
  geom_line(aes(y = predicted_mm, group = cycle),
            colour = "red",
            linewidth = 1) +
  labs(
    title = "Measured and Predicted Mattress Displacement",
    x = "Time within breathing cycle (s)",
    y = "Distance (mm)"
  ) +
  theme_minimal()

first_last <- raw_data_5s %>%
  filter(cycle %in% c(min(cycle)+1, max(cycle)-1)) %>%
  mutate(
    Cycle = ifelse(cycle == min(cycle),
                   "First Cycle",
                   "Last Cycle")
  )

#Figure used in Project report. 
ggplot(first_last, aes(x = t_cycle_local)) +
  #Measured data
  geom_point(aes(y = distance_mm,
                 colour = Cycle),
             size = 2,
             alpha = 0.8) +
  
  #Fitted model
  geom_line(aes(y = predicted_mm,
                colour = Cycle),
            linewidth = 1.2) +
  
  scale_colour_manual(values = c(
    "First Cycle" = "blue",
    "Last Cycle" = "red"
  )) +
  
  labs(
    title = "",
    x = "Time within cycle (s)",
    y = "Distance (mm)",
    colour = ""
  ) +
  
  theme_minimal(base_size = 13)