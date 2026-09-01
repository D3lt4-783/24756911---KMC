
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Load and clean the sensor data
# ---------------------------------------------------------
# Update this path to point at your actual xlsx file.
data_path <- "IRStartUp(01).xlsx"

raw <- read_excel(data_path, col_names = FALSE)

raw_data <- raw %>%
  separate(1, into = c("timestamp_ms", "raw_adc", "voltage_v", "distance_mm"),
           sep = ",")

# Drop header row if it got read in as data
raw_data <- raw_data[-1, ]

# Force everything to numeric now that the header text is gone
raw_data <- raw_data %>%
  mutate(across(everything(), as.numeric))

# Convert timestamp to seconds, starting from zero
raw_data$t_s <- (raw_data$timestamp_ms - raw_data$timestamp_ms[1]) / 1000

# Remove measurements below 90 mm (treated as noise)
raw_data <- raw_data %>%
  filter(distance_mm >= 90)

# Reset time so it starts at zero again after filtering
raw_data$t_s <- raw_data$t_s - raw_data$t_s[1]

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data
data_path <- "IRStartUp(01).xlsx"

raw <- read_excel(data_path, col_names = FALSE)

raw_data <- raw %>%
  separate(
    1,
    into = c("timestamp_ms", "raw_adc", "voltage_v", "distance_mm"),
    sep = ","
  ) %>%
  slice(-1) %>%
  mutate(
    timestamp_ms = as.numeric(timestamp_ms),
    raw_adc = as.numeric(raw_adc),
    voltage_v = as.numeric(voltage_v),
    distance_mm = as.numeric(distance_mm)
  )

# Time in seconds
raw_data$t_s <- (raw_data$timestamp_ms -
                   raw_data$timestamp_ms[1]) / 1000

# Fit smoothing spline to the measured data
spline_fit <- smooth.spline(
  x = raw_data$t_s,
  y = raw_data$distance_mm,
  spar = 0.01
)

# Generate estimated values from the spline
raw_data$estimated_path <- predict(
  spline_fit,
  x = raw_data$t_s
)$y
ggplot() +
  
  geom_point(
    data = raw_data,
    aes(
      x = t_s,
      y = distance_mm,
      colour = "Data points"
    ),
    size = 1.5,
    alpha = 0.5
  ) +
  
  geom_line(
    data = raw_data,
    aes(
      x = t_s,
      y = estimated_path,
      colour = "Smoothed path"
    ),
    linewidth = 1
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Data points" = "black",
      "Smoothed path" = "blue"
    )
  ) +
  
  labs(
    x = "Time (s)",
    y = "Distance (mm)"
  ) +
  
  theme_minimal()
