library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)

#peak detection algorithm 
find_sensor_peaks <- function(df, window_ms, threshold) {
  df <- df %>% arrange(time_ms)
  onset_times <- df$time_ms[df$beat == 1 & lag(df$beat, default = 0) == 0]
  onset_times <- onset_times[!is.na(onset_times)]
  
  if (length(onset_times) == 0) return(NULL)
  
  sensors <- c("piezo1", "piezo2", "piezo3", "piezo4")
  
  peak_list <- lapply(seq_along(onset_times), function(i) {
    start_t <- onset_times[i]
    end_t   <- if (i < length(onset_times)) {
      min(start_t + window_ms, onset_times[i + 1])
    } else {
      start_t + window_ms
    }
    
    window <- df %>% filter(time_ms >= start_t, time_ms <= end_t)
    if (nrow(window) == 0) return(NULL)
    
    # For each sensor, find its own peak time/value within this window, but only 
#count it as a detected pulse if it clears the threshold
    sensor_rows <- lapply(sensors, function(s) {
      vals <- window[[s]]
      peak_val <- suppressWarnings(max(vals, na.rm = TRUE))
      
      if (!is.finite(peak_val) || peak_val <= threshold) {
        data.frame(pulse_id = i, sensor = s, peak_time = NA_real_, peak_value = NA_real_)
      } else {
        peak_time <- window$time_ms[which.max(vals)]
        data.frame(pulse_id = i, sensor = s, peak_time = peak_time, peak_value = peak_val)
      }
    })
    
    bind_rows(sensor_rows)
  })
  
  bind_rows(peak_list)
}

raw <- read_excel("PulseData(1234)-01.xlsx", col_names = FALSE)

data <- raw %>%
  separate(1, into = c("time_ms", "beat", "piezo1", "piezo2", "piezo3", "piezo4"),
           sep = ",")

#Drop header row
data <- data[-1, ]

#Force everything to numeric
data <- data %>%
  mutate(across(everything(), as.numeric))

threshold  <- 3     #per-sensor detection threshold
window_ms  <- 400   

sensor_peaks <- find_sensor_peaks(data, window_ms, threshold)
print(sensor_peaks)

sync_check <- sensor_peaks %>%
  filter(!is.na(peak_time)) %>%
  group_by(pulse_id) %>%
  summarise(
    n_sensors_detected = n(),
    earliest = min(peak_time),
    latest   = max(peak_time),
    spread_ms = latest - earliest,
    mean_time = mean(peak_time),
    sd_time   = sd(peak_time)
  )

print(sync_check)

#plot all data over time
ggplot(sensor_peaks %>% filter(!is.na(peak_time)),
       aes(x = factor(pulse_id), y = peak_time, colour = sensor)) +
  geom_point(size = 3) +
  labs(
    title = "Peak Timing Across Sensors Per Pulse",
    x = "Pulse Number",
    y = "Time of Peak (ms)",
    colour = "Sensor"
  ) +
  theme_minimal(base_size = 14)


#Exploratory data Analysis
#Prep Data
rm_data <- sensor_peaks %>%
  group_by(pulse_id) %>%
  filter(all(!is.na(peak_time)), n_distinct(sensor) == 4) %>%
  ungroup()


rm_data$pulse_id <- factor(rm_data$pulse_id)
rm_data$sensor   <- factor(rm_data$sensor)

rm_anova <- aov(peak_time ~ sensor + Error(pulse_id/sensor), data = rm_data)
summary(rm_anova)

#Friedman test
wide_check <- rm_data %>%
  select(pulse_id, sensor, peak_time) %>%
  pivot_wider(names_from = sensor, values_from = peak_time)

friedman_result <- friedman.test(as.matrix(wide_check[,-1]))
print(friedman_result)

#basic stats of all data
spread_summary <- sync_check %>%
  summarise(
    mean_spread_ms = mean(spread_ms),
    sd_spread_ms   = sd(spread_ms),
    max_spread_ms  = max(spread_ms),
    median_spread_ms = median(spread_ms)
  )
print(spread_summary)

#pairwise timing diffs. 

pair_diffs <- wide_check %>%
  mutate(
    d12 = abs(piezo1 - piezo2),
    d13 = abs(piezo1 - piezo3),
    d14 = abs(piezo1 - piezo4),
    d23 = abs(piezo2 - piezo3),
    d24 = abs(piezo2 - piezo4),
    d34 = abs(piezo3 - piezo4)
  ) %>%
  select(pulse_id, starts_with("d")) %>%
  pivot_longer(-pulse_id, names_to = "pair", values_to = "diff_ms")

#basic stats of pair data
pair_diffs %>%
  group_by(pair) %>%
  summarise(
    n = n(),
    min = min(diff_ms),
    Q1 = quantile(diff_ms, 0.25),
    median = median(diff_ms),
    Q3 = quantile(diff_ms, 0.75),
    max = max(diff_ms),
    mean = mean(diff_ms),
    sd = sd(diff_ms)
  )

ggplot(pair_diffs, aes(x = pair, y = diff_ms)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(
    x = "Sensor Pair",
    y = "Time Difference (ms)"
  ) +
  theme_minimal(base_size = 14)


#Assign a pulse number based on cumulative beat starts, then keep first 5 pulses
#Purely for figure used in project
data_5 <- data %>%
  mutate(pulse_num = cumsum(beat == 1)) %>%
  filter(pulse_num <= 6)

ma_n <- 1  # smoothing window

data_5$piezo1_s <- as.numeric(stats::filter(data_5$piezo1, rep(1/ma_n, ma_n), sides = 2))
data_5$piezo2_s <- as.numeric(stats::filter(data_5$piezo2, rep(1/ma_n, ma_n), sides = 2))
data_5$piezo3_s <- as.numeric(stats::filter(data_5$piezo3, rep(1/ma_n, ma_n), sides = 2))
data_5$piezo4_s <- as.numeric(stats::filter(data_5$piezo4, rep(1/ma_n, ma_n), sides = 2))

data_long_5 <- data_5 %>%
  select(time_ms, piezo1_s, piezo2_s, piezo3_s, piezo4_s) %>%
  pivot_longer(
    cols = starts_with("piezo"),
    names_to = "sensor",
    values_to = "value"
  )

ggplot(data_long_5, aes(x = time_ms, y = value)) +
  geom_line(linewidth = 1, color = "steelblue") +
  facet_wrap(~ sensor, ncol = 1, scales = "free_y") +
  labs(x = "Time (ms)", y = "Amplitude") +
  theme_minimal(base_size = 14)

ggplot(data_long_5, aes(x = time_ms, y = value, color = sensor)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  labs(x = "Time (ms)", y = "Amplitude", color = "Sensor") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

pulse_bounds <- data_5 %>%
  filter(beat == 1) %>%
  group_by(pulse_num) %>%
  summarise(
    start_time = min(time_ms),
    end_time   = max(time_ms)
  ) %>%
  ungroup()

print(pulse_bounds)

ggplot(data_long_5, aes(x = time_ms, y = value)) +
  geom_rect(
    data = pulse_bounds,
    aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "red", alpha = 4
  ) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ sensor, ncol = 1, scales = "free_y") +
  labs(
    title = "Piezo Sensor Signals with Beat Duration Shaded (First Pulses)",
    x = "Time (ms)",
    y = "Signal"
  ) +
  theme_minimal(base_size = 14)

############################################

peak_data <- data_5 %>%
  transmute(
    time_ms,
    beat,
    piezo1 = piezo1_s,
    piezo2 = piezo2_s,
    piezo3 = piezo3_s,
    piezo4 = piezo4_s
  )

sensor_peaks <- find_sensor_peaks(
  peak_data,
  window_ms = 300,    # chosen search window
  threshold = 2.5     # detection threshold
)

peak_points <- sensor_peaks %>%
  filter(!is.na(peak_time)) %>%
  rename(time_ms = peak_time) %>%
  mutate(sensor = paste0(sensor, "_s")) %>%
  left_join(
    data_long_5,
    by = c("time_ms", "sensor")
  )

ggplot(data_long_5, aes(x = time_ms, y = value)) +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_point(
    data = peak_points,
    aes(x = time_ms, y = value),
    colour = "red",
    size = 3
  ) +
  facet_wrap(~sensor, ncol = 1, scales = "free_y") +
  labs(x = "Time (ms)", y = "Amplitude") +
  theme_minimal(base_size = 14)

