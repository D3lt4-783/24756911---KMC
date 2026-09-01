library(dplyr)
library(ggplot2)

load_pulse_file <- function(filename) {
  raw <- readLines(filename)
  raw <- gsub('"', '', raw)
  raw <- raw[-1]
  split_data <- strsplit(raw, ",")
  
  df <- data.frame(
    time_ms = as.numeric(sapply(split_data, `[`, 1)),
    beat    = as.numeric(sapply(split_data, `[`, 2)),
    piezo1  = as.numeric(sapply(split_data, `[`, 3))
  )
  
  #Drop rows where parsing failed
  df <- df[complete.cases(df), ]
  df
}

find_peaks_for_location <- function(df, window_ms) {
  df <- df %>% arrange(time)
  raw_beat_times <- df$time[df$beat == 1]
  
  if (length(raw_beat_times) == 0) return(NULL)
  
#Beat time tracking
#Keep a beat time only if it's further than window_ms after the last kept onset.
  onset_times <- raw_beat_times[1]
  for (t in raw_beat_times[-1]) {
    if (t - onset_times[length(onset_times)] > window_ms) {
      onset_times <- c(onset_times, t)
    }
  }
  
  peak_list <- lapply(seq_along(onset_times), function(i) {
    start_t <- onset_times[i]
    end_t   <- if (i < length(onset_times)) {
      min(start_t + window_ms, onset_times[i + 1])
    } else {
      start_t + window_ms
    }
    
    window <- df %>% filter(time >= start_t, time <= end_t)
    if (nrow(window) == 0) return(NULL)
    
    window %>%
      slice_max(piezo1, n = 1, with_ties = FALSE) %>%
      mutate(pulse_id = i)
  })
  
  bind_rows(peak_list)
}

loc1 <- load_pulse_file("PulseData-Pos1-02.csv")
loc2 <- load_pulse_file("PulseData-Pos2-02.csv")
loc3 <- load_pulse_file("PulseData-Pos3-01.csv")
loc4 <- load_pulse_file("PulseData-Pos4-01.csv")

loc1$Location <- "Position 1"
loc2$Location <- "Position 2"
loc3$Location <- "Position 3"
loc4$Location <- "Position 4"


all_data <- bind_rows(loc1, loc2, loc3, loc4)

all_data <- all_data %>%
  group_by(Location) %>%
  mutate(time = time_ms - first(time_ms))

#Smooth data#
all_data_s <- all_data %>%
  group_by(Location) %>%
  mutate(
    piezo_smooth = stats::filter(
      piezo1,
      rep(1/5, 5),
      sides = 2
    )
  )

#PLOT SMOOTHED DATA

ggplot(all_data_s,
       aes(x = time,
           y = piezo1,
           colour = Location)) +
  geom_line(alpha = 5) +
  labs(
    title = "Piezo Response at Four Mattress Locations",
    x = "Time (ms)",
    y = "Piezo Signal"
  ) +
  theme_minimal()

#Detect Peaks

# threshold <- 4
# no_noise <- all_data_s %>%
#   filter(piezo1 > threshold)
# 
# pulse_peaks <- no_noise %>%
#   group_by(Location) %>%
#   mutate(
#     pulse_id = cumsum(c(1, diff(time) > 400))
#   ) %>%
#   group_by(Location, pulse_id) %>%
#   slice_max(piezo1, n = 1, with_ties = FALSE) %>%
#   ungroup()

lag_window_ms <- 1000

pulse_peaks <- all_data_s %>%
  group_by(Location) %>%
  group_modify(~ find_peaks_for_location(.x, lag_window_ms)) %>%
  ungroup()

print(pulse_peaks)

###################

#Ensure pulses are detected properly. 

ggplot(all_data_s,
       aes(x = time,
           y = piezo1,
           colour = Location)) +
  geom_line(alpha = 5) +
  geom_point(data = pulse_peaks,
             size = 2,
             colour = "Red") +
  theme_minimal() +
  labs(
    title = "Detected Pulse Peaks",
    x = "Time (ms)",
    y = "Piezo Signal"
  )  

#Exploratory data analysis
#Basic stats#

peak_stats <- pulse_peaks %>%
  group_by(Location) %>%
  summarise(
    n = n(),
    Mean = mean(piezo1),
    SD = sd(piezo1),
    Min = min(piezo1),
    Max = max(piezo1),
    CV_percent = 100 * SD/Mean,
    SE = SD/sqrt(n),
    Q1 = quantile(piezo1,0.25),
    Q3 = quantile(piezo1,0.75),
    CI95_lower = Mean-1.96*SE,
    CI95_upper = Mean+1.96*SE
  )
print(peak_stats)
  
#Peak distribution plot#
ggplot(pulse_peaks, aes(Location, piezo1)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1) +
  theme_minimal() +
  labs(
    #title = "Distribution of Pulse Amplitudes",
    y = "Peak Amplitude"
  )
  
#Does mattress position affect pulse?
anova_result <- aov(piezo1 ~ Location, data = pulse_peaks)

summary(anova_result)

#Check if there a significant pulse difference between locations?
TukeyHSD(anova_result)