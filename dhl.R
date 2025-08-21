# import libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# My DHL times
dhl <- data.frame(
  Year = c(2017,2018,2019,2021,2022,2023,2024,2025),
  Time = c("00:25:30","00:24:56","00:26:58",
           "00:27:45","00:26:12","00:28:17",
           "00:27:13","00:28:03")
)

# Convert to seconds
dhl <- dhl %>%
  mutate(Time_sec = period_to_seconds(hms(Time)))

# Scale positions: fastest = 100m, others relatively behind to convert
# into 100m sprint race
max_dist <- 100
min_time <- min(dhl$Time_sec)
dhl <- dhl %>%
  mutate(dist = max_dist * (min_time / Time_sec),
         lane = row_number())

# Number of lanes
nlanes <- nrow(dhl)

# Background rectangle (track)
track_bg <- data.frame(xmin=-1, xmax=100, ymin=0, ymax=nlanes)

# Plot
gg <- ggplot() +
  # Track background
  geom_rect(data=track_bg, 
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="#FFCC00") +   # DHL yellow
  # Lane lines
  geom_hline(yintercept=0:nlanes, color="white") +
  # Start line
  geom_vline(xintercept=0, color="white", linewidth=1.5) +
  # Finish line
  geom_vline(xintercept=100, color="white", linewidth=1.5) +
  # Runners (years)
  geom_point(data=dhl, aes(x=dist, y=lane - 0.5), 
             size=5, color="black", fill="white", shape=21) +
  # Text inside the lane: Year - Time
  geom_text(data=dhl, 
            aes(x=50, y=lane, 
            label=paste0(Year, " - ", sub("^00:", "", Time))),
            color="red", size=4, fontface="bold", # DHL Red
            nudge_y = 0.5) +
  scale_y_reverse() +   # First year at top
  scale_x_continuous(limits=c(-5,105), expand=c(0,0)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill="#FFCC00", color=NA),
    plot.title = element_text(hjust=0.5, size=12, face="bold", color="red")
  ) +
  ggtitle("Tracking My 5K Times in DHL Stafetten Over the Years")

ggsave("./images/DHL_staffetten.png", plot = gg, width = 6, height = 4, dpi = 300, bg="#FFCC00")
