##05-01-pithouse-count-plots

#These plots provide several different ways to visualize how the number of pithouses changed over time

samplecleanround <- readRDS(here::here("data/derived-data/samplecleanround.rds"))
samplecleanroundfilter <- samplecleanround %>%
  filter(PillarPosition != "uk")
sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))
sampleclean_trimfilter <- sampleclean_trim %>%
  filter(PillarPosition != "uk")

#Pithouse Counts - 2 parts
PithouseCounts1000 <- samplecleanround %>%
  group_by(sampled, RunID) %>%
  tally()

PithouseCounts1000Plot <- ggplot(data = PithouseCounts1000) +
  aes(x = -sampled, y = n, group = RunID) +
  stat_smooth(geom = "line", color = "blue", alpha = 0.1, se = FALSE) +
  coord_cartesian(c(5400,4400)) +
  ggtitle("Pithouse Counts") + xlab("Years calBP") + ylab("Pithouses") +
  scale_x_reverse() +
  theme_minimal()

#Pithouse Count Histogram - Barplot 
PithouseCounts1000Barplot <- ggplot(data = samplecleanround) +
  aes(x = -sampled) +
  geom_bar() +
  ggtitle("Pithouse Counts") + xlab("Years calBP") + ylab("pithouses") +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

#Stacked Barplot
PithouseCounts1000BarplotStack <- ggplot(data = samplecleanround) +
  aes(x = -sampled, fill = PillarPosition) +
  geom_bar(position = "stack") +
  ggtitle("Pithouse Counts") + xlab("Years calBP") + ylab("pithouses") +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

PithouseCounts1000BarplotStackAvg <- ggplot(data = samplecleanround) +
  aes(x = -sampled, fill = PillarPosition) +
  scale_y_continuous(labels = function(x) x/1000) +
  geom_bar(position = "stack") +
  ggtitle("Pithouse Counts - Scaled") + xlab("Years calBP") + ylab("pithouses") +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

PithouseCounts1000BarplotStackAvgNoUK <- ggplot(data = samplecleanroundfilter) +
  aes(x = -sampled, fill = PillarPosition) +
  scale_y_continuous(labels = function(x) x/1000) +
  geom_bar(position = "stack") +
  ggtitle("Pithouse Counts - Scaled") + xlab("Years calBP") + ylab("pithouses") +
  scale_x_reverse(breaks = seq(min(-samplecleanroundfilter$sampled), 
                               max(-samplecleanroundfilter$sampled), 100), 
                  labels = seq(min(-samplecleanroundfilter$sampled), 
                               max(-samplecleanroundfilter$sampled), 100) ) +
  theme_minimal()

#Barplot Series
PithouseCounts1000BarplotSeries <- ggplot(data = samplecleanround) +
  aes(x = -sampled, fill = PillarPosition) +
  geom_bar(position = "dodge") +
  ggtitle("Pithouse Counts") + xlab("Years calBP") + ylab("pithouses") +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

PithouseCounts1000BarplotSeriesAvg <- ggplot(data = samplecleanround) +
  aes(x = -sampled, fill = samplecleanround$PillarPosition) +
  scale_y_continuous(labels = function(x) x/1000) +
  geom_bar(position = "dodge") +
  ggtitle("Pithouse Counts - Scaled") + xlab("Years calBP") + ylab("pithouses") + labs(fill = "Pithouse Types") +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

PithouseCounts1000BarplotSeriesAvgNoUK <- ggplot(data = samplecleanroundfilter) +
  aes(x = -sampled, fill = samplecleanroundfilter$PillarPosition) +
  scale_y_continuous(labels = function(x) x/1000) +
  geom_bar(position = "dodge") +
  ggtitle("Pithouse Counts - Scaled") + xlab("Years calBP") + ylab("pithouses") + labs(fill = "Pithouse Types") +
  scale_x_reverse(breaks = seq(min(-samplecleanroundfilter$sampled), 
                               max(-samplecleanroundfilter$sampled), 100), 
                  labels = seq(min(-samplecleanroundfilter$sampled), 
                               max(-samplecleanroundfilter$sampled), 100) ) +
  theme_minimal()

#Pithouse Line Plot By Type
PithouseLineCounts1000 <- samplecleanround %>%
  group_by(sampled, RunID, PillarPosition) %>%
  tally() %>%
  group_by(PillarPosition, sampled) %>%
  summarise_at(vars(n), sum)

PithouseCounts1000LineType <- ggplot(data = PithouseLineCounts1000) +
  aes(x = -sampled, y = n, group = PillarPosition) +
  geom_line(aes(color = PillarPosition)) +
  ggtitle("Pithouse Counts") + xlab("Years calBP") + ylab("pithouses") +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

#Kernel Density Estimate
PithouseCounts1000KDE <- ggplot(samplecleanroundfilter) +
  aes(-sampled, color = PillarPosition, fill = PillarPosition, alpha = .2) +
  guides(alpha=FALSE) +
  stat_density(adjust = 5) +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()

#Combinded histogram and KDE
PithouseCounts1000HistogramKDE <- ggplot(samplecleanround, aes(x= -sampled)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=100,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666", bw=55) +
  scale_x_reverse(breaks = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100), 
                  labels = seq(min(-samplecleanround$sampled), 
                               max(-samplecleanround$sampled), 100) ) +
  theme_minimal()