samplecleanroundneg <- readRDS(here::here("data/derived-data/samplecleanroundneg.rds"))

#Total Pithouse Length per period
PithouseLengthSumTotal <- samplecleanroundneg %>%
  group_by(PillarPosition, sampled) %>%
  summarise(LengthSum = sum(Length, na.rm = TRUE))

PithouseLengthSumBarplotSeries <- ggplot(data = PithouseLengthSumTotal) +
  aes(x = sampled, y = LengthSum) +
  scale_y_continuous(labels = function(x) x/1000) +
  geom_bar(stat = "identity", aes(fill = PillarPosition), position = "dodge") +
  ggtitle("Pithouse Length Sum") + xlab("Years calBP") + ylab("Total Length (m)") +
  scale_x_reverse(labels = PithouseLengthSumTotal$sampled, breaks = PithouseLengthSumTotal$sampled) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_minimal()

#Total Pithouse Length per period with uk-types and P152-1029-1 removed
PithouseLengthSumTotalfilter <- subset(samplecleanroundneg, !is.na(Length) & PillarPosition != "uk" & PithouseID != "P152-1029-1") %>%
  group_by(PillarPosition, sampled) %>%
  summarise(LengthSum = sum(Length, na.rm = TRUE))

PithouseLengthSumBarplotSeriesfilter <- ggplot(data = PithouseLengthSumTotalfilter) +
  aes(x = sampled, y = LengthSum) +
  scale_y_continuous(labels = function(x) x/1000) +
  geom_bar(stat = "identity", aes(fill = PillarPosition), position = "dodge") +
  ggtitle("Pithouse Length Sum") + xlab("Years calBP") + ylab("Total Length (m)") +
  scale_x_reverse(labels = PithouseLengthSumTotalfilter$sampled, breaks = PithouseLengthSumTotalfilter$sampled) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_minimal()

#average pithouse length per period for each type

PithouseLengthMean <- samplecleanroundneg %>%
  group_by(PillarPosition, sampled) %>%
  summarise(LengthMean = mean(Length, na.rm = TRUE))

PithouseLengthMeanBarplotSeries <- ggplot(data = PithouseLengthMean) +
  aes(x = sampled, y = LengthMean) +
  geom_bar(stat = "identity", aes(fill = PillarPosition), position = "dodge") +
  ggtitle("Pithouse Length Mean") + xlab("Years calBP") + ylab("Mean Length (m)") +
  scale_x_reverse(labels = PithouseLengthMean$sampled, breaks = PithouseLengthMean$sampled) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_minimal()

#average pithouse length per period for each type with uk-types and P152-1029-1 removed

PithouseLengthMeanfilter <- subset(samplecleanroundneg, !is.na(Length) & PillarPosition != "uk" & PithouseID != "P152-1029-1") %>%
  group_by(PillarPosition, sampled) %>%
  summarise(LengthMean = mean(Length, na.rm = TRUE))

PithouseLengthMeanBarplotSeriesfilter <- ggplot(data = PithouseLengthMeanfilter) +
  aes(x = sampled, y = LengthMean) +
  geom_bar(stat = "identity", aes(fill = PillarPosition), position = "dodge") +
  ggtitle("Pithouse Length Mean") + xlab("Years calBP") + ylab("Mean Length (m)") +
  scale_x_reverse(labels = PithouseLengthMeanfilter$sampled, breaks = PithouseLengthMeanfilter$sampled) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme_minimal()