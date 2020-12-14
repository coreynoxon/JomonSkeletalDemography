##05-09-plots-growth-rates

##Creates growth rate plots from skeletal data

samplecleanround <- readRDS(here::here("data/derived-data/samplecleanround.rds"))

##testing growth rate calculation
growth_rate <- samplecleanround %>%
  group_by(sampled, RunID) %>%
  tally() %>%
  mutate(Growth = (n - lag(n))/lag(n)*100) %>%
  group_by(sampled) %>%
  mutate(sd = sd(Growth, na.rm = TRUE)) %>%
  group_by(sampled) %>%
  summarise(Growth = mean(Growth, na.rm = TRUE), sd = mean(sd))


growth_rate_total <- samplecleanround %>%
  group_by(sampled) %>%
  tally() %>%
  mutate(Growth = (n - lag(n))/lag(n)*100) %>%
  group_by(sampled) %>%
  mutate(sd = ifelse(is.na(Growth), NA, sd(Growth))) %>%
  group_by(sampled) %>%
  summarise(Growth = mean(Growth, na.rm = TRUE), sd = mean(sd))

GrowthBarplot <- ggplot(data = growth_rate) +
  aes(x = sampled, y = Growth) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = sampled, ymin = Growth-sd, ymax = Growth+sd)) +
  ggtitle("Pithouse Growth Rate") + xlab("Years calBP") + ylab("growth rate") +
  theme_minimal()

GrowthBarplotNoSD <- ggplot(data = growth_rate) +
  aes(x = sampled, y = Growth) +
  geom_bar(stat = "identity") +
  ggtitle("Pithouse Growth Rate") + xlab("Years calBP") + ylab("growth rate") +
  theme_minimal()