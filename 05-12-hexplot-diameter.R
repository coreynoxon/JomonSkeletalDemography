#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdatadiameter <- sampleclean_trim %>%
  select(mean_diameter, sampled) %>%
  filter(mean_diameter != "uk") %>%
  mutate(mean_diameter = as.numeric(mean_diameter))

hexdatadiameter <- sampleclean_trim %>%
  filter(mean_diameter != "uk") %>%
  mutate(mean_diameter = as.numeric(mean_diameter))


hexdatatotaldiameter <- ggplot(hexdatadiameter, aes(x = -sampled, y = mean_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Diameter - Combined Types") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindiameter <- ggplot(subset(hexdatadiameter, PillarPosition == "Main"), aes(x = -sampled, y = mean_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Diameter - M-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldiameter <- ggplot(subset(hexdatadiameter, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Diameter - MW-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotaldiameterscale <- ggplot(hexdatadiameter, aes(x = -sampled, y = mean_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter - Combined Types") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindiameterscale <- ggplot(subset(hexdatadiameter, PillarPosition == "Main"), aes(x = -sampled, y = mean_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter - M-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldiameterscale <- ggplot(subset(hexdatadiameter, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter - MW-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()


#Raster Density
hexdatatotaldensitydiameter <- ggplot(data = hexdatadiameter, aes(x = -sampled, y = mean_diameter)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter - Combined Types") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensitydiameter <- ggplot(subset(hexdatadiameter, PillarPosition == "Main"), aes(x = -sampled, y = mean_diameter)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter - M-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensitydiameter <- ggplot(subset(hexdatadiameter, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_diameter)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter - M-type") + xlab("Years calBP") + ylab("Diameter (m)") +
  scale_x_reverse() +
  theme_minimal()



