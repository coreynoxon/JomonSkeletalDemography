#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdatadiameterSD <- sampleclean_trim %>%
  select(sd_diameter, sampled) %>%
  filter(sd_diameter != "uk") %>%
  mutate(sd_diameter = as.numeric(sd_diameter))

hexdatadiameterSD <- sampleclean_trim %>%
  filter(sd_diameter != "uk") %>%
  mutate(sd_diameter = as.numeric(sd_diameter))


hexdatatotaldiameterSD <- ggplot(hexdatadiameterSD, aes(x = -sampled, y = sd_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Diameter SD - Combined Types") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindiameterSD <- ggplot(subset(hexdatadiameterSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Diameter SD - M-type") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldiameterSD <- ggplot(subset(hexdatadiameterSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Diameter SD - MW-type") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotaldiameterscaleSD <- ggplot(hexdatadiameterSD, aes(x = -sampled, y = sd_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter SD - Combined Types") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindiameterscaleSD <- ggplot(subset(hexdatadiameterSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter SD - M-type") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldiameterscaleSD <- ggplot(subset(hexdatadiameterSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_diameter)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Diameter SD - MW-type") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()


#Raster Density
hexdatatotaldensitydiameterSD <- ggplot(data = hexdatadiameterSD, aes(x = -sampled, y = sd_diameter)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter SD - Combined Types") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensitydiameterSD <- ggplot(subset(hexdatadiameterSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_diameter)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter SD - M-type") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensitydiameterSD <- ggplot(subset(hexdatadiameterSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_diameter)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Diameter SD - MW-type") + xlab("Years calBP") + ylab("Diameter SD") +
  scale_x_reverse() +
  theme_minimal()



