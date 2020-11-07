#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdatavolumeSD <- sampleclean_trim %>%
  select(sd_volume, sampled) %>%
  filter(sd_volume != "uk") %>%
  mutate(sd_volume = as.numeric(sd_volume))

hexdatavolumeSD <- sampleclean_trim %>%
  filter(sd_volume != "uk") %>%
  mutate(sd_volume = as.numeric(sd_volume))


hexdatatotalvolumeSD <- ggplot(hexdatavolumeSD, aes(x = -sampled, y = sd_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Volume SD - Combined Types") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainvolumeSD <- ggplot(subset(hexdatavolumeSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Volume SD - M-type") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwallvolumeSD <- ggplot(subset(hexdatavolumeSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Volume SD - MW-type") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotalvolumescaleSD <- ggplot(hexdatavolumeSD, aes(x = -sampled, y = sd_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume SD - Combined Types") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainvolumescaleSD <- ggplot(subset(hexdatavolumeSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume SD - M-type") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwallvolumescaleSD <- ggplot(subset(hexdatavolumeSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume SD - MW-type") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

#Raster Density
hexdatatotaldensityvolumeSD <- ggplot(data = hexdatavolumeSD, aes(x = -sampled, y = sd_volume)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume SD - Combined Types") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensityvolumeSD <- ggplot(subset(hexdatavolumeSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_volume)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume SD - M-type") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensityvolumeSD <- ggplot(subset(hexdatavolumeSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_volume)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume SD - MW-type") + xlab("Years calBP") + ylab("Volume SD") +
  scale_x_reverse() +
  theme_minimal()



