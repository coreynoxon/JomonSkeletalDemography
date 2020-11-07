#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdatavolume <- sampleclean_trim %>%
  select(mean_volume, sampled) %>%
  filter(mean_volume != "uk") %>%
  mutate(mean_volume = as.numeric(mean_volume))

hexdatavolume <- sampleclean_trim %>%
  filter(mean_volume != "uk") %>%
  mutate(mean_volume = as.numeric(mean_volume))


hexdatatotalvolume <- ggplot(hexdatavolume, aes(x = -sampled, y = mean_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Volume - Combined Types") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

hexdatamainvolume <- ggplot(subset(hexdatavolume, PillarPosition == "Main"), aes(x = -sampled, y = mean_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Volume - M-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwallvolume <- ggplot(subset(hexdatavolume, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Volume - MW-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotalvolumescale <- ggplot(hexdatavolume, aes(x = -sampled, y = mean_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume - Combined Types") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

hexdatamainvolumescale <- ggplot(subset(hexdatavolume, PillarPosition == "Main"), aes(x = -sampled, y = mean_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume - M-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwallvolumescale <- ggplot(subset(hexdatavolume, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_volume)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Volume - MW-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

#Raster Density
hexdatatotaldensityvolume <- ggplot(data = hexdatavolume, aes(x = -sampled, y = mean_volume)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume - Combined") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensityvolume <- ggplot(subset(hexdatavolume, PillarPosition == "Main"), aes(x = -sampled, y = mean_volume)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume - M-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensityvolume <- ggplot(subset(hexdatavolume, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_volume)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Volume - MW-type") + xlab("Years calBP") + labs(y = bquote("Volume" ~ (m^3))) +
  scale_x_reverse() +
  theme_minimal()



