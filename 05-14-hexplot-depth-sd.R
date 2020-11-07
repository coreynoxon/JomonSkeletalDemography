#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdatadepthSD <- sampleclean_trim %>%
  select(sd_depth, sampled) %>%
  filter(sd_depth != "uk") %>%
  mutate(sd_depth = as.numeric(sd_depth))

hexdatadepthSD <- sampleclean_trim %>%
  filter(sd_depth != "uk") %>%
  mutate(sd_depth = as.numeric(sd_depth))


hexdatatotaldepthSD <- ggplot(hexdatadepthSD, aes(x = -sampled, y = sd_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Depth SD - Combined Types") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindepthSD <- ggplot(subset(hexdatadepthSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Depth SD - M-type") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldepthSD <- ggplot(subset(hexdatadepthSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Depth SD - MW-type") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotaldepthscaleSD <- ggplot(hexdatadepthSD, aes(x = -sampled, y = sd_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth SD - Combined Types") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindepthscaleSD <- ggplot(subset(hexdatadepthSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth SD - M-type") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldepthscaleSD <- ggplot(subset(hexdatadepthSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth SD - MW-type") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

#Raster Density
hexdatatotaldensitydepthSD <- ggplot(data = hexdatadepthSD, aes(x = -sampled, y = sd_depth)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth SD - Combined Types") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensitydepthSD <- ggplot(subset(hexdatadepthSD, PillarPosition == "Main"), aes(x = -sampled, y = sd_depth)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth SD - M-type") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensitydepthSD <- ggplot(subset(hexdatadepthSD, PillarPosition == "MainWall"), aes(x = -sampled, y = sd_depth)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth SD - MW-type") + xlab("Years calBP") + ylab("Depth SD") +
  scale_x_reverse() +
  theme_minimal()



