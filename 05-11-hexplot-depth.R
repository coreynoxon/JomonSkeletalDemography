#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdatadepth <- sampleclean_trim %>%
  filter(mean_depth != "uk") %>%
  mutate(mean_depth = as.numeric(mean_depth))


hexdatatotaldepth <- ggplot(hexdatadepth, aes(x = -sampled, y = mean_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Depth - Combined Types") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindepth <- ggplot(subset(hexdatadepth, PillarPosition == "Main"), aes(x = -sampled, y = mean_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Depth - M-type") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldepth <- ggplot(subset(hexdatadepth, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Posthole Depth - MW-type") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotaldepthscale <- ggplot(hexdatadepth, aes(x = -sampled, y = mean_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth - Combined Types") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindepthscale <- ggplot(subset(hexdatadepth, PillarPosition == "Main"), aes(x = -sampled, y = mean_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth - M-type") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldepthscale <- ggplot(subset(hexdatadepth, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_depth)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Posthole Depth - MW-type") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

#Raster Density
hexdatatotaldensitydepth <- ggplot(data = hexdatadepth, aes(x = -sampled, y = mean_depth)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth - Combined Types") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensitydepth <- ggplot(subset(hexdatadepth, PillarPosition == "Main"), aes(x = -sampled, y = mean_depth)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth - M-type") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensitydepth <- ggplot(subset(hexdatadepth, PillarPosition == "MainWall"), aes(x = -sampled, y = mean_depth)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ggtitle("Posthole Depth - MW-type") + xlab("Years calBP") + ylab("Depth (m)") +
  scale_x_reverse() +
  theme_minimal()



