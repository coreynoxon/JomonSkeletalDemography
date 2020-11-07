#hex plots
library(hexbin)

sampleclean_trim <- readRDS(here::here("data/derived-data/sampleclean_trim.rds"))

#set colors
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

#arrange data for each pithouse type and the collective group
hexdata <- sampleclean_trim %>%
  filter(Length != "uk") %>%
  mutate(Length = as.numeric(Length))

#plots

hexdatatotal <- ggplot(hexdata, aes(x = -sampled, y = Length)) +
         geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Pithouse Length") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatapit <- ggplot(subset(hexdata, PillarPosition == "Pit"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Pithouse Length - P-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatawall <- ggplot(subset(hexdata, PillarPosition == "Wall"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Pithouse Length - W-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamain <- ggplot(subset(hexdata, PillarPosition == "Main"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Pithouse Length - M-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwall <- ggplot(subset(hexdata, PillarPosition == "MainWall"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Pithouse Length - MW-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamirror <- ggplot(subset(hexdata, PillarPosition == "Mirror"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis") +
  ggtitle("Pithouse Length - HM-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()


#Scaled Versions
hexdatatotalscale <- ggplot(hexdata, aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Pithouse Length") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatapitscale <- ggplot(subset(hexdata, PillarPosition == "Pit"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Pithouse Length - P-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatawallscale <- ggplot(subset(hexdata, PillarPosition == "Wall"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Pithouse Length - W-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainscale <- ggplot(subset(hexdata, PillarPosition == "Main"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Pithouse Length - M-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwallscale <- ggplot(subset(hexdata, PillarPosition == "MainWall"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Pithouse Length - MW-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamirrorscale <- ggplot(subset(hexdata, PillarPosition == "Mirror"), aes(x = -sampled, y = Length)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", labels = function(x) x/1000) +
  ggtitle("Pithouse Length - HM-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()


#Raster Density
hexdatatotaldensity <- ggplot(data = hexdata, aes(x = -sampled, y = Length)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ylim(1,10) +
  ggtitle("Pithouse Length") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatapitdensity <- ggplot(subset(hexdata, PillarPosition == "Pit"), aes(x = -sampled, y = Length)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ylim(1,6) +
  ggtitle("Pithouse Length - P-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatawalldensity <- ggplot(subset(hexdata, PillarPosition == "Wall"), aes(x = -sampled, y = Length)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ylim(1,7) +
  ggtitle("Pithouse Length - W-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamaindensity <- ggplot(subset(hexdata, PillarPosition == "Main"), aes(x = -sampled, y = Length)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ylim(1,10) +
  ggtitle("Pithouse Length - M-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamainwalldensity <- ggplot(subset(hexdata, PillarPosition == "MainWall"), aes(x = -sampled, y = Length)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ylim(1,10) +
  ggtitle("Pithouse Length - MW-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()

hexdatamirrordensity <- ggplot(subset(hexdata, PillarPosition == "Mirror"), aes(x = -sampled, y = Length)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral") +
  xlim(5400, 4400) +
  ylim(1,8) +
  ggtitle("Pithouse Length - HM-type") + xlab("Years calBP") + ylab("Length (m)") +
  scale_x_reverse() +
  theme_minimal()


