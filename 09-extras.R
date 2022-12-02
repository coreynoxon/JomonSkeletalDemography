###15-extras
#This section is set aside for incidental plots not focused on specific aspects of the study

#koyama population plot
koyama<- read_csv("other-things/Koyama Prefecture Site Counts Chart - Combined - Trim.csv")
koyamaclean <- koyama %>%
  group_by(Region) %>%
  summarise(Initial = sum(Initial), 
            Early = sum(Early), 
            Middle = sum(Middle), 
            Late = sum(Late), 
            Final = sum(Final))
koyamagather <- gather(koyamaclean, Period, Sites, c("Initial", "Early","Middle", "Late", "Final"))
koyamagather$Period <- factor(koyamagather$Period, level = c("Initial", "Early","Middle", "Late", "Final"))
koyamaplot <- ggplot(koyamagather, aes(x = Period, y = Sites, group = Region, color = Region)) +
  geom_line(size = 1) +
  theme_minimal()
ggsave(path = "figures", filename = "koyamasites.pdf")
