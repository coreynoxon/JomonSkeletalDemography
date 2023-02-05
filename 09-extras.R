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

#skeletal summary csv export - filters out sites with less than 5 individuals
write.csv(skeletalsummary, file = 'figures/tables/skeletalsummary.csv')

#unfiltered skeletal summary csv export - does not filtour out sites based on number of individuals present
skeletalsummaryunfiltered <- skeletalclean %>%
  group_by(site_name_rom, phase_start, phase_end) %>%
  add_tally() %>%
  summarise(total_0_4 = sum(percent_0_4), total_over_5 = sum(over_5_values), 
            total_5_19 = sum(percent_5_19), n = mean(n)) %>%
  ungroup() %>%
  mutate(UniqueID = make.unique(site_name_rom)) %>%
  rowwise() %>%
  mutate(ratio_5_19 = total_5_19 / total_over_5, 
         ratio_0_5 = total_0_4 / total_over_5) %>%
  select(UniqueID, everything())

write.csv(skeletalsummaryunfiltered, file = 'figures/tables/skeletalsummaryunfiltered.csv')