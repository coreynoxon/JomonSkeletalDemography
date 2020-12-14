#02-data-prep

#pithouse data
#clean up data from import, create and populate columns for mean and SD values

#Clean unnecessary columns
cleanpits <- select(pits, -c(PubPeriod, PhaseStart, PhaseEnd, PhaseStartRom, PhaseEndRom, Reference, Notes, TopologyNotes))

cleanpits$Length <- as.numeric(cleanpits$Length)
cleanpits$Width <- as.numeric(cleanpits$Width)
cleanpits$RecFloor <- as.numeric(cleanpits$RecFloor)

cleanposts <- select(posts, -c(Diameter, Major, Minor, Scale1m, PostType, D_Ref, Notes)) %>%
  mutate(FinalDepth = case_when(is.na(FlrLvlDepScale) ~ Depth,
                                FlrLvlDepScale > 0 ~ FlrLvlDepScale,
                                FlrLvlDepScale == 0 ~ Depth)) %>%
  left_join(select(cleanpits, PithouseID, PillarPosition), by = ("id" = "PithouseID"))

#Calculate oval shapted pithouse floor area
oval <- filter(cleanpits, ShapeFormula == "oval") %>%
  mutate(NewFloor = as.numeric(Length)*as.numeric(Width)*0.8)

#Calculate circle shaped pithouse floor area
circle <- filter(cleanpits, ShapeFormula == "circle") %>%
  mutate(NewFloor =  pi / 4 * as.numeric(Length)^2) 

#Combine oval and circle shaped pithouse floor areas back into dataframe  
pitfinal <- full_join(oval, circle)
pitfinal <- full_join(pitfinal, cleanpits)

#cleaning posthole data to provide averages and SDs for depth and diameter
postfinal <- cleanposts %>%
  mutate(volume = pi * ((0.5 * D_Scaled)^2) * FinalDepth)%>%
  group_by(PithouseID) %>%
  summarise(mean_depth = mean(FinalDepth), 
            sd_depth = sd(FinalDepth), 
            mean_diameter = mean(D_Scaled), 
            sd_diameter = sd(D_Scaled),
            mean_volume = mean(volume),
            sd_volume = sd(volume),
            counts = n_distinct(PostholeID))

#combine pithouse and posthole data  
pithouse <- full_join(pitfinal, postfinal, by = "PithouseID")%>%
  drop_na("TotalPhaseRom") %>% 
  mutate_at(vars(topo_after1:topo_after13), ~replace(., which(!(. %in% PithouseID)), NA))

#reposition NA values after actual values  
pithouse[paste0("topo_after", 1:13)] <- t(apply(pithouse[paste0("topo_after", 1:13)], 1, function(x) c(x[!is.na(x)], x[is.na(x)])))

#add blank sampled column
pithouse["sampled"] <- NA

#create data frames for analysis
sampleclean <- pithouse 
temp_sampleclean<-sampleclean #temp_pithouse_join used for dynamically created samples
temp_sampleclean$sampled <- NA #blanking out the sample column so I can check against NA for the dynamic detereminatination.
temp_sampleclean %>% mutate_if(is.factor, as.character) -> temp_sampleclean #change factors to characters

#save cleanpost data for later use
saveRDS(cleanposts, here::here("data/derived-data/cleanposts.rds"))

#skeletal data
#filter for necessary columns, create percentage columns for 15p5 and 5p0 ratios, prepare data for aoristic analysis and MCMC

#clean data
skeletalclean <- skeletalDF %>%
  filter(!age_min_noxon %in% c("U","X")) %>%
  filter(!is.na(calBPmin) | !is.na(calBPmax)) %>%
  filter(!is.na(age_min_noxon) | !is.na(age_max_noxon)) %>%
  filter(age_min_noxon != "" | age_max_noxon != "") %>%
  select(master_id, region_number, site_name_dup_fix, site_name_rom, site_period_occurances, age_range, age_min_noxon,  age_max_noxon, over_5_values,
         output_values_0_5, output_values_5_19, period_range, period_start, period_end, phase_start, phase_end)

#create percentages for 5-19 column
skeletalclean <- skeletalclean %>%
  rowwise() %>%
  mutate(percent_5_19 = mean(age_min_noxon:age_max_noxon %in% 5:19))

#create percentages for under 5 
skeletalclean <- skeletalclean %>%
  rowwise() %>%
  mutate(percent_0_4 = mean(age_min_noxon:age_max_noxon %in% 0:4))

#create percentages for over 5
skeletalclean <- skeletalclean %>%
  rowwise() %>%
  mutate(percent_5_over = mean(age_min_noxon:age_max_noxon %in% 5:100))

#group and summarize
skeletalsummary <- skeletalclean %>%
  group_by(site_name_rom, phase_start, phase_end) %>%
  add_tally() %>%
  summarise(total_0_4 = sum(percent_0_4), total_over_5 = sum(over_5_values), 
            total_5_19 = sum(percent_5_19), n = mean(n)) %>%
  ungroup() %>%
  mutate(UniqueID = make.unique(site_name_rom)) %>%
  rowwise() %>%
  mutate(ratio_5_19 = total_5_19 / total_over_5, 
         ratio_0_5 = total_0_4 / total_over_5) %>%
  select(UniqueID, everything()) %>%
  filter(n > 4)

#create temp_skeletal summary for dynamic sampling
temp_skeletalsummary <- skeletalsummary 

#add sampled column
temp_skeletalsummary$sampled <- NA

saveRDS(skeletalsummary, here::here("data/derived-data/skeletalsummary.rds"))