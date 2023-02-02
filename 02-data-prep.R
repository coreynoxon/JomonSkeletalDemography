#02-data-prep

#skeletal data
#filter for necessary columns, create percentage columns for 15p5 and 5p0 ratios, prepare data for aoristic analysis and MCMC

#clean data
skeletalclean <- skeletalDF %>%
  filter(!age_min_noxon %in% c("U","X")) %>%
  filter(!is.na(calBPmin) | !is.na(calBPmax)) %>%
  filter(!is.na(age_min_noxon) | !is.na(age_max_noxon)) %>%
  filter(age_min_noxon != "" | age_max_noxon != "") %>%
  select(master_id, region_number, site_name_dup_fix, site_name_rom, site_period_occurances, age_range, age_min_noxon,  age_max_noxon, over_5_values,
         output_values_0_5, output_values_5_19, period, period_start, period_end, phase_start, phase_end)

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