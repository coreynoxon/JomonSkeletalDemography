###11-tables
#this section is set aside for the creation of tables
sampleclean <- readRDS(here::here("data/derived-data/sampleclean.rds"))
samplecleanround <- readRDS(here::here("data/derived-data/samplecleanround.rds"))
skeletalsummary <- readRDS(here::here("data/derived-data/skeletalsummary.rds"))

#Pithouse Type Counts per timeblock
PithouseTypeTableData <- samplecleanround %>%
  group_by(sampled) %>%
  count(PillarPosition) %>%
  select(PillarPosition, sampled, n) %>%
  pivot_wider(names_from = sampled, values_from = n) %>%
  arrange(factor(PillarPosition, levels = c("Pit", "Wall", "Main", "MainWall", "Mirror", "uk"))) %>%
  tibble::column_to_rownames('PillarPosition') 

PithouseTypeTableData <- rbind(PithouseTypeTableData, "Total" = colSums(PithouseTypeTableData, na.rm = TRUE)) 

PithouseTypeTable <- formattable::formattable(PithouseTypeTableData, align = c("l", rep("r", NCOL(PithouseTypeTableData) - 1)))

PithouseTypeTableScaled <- PithouseTypeTable[,]/1000

#pithouse counts by type without dates
PithouseTypeTableNoDates <- sampleclean %>%
  count(PillarPosition) %>%
  arrange(factor(PillarPosition, levels = c("Pit", "Wall", "Main", "MainWall", "Mirror", "uk"))) 

SkeletalTable <- skeletalsummary %>%
  select(UniqueID, phase_start, phase_end, n, total_5_19)

#create main pithouse table

PithouseCombinePrep <- merge(pithouse, pits[,c("PithouseID", "Reference")], by = "PithouseID")

PostholeCombinePrep <- merge(cleanposts, posts[,c("PostholeID", "D_Ref")], by = "PostholeID")

PithousePostholeCombinedTable <- full_join(PostholeCombinePrep, PithouseCombinePrep) %>%
  select(PostholeID, PithouseID, BuildNum, Length, Width, RecFloor, NewFloor, Shape, ShapeFormula, Rebuild, PillarPosition, PillarConfig, Adapted_ID, 
         D_Scaled, FinalDepth, mean_depth, sd_depth, mean_diameter, sd_diameter, mean_volume, sd_volume, PostDataUsed, Reference, D_Ref)

readr::write_excel_csv(PithousePostholeCombinedTable, "other-things/PithousePostholeCombinedTable.csv")



#average pithouse measures by type
pithousesizetype <- pitfinal %>%
  group_by(PillarPosition) %>%
  summarize("Average Length" = mean(Length, na.rm = TRUE),
            "Min. Length" = min(Length, na.rm = TRUE),
            "Max. Length" = max(Length, na.rm = TRUE)) %>%
  arrange(factor(PillarPosition, levels = c("Pit", "Wall", "Main", "MainWall", "Mirror", "uk")))

#combine pithouse counts and size measures by type
pithousesizetablecounts <- left_join(PithouseTypeTableNoDates, pithousesizetype) 

readr::write_excel_csv(pithousesizetablecounts, "other-things/pithousesizetablecounts.csv")

#skeletal data used table
skeletal_used_list <- skeletalsummary2 %>%
  filter(RunID == 1)

readr::write_excel_csv(skeletal_used_list, "other-things/skeletal_used_list.csv")

#sampling effect explanation table
PitSubsetClean <- PitSubset %>%
  select(PithouseID, sampled, RunID, NumPhaseStart, NumPhaseEnd)

sampling_explanation_table <- PitSubsetClean %>%
  group_by(PithouseID)

readr::write_excel_csv(sampling_explanation_table, "other-things/sampling_explanation_table.csv")

##skeletal calculations

#calculating average number of sites included per MCMC run
avg_site_n <- skeletalsummary2 %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_subfilter <- skeletalsummary2_subfilter %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim <- skeletalsummary_trim %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim_subfilter <- skeletalsummary_trim_subfilter %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim10 <- skeletalsummary_trim10 %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim10_subfilter <- skeletalsummary_trim10_subfilter %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim10 <- skeletalsummary_trim10 %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim10_subfilter <- skeletalsummary_trim10_subfilter %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim25 <- skeletalsummary_trim25 %>% group_by(RunID) %>% summarise(mean = mean(n))
avg_site_n_trim25_subfilter <- skeletalsummary_trim25_subfilter %>% group_by(RunID) %>% summarise(mean = mean(n))

avg_site_n_trim_summary <- avg_site_n_trim %>%
  summarise(min = min(mean), max = max(mean), mean = mean(mean))
avg_site_n_trim_summary_subfilter <- avg_site_n_trim_subfilter %>%
  summarise(min = min(mean), max = max(mean), mean = mean(mean))
avg_site_n_trim10_summary <- avg_site_n_trim10 %>%
  summarise(min = min(mean), max = max(mean), mean = mean(mean))
avg_site_n_trim10_summary_subfilter <- avg_site_n_trim10_subfilter %>%
  summarise(min = min(mean), max = max(mean), mean = mean(mean))
avg_site_n_trim25_summary <- avg_site_n_trim25 %>%
  summarise(min = min(mean), max = max(mean), mean = mean(mean))
avg_site_n_trim25_summary_subfilter <- avg_site_n_trim25_subfilter %>%
  summarise(min = min(mean), max = max(mean), mean = mean(mean))

#calculating average number of individuals included per MCMC run
tally_per_run <- skeletalsummary2 %>% group_by(RunID) %>% tally()
tally_per_run_subfilter <- skeletalsummary2_subfilter %>% group_by(RunID) %>% tally()
tally_per_run_trim <- skeletalsummary_trim %>% group_by(RunID) %>% tally()
tally_per_run_trim_subfilter <- skeletalsummary_trim_subfilter %>% group_by(RunID) %>% tally()
tally_per_run_trim10 <- skeletalsummary_trim10 %>% group_by(RunID) %>% tally()
tally_per_run_trim10_subfilter <- skeletalsummary_trim10_subfilter %>% group_by(RunID) %>% tally()
tally_per_run_trim25 <- skeletalsummary_trim25 %>% group_by(RunID) %>% tally()
tally_per_run_trim25_subfilter <- skeletalsummary_trim25_subfilter %>% group_by(RunID) %>% tally()


tally_per_run_trim_summary <- tally_per_run_trim %>%
  summarise(min = min(n), max = max(n), mean = mean(n))
tally_per_run_trim_summary_subfilter <- tally_per_run_trim_subfilter %>%
  summarise(min = min(n), max = max(n), mean = mean(n))
tally_per_run_trim10_summary <- tally_per_run_trim10 %>%
  summarise(min = min(n), max = max(n), mean = mean(n))
tally_per_run_trim10_summary_subfilter <- tally_per_run_trim10_subfilter %>%
  summarise(min = min(n), max = max(n), mean = mean(n))
tally_per_run_trim25_summary <- tally_per_run_trim25 %>%
  summarise(min = min(n), max = max(n), mean = mean(n))
tally_per_run_trim25_summary_subfilter <- tally_per_run_trim25_subfilter %>%
  summarise(min = min(n), max = max(n), mean = mean(n))

#counting number of sites per MCMC run
sites_per_run <- skeletalsummary_trim %>%
  group_by(RunID) %>%
  mutate(site_n = n_distinct(UniqueID)) %>%
  summarise(site_n = mean(site_n))

sites_per_run_subfilter <- skeletalsummary_trim_subfilter %>%
  group_by(RunID) %>%
  mutate(site_n = n_distinct(UniqueID)) %>%
  summarise(site_n = mean(site_n))

sites_per_run10 <- skeletalsummary_trim10 %>%
  group_by(RunID) %>%
  mutate(site_n = n_distinct(UniqueID)) %>%
  summarise(site_n = mean(site_n))

sites_per_run10_subfilter <- skeletalsummary_trim10_subfilter %>%
  group_by(RunID) %>%
  mutate(site_n = n_distinct(UniqueID)) %>%
  summarise(site_n = mean(site_n))

sites_per_run25 <- skeletalsummary_trim25 %>%
  group_by(RunID) %>%
  mutate(site_n = n_distinct(UniqueID)) %>%
  summarise(site_n = mean(site_n))

sites_per_run25_subfilter <- skeletalsummary_trim25_subfilter %>%
  group_by(RunID) %>%
  mutate(site_n = n_distinct(UniqueID)) %>%
  summarise(site_n = mean(site_n))


sites_per_run_summary <- sites_per_run %>%
  summarise(min = min(site_n), max = max(site_n), mean = mean(site_n))

sites_per_run_summary_subfilter <- sites_per_run_subfilter %>%
  summarise(min = min(site_n), max = max(site_n), mean = mean(site_n))

sites_per_run10_summary <- sites_per_run10 %>%
  summarise(min = min(site_n), max = max(site_n), mean = mean(site_n))

sites_per_run10_summary_subfilter <- sites_per_run10_subfilter %>%
  summarise(min = min(site_n), max = max(site_n), mean = mean(site_n))

sites_per_run25_summary <- sites_per_run25 %>%
  summarise(min = min(site_n), max = max(site_n), mean = mean(site_n))

sites_per_run25_summary_subfilter <- sites_per_run25_subfilter %>%
  summarise(min = min(site_n), max = max(site_n), mean = mean(site_n))