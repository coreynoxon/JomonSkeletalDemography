###11-tables
#this section is set aside for the creation of tables
skeletalDF <- read_csv("data/raw-data/MA Skeletal Data - Region 4 trim.csv", na = c("#VALUE!", "#N/A", "uk"))
skeletalsummary <- readRDS(here::here("data/derived-data/skeletalsummary.rds"))
skeletalsummary2 <- readRDS(here::here("data/derived-data/skeletalsummary2.rds"))

#Border Cases - age range crosses categories
TotalCases <- skeletalDF %>% filter(!is.na(percentage_value_5_19))
BorderCases <- skeletalDF %>% filter(output_values_5_19 < 1 & output_values_5_19 != 0)


#Skeletal Table

SkeletalTable <- skeletalsummary %>%
  select(UniqueID, phase_start, phase_end, n, total_5_19)

#skeletal data used table
skeletal_used_list <- skeletalsummary2 %>%
  filter(RunID == 1)

readr::write_excel_csv(skeletal_used_list, "other-things/skeletal_used_list.csv")

##skeletal calculations

#calculating average number of individuals included per MC run
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

#counting number of sites per skeletal MC run
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