### 1 - House Keeping ---
setwd("/conf/WholeSystMod_models/git/WSM/Nuala/WS8/census exploration")

#load packages
if(is.na(utils::packageDate("pacman"))) install.packages("pacman")
pacman::p_load(tidyverse, phsmethods, odbc, glue, janitor, scales, phsstyles, stringr)

#set filepath
lookup_path <- "/conf/linkage/output/lookups/Unicode"

#council area lookup
council_area_lookup <- readRDS(glue("{lookup_path}/Deprivation/postcode_2024_1_simd2020v2.rds")) %>%
  select(council_area_2019 = ca2019, council_area = ca2019name) %>% 
  distinct()

# read in care home places file and tidy
care_places_df <- read_csv("file12a_number_of_registered_places.csv") %>% 
  select(Date, council_area_2019 = CA, MainClientGroup, Sector, number_of_places = Value) %>% 
  filter(!is.na(Date)) %>% 
  mutate(Date = ymd(Date)) %>% 
  clean_names() %>% 
  left_join(council_area_lookup) %>% 
  mutate(council_area = ifelse(council_area_2019 == "S92000003","Scotland",council_area)) %>%
  mutate(financial_year = extract_fin_year(date)) %>% 
  write_csv(file = "care_places_cleaned.csv", append = FALSE)

# produce summary table of care home places
care_places_summary <- care_places_df %>% 
  group_by(financial_year, council_area, sector, main_client_group) %>% 
  summarise(number_of_places = sum(number_of_places)) %>% 
  write_csv(file = "output/care places/care_places_summary.csv", append = FALSE)
# all dates are 31st march of each year, so data is yearly and by CA and scotland total
# with sector breakdown showing: private, local authority, and non-profit
# main client group breakdown all adults, over 65, mental health, physical/sensory impairment, learning disability

# create care places chart broken down by main client group across council areas
care_places_client_ca <- care_places_df %>% 
  filter(sector == "All Sectors") %>% 
  filter(!main_client_group == "All Adults") %>% 
  filter(!council_area == "Scotland") %>% 
  group_by(financial_year, main_client_group, council_area) %>% 
  summarise(number_of_places = sum(number_of_places))

care_places_client_ca_plot <- ggplot() +
  geom_line(data = care_places_client_ca, 
            aes(x = financial_year, y = number_of_places, group = main_client_group, colour = main_client_group)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) + facet_wrap("council_area") +
  labs(title = paste0("Number of Care Home Places by Main Client Group Across Council Areas"),
       y = "Number of Places", 
       x = "Date", 
       colour = "Main Client Group",
       fill = "Main Client Group")

care_places_client_ca_plot
ggsave(paste0("output/care places/", "Number of Care Places by Main Client Group Across Council Areas.png"),
       width = 12, height = 7)

# create care places chart broken down by main client group for scotland
care_places_client_scot <- care_places_df %>% 
  filter(sector == "All Sectors") %>% 
  filter(!main_client_group == "All Adults") %>% 
  filter(council_area == "Scotland") %>% 
  group_by(financial_year, main_client_group) %>% 
  summarise(number_of_places = sum(number_of_places))

care_places_client_scot_plot <- ggplot() +
  geom_line(data = care_places_client_scot, 
            aes(x = financial_year, y = number_of_places, group = main_client_group, colour = main_client_group)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Care Home Places by Main Client Group"),
       y = "Number of Places", 
       x = "Date", 
       colour = "Main Client Group",
       fill = "Main Client Group")

care_places_client_scot_plot
ggsave(paste0("output/care places/", "Number of Care Places by Main Client Group in Scotland.png"),
       width = 12, height = 7)

# create care home places chart broken down by sector by council area
care_places_sector_ca <- care_places_df %>% 
  filter(!sector == "All Sectors") %>% 
  filter(main_client_group == "All Adults") %>% 
  filter(!council_area == "Scotland") %>% 
  group_by(financial_year, council_area, sector) %>% 
  summarise(number_of_places = sum(number_of_places))

care_places_sector_ca_plot <- ggplot() +
  geom_line(data = care_places_sector_ca, 
            aes(x = financial_year, y = number_of_places, group = sector, colour = sector)) +
  facet_wrap('council_area') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Care Home Places by Sector"),
       y = "Number of Places", 
       x = "Financial Year", 
       colour = "Sector",
       fill = "Sector")

care_places_sector_ca_plot
ggsave(paste0("output/care places/", "Number of Care Places by Sector across Council Areas.png"),
       width = 12, height = 7)

# create care places chart broken down by sector for scotland
care_places_sector_scot <- care_places_df %>% 
  filter(!sector == "All Sectors") %>% 
  filter(main_client_group == "All Adults") %>% 
  filter(council_area == "Scotland") %>% 
  group_by(financial_year, sector) %>% 
  summarise(number_of_places = sum(number_of_places))

care_places_sector_scot_plot <- ggplot() +
  geom_line(data = care_places_sector_scot, 
            aes(x = financial_year, y = number_of_places, group = sector, colour = sector)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40000)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Care Home Places by Sector"),
       y = "Number of Places", 
       x = "Financial Year", 
       colour = "Sector",
       fill = "Sector")

care_places_sector_scot_plot
ggsave(paste0("output/care places/", "Number of Care Places by Sector in Scotland.png"),
       width = 12, height = 7)

# read in residents by demographic and tidy
residents_demographic_df <- read_csv("file9b_number_of_long_stay_residents_by_sex_and_age.csv") %>% 
  select(Date, KeyStatistic, council_area_2019 = CA, MainClientGroup, number_of_people = Value) %>% 
  filter(!is.na(Date)) %>% 
  filter(!is.na(number_of_people)) %>% 
  mutate(Date = ymd(Date)) %>% 
  clean_names() %>% 
  filter(!main_client_group == "Older People Aged 65 and Older") %>% 
  left_join(council_area_lookup) %>% 
  mutate(council_area = ifelse(council_area_2019 == "S92000003","Scotland",council_area)) %>%
  mutate(financial_year = extract_fin_year(date)) %>% 
  mutate(key_statistic = str_remove(key_statistic,"Number of ")) %>% 
  mutate(key_statistic = str_remove(key_statistic,"Long Stay Residents")) %>% 
  mutate(key_statistic = str_remove(key_statistic," Aged "))  %>% 
  separate(key_statistic, sep = " ", into = c("sex", "age_group"), extra="merge") %>% 
  filter(!age_group == "") %>% 
  filter(!sex == "65") %>% 
  write_csv(file = "residents_by_demographic_cleaned.csv", append = FALSE)

# produce summary table of residents by demographic
residents_demographic_summary <- residents_demographic_df %>% 
  group_by(financial_year, council_area, sex, age_group) %>% 
  summarise(number_of_people = sum(number_of_people)) %>% 
  write_csv(file = "output/residents by demographic/residents_demographic_summary.csv", append = FALSE)
# not broken down by sector, broken down by age and sex, only clients given are: all adults or over 65s (so just kept in all adults)
# data is yearly and by council area and scotland total

# create charts for resident by demographic
# create residents chart broken down by sex by council area
residents_sex_ca <- residents_demographic_df %>% 
  filter(!council_area == "Scotland") %>% 
  group_by(financial_year, council_area, sex) %>% 
  summarise(number_of_people = sum(number_of_people))

residents_sex_ca_plot <- ggplot() +
  geom_line(data = residents_sex_ca, 
            aes(x = financial_year, y = number_of_people, group = sex, colour = sex)) +
  facet_wrap('council_area') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Residents by Sex"),
       y = "Number of Residents", 
       x = "Financial Year", 
       colour = "Sex",
       fill = "Sex")

residents_sex_ca_plot
ggsave(paste0("output/residents by demographic/", "Number of Residents by Sex across Council Areas.png"),
       width = 12, height = 7)

# create residents chart broken down by sex for Scotland
residents_sex_scot <- residents_demographic_df %>% 
  filter(council_area == "Scotland") %>% 
  group_by(financial_year, council_area, sex) %>% 
  summarise(number_of_people = sum(number_of_people))

residents_sex_scot_plot <- ggplot() +
  geom_line(data = residents_sex_scot, 
            aes(x = financial_year, y = number_of_people, group = sex, colour = sex)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30000)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Residents by Sex"),
       y = "Number of Residents", 
       x = "Financial Year", 
       colour = "Sex",
       fill = "Sex")

residents_sex_scot_plot
ggsave(paste0("output/residents by demographic/", "Number of Residents by Sex across Scotland.png"),
       width = 12, height = 7)

# create residents chart broken down by age group by council area
residents_age_ca <- residents_demographic_df %>% 
  filter(!council_area == "Scotland") %>% 
  group_by(financial_year, council_area, age_group) %>% 
  summarise(number_of_people = sum(number_of_people))

residents_age_ca_plot <- ggplot() +
  geom_line(data = residents_age_ca, 
            aes(x = financial_year, y = number_of_people, group = age_group, colour = age_group)) +
  facet_wrap('council_area') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2000)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Residents by Age Group"),
       y = "Number of Residents", 
       x = "Financial Year", 
       colour = "Age Group",
       fill = "Age Group")

residents_age_ca_plot
ggsave(paste0("output/residents by demographic/", "Number of Residents by Age Group across Council Areas.png"),
       width = 12, height = 7)

# create residents chart broken down by sex for Scotland
residents_age_scot <- residents_demographic_df %>% 
  filter(council_area == "Scotland") %>% 
  group_by(financial_year, council_area, age_group) %>% 
  summarise(number_of_people = sum(number_of_people))

residents_age_scot_plot <- ggplot() +
  geom_line(data = residents_age_scot, 
            aes(x = financial_year, y = number_of_people, group = age_group, colour = age_group)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 16000)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Number of Residents by Age Group"),
       y = "Number of Residents", 
       x = "Financial Year", 
       colour = "Age Group",
       fill = "Age Group")

residents_age_scot_plot
ggsave(paste0("output/residents by demographic/", "Number of Residents by Age Group across Scotland.png"),
       width = 12, height = 7)

#read in and clean percentage occupancy file
percentage_occupancy_df <- read_csv("file6_percentage_occupancy.csv")  %>% 
  select(Date, council_area_2019 = CA, MainClientGroup, Sector, percentage_occupancy = Value) %>% 
  filter(!is.na(Date)) %>% 
  filter(!is.na(percentage_occupancy)) %>% 
  mutate(Date = ymd(Date)) %>% 
  clean_names() %>% 
  left_join(council_area_lookup) %>% 
  mutate(council_area = ifelse(council_area_2019 == "S92000003","Scotland",council_area)) %>%
  mutate(financial_year = extract_fin_year(date)) %>% 
  write_csv(file = "percentage_occupancy_cleaned.csv", append = FALSE)

# produce summary table of care home places
occupancy_summary <- percentage_occupancy_df %>% 
  group_by(financial_year, council_area, sector, main_client_group) %>% 
  summarise(percentage_occupancy = mean(percentage_occupancy)) %>% 
  write_csv(file = "output/occupancy/percentage_occupancy_summary.csv", append = FALSE)
# all dates are 31st march of each year, so data is yearly and by CA and scotland total
# with sector breakdown showing: private, local authority, and non-profit
# main client group breakdown all adults, over 65, mental health, physical/sensory impairment, learning disability

# create occupancy chart broken down by main client group across council areas
occupancy_client_ca <- percentage_occupancy_df %>% 
  filter(sector == "All Sectors") %>% 
  filter(!main_client_group == "All Adults") %>% 
  filter(!council_area == "Scotland") %>% 
  group_by(financial_year, main_client_group, council_area) %>% 
  summarise(percentage_occupancy = mean(percentage_occupancy))

occupancy_client_ca_plot <- ggplot() +
  geom_line(data = occupancy_client_ca, 
            aes(x = financial_year, y = percentage_occupancy, group = main_client_group, colour = main_client_group)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) + facet_wrap("council_area") +
  labs(title = paste0("Percentage Occupancy by Main Client Group Across Council Areas"),
       y = "Percentage Occupancy", 
       x = "Date", 
       colour = "Main Client Group",
       fill = "Main Client Group")

occupancy_client_ca_plot
ggsave(paste0("output/occupancy/", "Percentage Occupancy by Main Client Group Across Council Areas.png"),
       width = 12, height = 7)

# create occupancy chart broken down by main client group for scotland
occupancy_client_scot <- percentage_occupancy_df %>% 
  filter(sector == "All Sectors") %>% 
  filter(!main_client_group == "All Adults") %>% 
  filter(council_area == "Scotland") %>% 
  group_by(financial_year, main_client_group) %>% 
  summarise(percentage_occupancy = mean(percentage_occupancy))

occupancy_client_scot_plot <- ggplot() +
  geom_line(data = occupancy_client_scot, 
            aes(x = financial_year, y = percentage_occupancy, group = main_client_group, colour = main_client_group)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Percentage Occupancy by Main Client Group"),
       y = "Percentage Occupancy", 
       x = "Date", 
       colour = "Main Client Group",
       fill = "Main Client Group")

occupancy_client_scot_plot
ggsave(paste0("output/occupancy/", "Percentage Occupancy by Main Client Group in Scotland.png"),
       width = 12, height = 7)

# create occupancy places chart broken down by sector by council area
occupancy_sector_ca <- percentage_occupancy_df %>% 
  filter(!sector == "All Sectors") %>% 
  filter(main_client_group == "All Adults") %>% 
  filter(!council_area == "Scotland") %>% 
  group_by(financial_year, council_area, sector) %>% 
  summarise(percentage_occupancy = mean(percentage_occupancy))

occupancy_sector_ca_plot <- ggplot() +
  geom_line(data = occupancy_sector_ca, 
            aes(x = financial_year, y = percentage_occupancy, group = sector, colour = sector)) +
  facet_wrap('council_area') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 150)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Percentage Occupancy by Sector"),
       y = "Percentage Occupancy", 
       x = "Financial Year", 
       colour = "Sector",
       fill = "Sector")

occupancy_sector_ca_plot
ggsave(paste0("output/occupancy/", "Percentage Occupancy by Sector across Council Areas.png"),
       width = 12, height = 7)

# create occupancy chart broken down by main client group for scotland
occupancy_sector_scot <- percentage_occupancy_df %>% 
  filter(!sector == "All Sectors") %>% 
  filter(main_client_group == "All Adults") %>% 
  filter(council_area == "Scotland") %>% 
  group_by(financial_year, sector) %>% 
  summarise(percentage_occupancy = mean(percentage_occupancy))

occupancy_sector_scot_plot <- ggplot() +
  geom_line(data = occupancy_sector_scot, 
            aes(x = financial_year, y = percentage_occupancy, group = sector, colour = sector)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(
    angle = 45, 
    hjust = 1,
    colour = c("transparent", "black", "transparent", "transparent")
  )) +
  labs(title = paste0("Percentage Occupancy by Sector"),
       y = "Percentage Occupancy", 
       x = "Financial Year", 
       colour = "Sector",
       fill = "Sector")

occupancy_sector_scot_plot
ggsave(paste0("output/occupancy/", "Percentage Occupancy by Sector in Scotland.png"),
       width = 12, height = 7)
  