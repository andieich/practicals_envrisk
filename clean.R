library(tidyverse)
library(here)
dat <- readRDS(here("data/temp/clean_MooreaMPA.rds"))


dat <- dat %>% 
  dplyr::filter(habitat == "Outer slope") %>% 
  dplyr::select(year, date, marine_area, transect, substrate, benthic_group, proportion)



# get 2024 data
dat2024 <- read.csv(here("data/temp/20240301MPAMooreaData_substrate.csv")) %>% 
  select(-observations)


dat2024 <- dat2024 %>% 
  janitor::clean_names() %>% 
  filter(year == 2024, habitat == "Outer slope") 

dat2024$substrate %>% unique() %>% sort()

dat2024 <- dat2024 %>% 
  mutate(substrate = ifelse(substrate == "Dead coral (COTS)" , "Dead_coral", substrate))#no space

dat2024 <- dat2024 %>% 
  mutate(substrate = ifelse(substrate == "Napopora", "Porites", substrate)) %>% 
  separate(col = substrate, into = "substrate", sep = " ") %>% #separate and keep genus name
  mutate(substrate = tolower(substrate)) %>% #just to be sure
  mutate(substrate = str_to_title(substrate)) %>% 
  group_by(across(c(-proportion))) %>% 
  summarise(proportion = sum(proportion, na.rm = T)) %>% 
  ungroup()

dat2024$substrate %>% unique() %>% sort()

dat2024 <- dat2024 %>% 
  pivot_wider(names_from = substrate, values_from = proportion, values_fill = 0) %>% 
  pivot_longer(cols = 9:ncol(.),names_to = "substrate", values_to = "proportion")

corals <- c("Acanthastrea",
            "Acropora",
            "Astrea",
            "Astreopora",
            "Coscinaraea",
            "Cyphastrea",
            "Fungia",
            "Gardineroseris",
            "Goniastrea",
            "Herpolitha",
            "Leptastrea",
            "Leptoseris",
            "Lobophyllia",
            "Montipora",
            "Other_coral",
            "Pavona",
            "Pocillopora",
            "Porites",
            "Psammocora",
            "Synarea")

macro_algae <- c("Asparagopsis",
                 "Boodlea",
                 "Caulerpa",
                 "Dictyota",
                 "Halimeda",
                 "Macroalgae",
                 "Padina",
                 "Sargassum",
                 "Turbinaria", 
                 "Peyssonnelia")

substrate_ <- c("Mud",
                "Pavement",
                "Rubble",
                "Sand")

other <- c("Millepora",
           "Other",
           "Softcoral",
           "Other_anemone")

cyanos <- c("Cyanophyceae")

turf <- c("Stegastes_turf")

dead_coral <- "Dead_coral"

dat2024 <- dat2024 %>%
  mutate(benthic_group = case_when(substrate %in% corals ~ "coral",
                                   substrate %in% macro_algae ~ "macro_alga",
                                   substrate %in% substrate_ ~ "substrate",
                                   substrate %in% other ~ "other",
                                   substrate %in% turf ~ "turf",
                                   substrate %in% cyanos ~ "cyanos",
                                   substrate %in% dead_coral ~ "dead_coral"))




dat2024 %>% 
  filter(is.na(benthic_group))


dat2024 <- dat2024 %>% 
  select(names(dat))


dat2024 %>% 
  group_by(year, marine_area, transect) %>% 
  summarise(n = n())

dat2024 %>% 
  select(year, marine_area, transect) %>% 
  distinct() %>% 
  group_by(year, marine_area) %>% 
  summarise(n = n()) 



dat2024 %>% 
  distinct() %>% 
  group_by(year, marine_area, transect) %>% 
  summarise(prop = round(sum(proportion)),2) %>% 
  filter(prop != 1)


dat %>% 
  distinct() %>% 
  group_by(year, marine_area, transect) %>% 
  summarise(prop = round(sum(proportion)),2) %>% 
  filter(prop != 1)

# combine
dat2024 <- dat2024 %>% 
  mutate(date = as_date(date))


dat <- dat %>% 
  bind_rows(dat2024)


dat %>% 
  select(year, marine_area, transect) %>% 
  group_by(year, marine_area, transect) %>% 
  summarise(n = n()) %>% 
  pull(n) %>% 
  range()

dat$substrate %>% unique() %>% sort()#41


# wider, longer
dat <- dat %>% 
  select(-benthic_group) %>% 
  pivot_wider(names_from = substrate, values_from = proportion, values_fill = 0) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "substrate", values_to = "proportion") %>% 
  mutate(benthic_group = case_when(substrate %in% corals ~ "coral",
                                   substrate %in% macro_algae ~ "macro_alga",
                                   substrate %in% substrate_ ~ "substrate",
                                   substrate %in% other ~ "other",
                                   substrate %in% turf ~ "turf",
                                   substrate %in% cyanos ~ "cyanos",
                                   substrate %in% dead_coral ~ "dead_coral"))

dat %>% 
  distinct() %>% 
  group_by(year, marine_area, transect) %>% 
  summarise(prop = round(sum(proportion)),2) %>% 
  filter(prop != 1)

dat %>% 
  select(year, marine_area, transect) %>% 
  distinct() %>% 
  group_by(year, marine_area) %>% 
  summarise(n = n()) %>% 
  filter(n != 3)


dat %>% 
  group_by(year, marine_area, transect) %>% 
  summarise(n = n()) %>% 
  filter(n != 41)


# make nicer


dat <- dat %>% 
  rename("site" = "marine_area",
         "fine_group" = "substrate",
         "broad_group" = "benthic_group",
         "percent" = "proportion") %>% 
  select(date, year, site, transect, broad_group, fine_group, percent) %>% 
  mutate(percent = percent * 100)


coral_cover <- dat %>% 
  dplyr::filter(broad_group == "coral") %>% 
  group_by(year, date, site, transect) %>% 
  summarise(percent = sum(percent))




coralgenus_cover <- dat %>% 
  dplyr::filter(broad_group == "coral") %>% 
  group_by(year, date, site, transect, fine_group) %>% 
  summarise(percent = sum(percent)) %>% 
  rename("coral_genus" = "fine_group")



coralgenus_cover %>% 
  group_by(year, date, site, transect) %>% 
  summarise(n = n()) %>% 
  filter(n != 20)



#get top corals
top_corals <- coralgenus_cover %>% 
  group_by(coral_genus) %>% 
  summarise(percent = mean(percent)) %>% 
  arrange(desc(percent)) %>% 
  pull(coral_genus)


coralgenus_cover <- coralgenus_cover %>% 
  mutate(coral_genus = ifelse(coral_genus %in% top_corals[1:3], coral_genus, "other")) %>% 
  group_by(across(c(-percent))) %>% 
  summarise(percent = sum(percent, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = coral_genus, values_from = percent, values_fill = 0) %>% 
  pivot_longer(cols = 5:ncol(.),names_to = "coral_genus", values_to = "percent")



write.csv(coralgenus_cover, file = "data/coralgenus_cover.csv", row.names = F)

#write.csv(coral_cover, file = "data/coral_cover.csv", row.names = F)
