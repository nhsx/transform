################################################################################
## Title: PublicData
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

## Read in ons data

ons_population <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                     sheet = "Population",
                     range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "population") %>% 
  mutate(year = as.numeric(year))

ons_maternities <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                     sheet = "Maternities",
                     range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "maternities") %>% 
  mutate(year = as.numeric(year)) 

ons_deaths <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                       sheet = "Deaths",
                       range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "deaths") %>% 
  mutate(year = as.numeric(year)) 

ons_migration <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                     sheet = "Migration",
                     range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "netMigration") %>% 
  mutate(year = as.numeric(year)) 

ons_fertilityRate <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                     sheet = "FertilityRate",
                     range = "A1:AH183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "fertilityRate") %>% 
  mutate(year = as.numeric(year)) 

ons_mortalityRate <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                               sheet = "MortalityRate",
                               range = "A1:AH183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "mortalityRatePer100K") %>% 
  mutate(year = as.numeric(year)) 

ons_CoCFertility <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                              sheet = "DataNotes",
                              range = "B14:D20") %>% 
  filter(ComponentOfChange == "Fertility")

ons_CoCMigration <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                              sheet = "DataNotes",
                              range = "B14:D20") %>% 
  filter(ComponentOfChange == "Migration")

ons_CoCMortality <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                              sheet = "CoC_Mortality",
                              range = "A1:D547")

ons_GPRegAdjustmentRatio <- read_xlsx(path = here("publicdata/ONSPopDataExtracts.xlsx"),
                                      sheet = "GPRegAdjustment",
                                      range = "A1:C183")
