################################################################################
## Title: PublicData
## Last Update: 01/10/2020
## Version: 1.0
## Developer Contact: analytics-unit-internal@nhsx.nhs.uk
################################################################################

## Read in ons data

ons_population <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                     sheet = "Population",
                     range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "population") %>% 
  mutate(year = as.numeric(year))


ons_maternities <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                     sheet = "Maternities",
                     range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "maternities") %>% 
  mutate(year = as.numeric(year)) 

ons_deaths <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                       sheet = "Deaths",
                       range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "deaths") %>% 
  mutate(year = as.numeric(year)) 

ons_migration <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                     sheet = "Migration",
                     range = "A1:K183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "netMigration") %>% 
  mutate(year = as.numeric(year)) 

ons_fertilityRate <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                     sheet = "FertilityRate",
                     range = "A1:AH183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "fertilityRate") %>% 
  mutate(year = as.numeric(year)) 

ons_mortalityRate <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                               sheet = "MortalityRate",
                               range = "A1:AH183") %>%
  pivot_longer(c(-sex, -age), names_to = "year", values_to = "mortalityRatePer100K") %>% 
  mutate(year = as.numeric(year)) 

ons_CoCFertility <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                              sheet = "DataNotes",
                              range = "B14:D20") %>% 
  filter(ComponentOfChange == "Fertility")

ons_CoCMigration <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                              sheet = "DataNotes",
                              range = "B14:D20") %>% 
  filter(ComponentOfChange == "Migration")

ons_CoCMortality <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                              sheet = "CoC_Mortality",
                              range = "A1:D547")

CoC_mortality_function = data.frame(a = 0.998385639632051,
                                    b = -0.0524494034902026,
                                    c = -0.0000202560567696707,
                                    d = 0.0000151832259045123)

ons_GPRegAdjustmentRatio <- read_xlsx(path = here("ONSPopDataExtracts.xlsx"),
                                      sheet = "GPRegAdjustment",
                                      range = "A1:C183")
