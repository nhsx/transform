library(tidyverse)
library(readxl)
library(here)
library(splines)
library(ggplot2)
library(dplyr)
library(gridExtra)
options(scipen = 9999999)

## --------------------------- ONS Populaion ---------------------------

## Read in Population
ons_population = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                           sheet = "Population",
                           range = "A2:J254") %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "Population") %>% 
  mutate(Year = as.numeric(Year))

## Read in Births
ons_births = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                       sheet = "Births",
                       range = "A2:H34") %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "Births") %>% 
  mutate(Year = as.numeric(Year))

## Read in Deaths
ons_deaths = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                       sheet = "Deaths",
                       range = "A2:I256") %>%
  replace(is.na(.), 0) %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "Deaths") %>% 
  mutate(Year = as.numeric(Year))

## Read in Migration
ons_migration = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                          sheet = "Migration",
                          range = "A2:AP254") %>%
  replace(is.na(.), 0) %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "NetMigration") %>% 
  mutate(Year = as.numeric(Year))

## Read in Mortality
mortality_rate = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                           sheet = "Mortaltyrate",
                           range = "A2:AJ256") %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "mortalityRatePer100K") %>% 
  mutate(Year = as.numeric(Year))

## Read in Fertility
fertility_rate = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                           sheet = "Fertilityrate",
                           range = "A2:AJ34") %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "fertilityRate") %>% 
  mutate(Year = as.numeric(Year))

gpRegPopulation = read_xlsx(path = here("ONSBasePopulation.xlsx"),
                            sheet = "historic populations (GPreg)",
                            range = "A1:K253") %>%
  pivot_longer(c(-Sex, -Age), names_to = "Year", values_to = "GPHistoric") %>%
  mutate(Year = as.integer(Year))

# INPUTS
CoC_fertility = read.csv(here("ComponentOfChangeRates_Fertility&Migration.csv")) %>% filter(ComponentOfChange == "Fertility")
CoC_migration = read.csv(here("ComponentOfChangeRates_Fertility&Migration.csv")) %>% filter(ComponentOfChange == "Migration")
CoC_mortality = read.csv(here("ComponentOfChangeRates_Mortality.csv"))

# load in raw SUS data - TEMPORARY MEASURE MOVE this to direCt ncdr feed
activity_data_raw = read_xlsx(path = here("SUSActivityData.xlsx"),
                              sheet = "SUSActivityRaw",
                              range = "A1:N55637") %>%
  rename(AGE_CORR_ORIG = "agecorrected",
         SEX_CORR_ORIG = "sex_corrected")

CoC_mortality_function = data.frame(a = 0.998385639632051,
                                    b = -0.0524494034902026,
                                    c = -0.0000202560567696707,
                                    d = 0.0000151832259045123)


## USER OPTIONS ##
lastForecastYear = 2049
maleWeight = 105
femaleWeight = 100
fertScalingStartYear = 2016
fertScalingEndYear = 2040
mortScalingStartYear = 2016
migrationPopScale = data.frame(Type = c("Principal", "Low", "High"), Pop = c(144000, 81500, 206500)) %>% mutate(rate = Pop / Pop[1])
migrationScalingStartYear = 2016
migrationScalingEndYear = 2022

fertility_variant = "Low"
mortality_variant = "Low"
migration_variant = "Low"
ForecastYearStart = 2018
ForecastYearEnd = 2030

## Population variant model
options = c("Principal", "Low", "High")

CoC = data.frame()

for(ff in 1:length(options)){
  
  fertilityVariant = options[ff]
  
  for(mm in 1:length(options)){
    
    mortalityVariant = options[mm]
    
    for(ii in 1:length(options)){
      
      migrationVariant = options[ii]
      
      ## selected variants
      # fertility variant
      fertRate_Var = fertility_rate %>%
        mutate(VarRate = CoC_fertility %>% filter(Variant == fertilityVariant) %>% select(Rate) %>% pull(),
               PrincipalVarRate = CoC_fertility %>% filter(Variant == "Principal") %>% select(Rate) %>% pull(),
               scaling = ifelse((Year - fertScalingStartYear) /(fertScalingEndYear - fertScalingStartYear) > 1, 
                                1, 
                                (Year - fertScalingStartYear) /(fertScalingEndYear - fertScalingStartYear)),
               multiplier = ((1 - scaling) * PrincipalVarRate + scaling * VarRate) / PrincipalVarRate,
               fertilityRate = fertilityRate * multiplier) %>%
        select(Sex, Age, Year, fertilityRate)
      
      # mortality variant
      mortRate_Var = mortality_rate %>%
        left_join(CoC_mortality %>% filter(Type == "Principal") %>% select(Sex, Age, Rate), by = c("Age", "Sex")) %>%
        rename("Principal" = "Rate") %>%
        left_join(CoC_mortality %>% filter(Type == mortalityVariant) %>% select(Sex, Age, Rate), by = c("Age", "Sex")) %>%
        mutate(x = Year - mortScalingStartYear,
               scaling = ifelse(CoC_mortality_function$a + x * CoC_mortality_function$b + x^2 * CoC_mortality_function$c + x^3 * CoC_mortality_function$d < 0,
                                0,
                                CoC_mortality_function$a + x * CoC_mortality_function$b + x^2 * CoC_mortality_function$c + x^3 * CoC_mortality_function$d),
               mortalityRatePer100K_new = ifelse(Year <= mortScalingStartYear + 1, mortalityRatePer100K, NA))
      
      for(rr in 1:nrow(mortRate_Var)){
        
        if(mortRate_Var$Year[rr] > mortScalingStartYear + 1){
          
          mortRate_Var$mortalityRatePer100K_new[rr] = ((mortRate_Var$Rate[rr] - mortRate_Var$Principal[rr]) *
                                                         (1 - mortRate_Var$scaling[rr]) +
                                                         (mortRate_Var$mortalityRatePer100K[rr] / mortRate_Var$mortalityRatePer100K[rr - 1])) *
            mortRate_Var$mortalityRatePer100K_new[rr - 1]
          
        }
        
      }
      
      mortRate_Var = mortRate_Var %>%
        mutate(mortalityRatePer100K = mortalityRatePer100K_new) %>%
        select(Sex, Age, Year, mortalityRatePer100K)
      
      # migration variant
      migration_Var = ons_migration %>%
        mutate(scaling = ((Year - migrationScalingStartYear) / (migrationScalingEndYear - migrationScalingStartYear)),
               scaling = ifelse(scaling >= 0 & scaling <= 1,
                                scaling,
                                ifelse(scaling < 0, 0, 1)),
               rate = CoC_migration %>% filter(Variant == migrationVariant) %>% select(Rate) %>% pull(),
               multiplier = rate * (1 - scaling) + migrationPopScale$rate[migrationPopScale$Type == migrationVariant] * scaling,
               NetMigration = ifelse(Year > migrationScalingStartYear, NetMigration * multiplier, NetMigration)) %>%
        select(Sex, Age, Year, NetMigration)
      
      
      
      sex = 1:2
      ages = seq.int(from = min(ons_births$Age, ons_deaths$Age, ons_migration$Age, ons_population$Age),
                     to = max(ons_births$Age, ons_deaths$Age, ons_migration$Age, ons_population$Age), 
                     by = 1)
      years = seq.int(from = min(ons_births$Year, ons_deaths$Year, ons_migration$Year, ons_population$Year),
                      to = max(ons_births$Year, ons_deaths$Year, ons_migration$Year, ons_population$Year), 
                      by = 1)
      
      
      ons_CoC = data.frame(Sex = rep(sex,
                                     each = length(ages) * length(years)),
                           Age = rep(ages,
                                     each = length(years),
                                     times = length(sex)),
                           Year = years) %>%
        left_join(ons_population, by = c("Sex", "Age", "Year")) %>%
        left_join(ons_births, by = c("Sex", "Age", "Year")) %>%
        left_join(ons_deaths, by = c("Sex", "Age", "Year")) %>%
        left_join(ons_migration, by = c("Sex", "Age", "Year"))
      
      lastYearBirths = max(as.numeric(ons_births$Year))
      
      yearsToCalc = (lastYearBirths + 1):lastForecastYear
      
      for(yy in 1:length(yearsToCalc)){
        
        estimatedBirths = ons_CoC %>%
          filter(Year %in% c(yearsToCalc[yy], yearsToCalc[yy] + 1)) %>%
          left_join(fertRate_Var %>% filter(Year == yearsToCalc[yy]), by = c("Sex", "Age", "Year")) %>%
          group_by(Age, Sex) %>%
          mutate(estPop = sum(Population)) %>%
          filter(Year == yearsToCalc[yy]) %>%
          mutate(Births_new = round(estPop * fertilityRate / 1000 / 2),
                 Deaths_new = NA) %>%
          select(-fertilityRate, -estPop, -Births, -Deaths) %>%
          filter(!is.na(Births_new))
        
        if(yy > 1){
          
          init = data.frame(Sex = c(1, 2),
                            Age = -1,
                            Year = yearsToCalc[yy],
                            weight = c(maleWeight, femaleWeight)) %>%
            left_join(mortRate_Var, by = c("Sex", "Age", "Year")) %>%
            mutate(Births_new = sum(estimatedBirths$Births_new) * (weight/ sum(weight)),
                   Deaths_new = round(Births_new * mortalityRatePer100K / 100000)) %>%
            select(-weight, -mortalityRatePer100K)
          
          initPop = init %>%
            mutate(Age = 0) %>%
            left_join(migration_Var, by = c("Sex", "Age", "Year")) %>%
            mutate(Year = Year + 1,
                   Population_new = Births_new + NetMigration / 2 - Deaths_new) %>%
            select(Sex, Age, Year, Population_new)
          
          ons_CoC = ons_CoC %>%
            left_join(init, by = c("Sex", "Age", "Year")) %>%
            left_join(initPop, by = c("Sex", "Age", "Year")) %>%
            mutate(Births = ifelse(!is.na(Births_new), Births_new, Births),
                   Deaths = ifelse(!is.na(Deaths_new), Deaths_new, Deaths),
                   Population = ifelse(!is.na(Population_new), Population_new, Population)) %>%
            select(-ends_with("_new")) %>%
            arrange(Sex, Age, Year)
          
        }
        
        estimatedDeaths = ons_CoC %>%
          filter(Year == (yearsToCalc[yy] + 1)) %>%
          left_join(mortRate_Var %>% filter(Year == yearsToCalc[yy]+1), by = c("Sex", "Age", "Year")) %>%
          mutate(Births_new = NA,
                 Deaths_new = round((Population + 0.5 * NetMigration) * mortalityRatePer100K / 100000)) %>%
          select(-mortalityRatePer100K, -Births, -Deaths)
        
        ons_CoC = ons_CoC %>%
          left_join(select(rbind.data.frame(estimatedBirths, estimatedDeaths), Sex, Age, Year, Births_new, Deaths_new),
                    by = c("Sex", "Age", "Year")) %>%
          mutate(Births = ifelse(!is.na(Births_new), Births_new, Births),
                 Deaths = ifelse(!is.na(Deaths_new), Deaths_new, Deaths)) %>%
          select(-ends_with("_new"))
        
        estimatedPopulation_OnePlus = ons_CoC %>%
          filter(Year == (yearsToCalc[yy] + 1)) %>%
          group_by(Sex) %>%
          mutate(Year = (yearsToCalc[yy] + 2),
                 Population_new = lag(Population, 1) + (lag(NetMigration, 1) + NetMigration) / 2 - lag(Deaths, 1)) %>%
          ungroup()
        
        ons_CoC = ons_CoC %>%
          left_join(select(estimatedPopulation_OnePlus, Sex, Age, Year, Population_new),
                    by = c("Sex", "Age", "Year")) %>%
          mutate(Population = ifelse(!is.na(Population_new), Population_new, Population)) %>%
          select(-ends_with("_new"))
        
      }
      
      ons_CoC = ons_CoC %>%
        filter(Age >= 0) %>%
        mutate(FertilityVariant = fertilityVariant,
               MortalityVariant = mortalityVariant,
               MigrationVariant = migrationVariant)
      
      
      CoC = rbind.data.frame(CoC, ons_CoC, stringsAsFactors = F)
      
    }
  }
}

## --------------------------- Create Principal Table ---------------------------

ageLabels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
              "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
              "80-84", "85-89", "90-94", "95-99", "100+")


# Summary Tables
summary.table.allPrincipal = cbind.data.frame(CoC %>%
                                                filter(Year == ForecastYearStart, 
                                                       FertilityVariant == "Principal",
                                                       MortalityVariant == "Principal",
                                                       MigrationVariant == "Principal") %>%
                                                select(Sex, Age, Year, Population) %>%
                                                mutate(AgeGroup = cut(Age, 
                                                                      breaks = c(seq.int(from = 0, to = 100, by = 5), 125 + 1), 
                                                                      right = F,
                                                                      labels = ageLabels)) %>%
                                                select(-Age, Year) %>%
                                                group_by(Sex, AgeGroup) %>%
                                                summarise(Population = sum(Population)) %>%
                                                ungroup() %>%
                                                mutate(Sex = ifelse(Sex == 1, "Male_PeriodStart", "Female_PeriodStart")) %>%
                                                pivot_wider(names_from = Sex, values_from = Population),
                                              CoC %>%
                                                filter(Year == ForecastYearEnd, 
                                                       FertilityVariant == "Principal",
                                                       MortalityVariant == "Principal",
                                                       MigrationVariant == "Principal") %>%
                                                select(Sex, Age, Year, Population) %>%
                                                mutate(AgeGroup = cut(Age, 
                                                                      breaks = c(seq.int(from = 0, to = 100, by = 5), 125 + 1), 
                                                                      right = F,
                                                                      labels = ageLabels)) %>%
                                                select(-Age, Year) %>%
                                                group_by(Sex, AgeGroup) %>%
                                                summarise(Population = sum(Population)) %>%
                                                ungroup() %>%
                                                mutate(Sex = ifelse(Sex == 1, "Male_PeriodEnd", "Female_PeriodEnd")) %>%
                                                pivot_wider(names_from = Sex, values_from = Population) %>%
                                                select(-AgeGroup),
                                              stringsAsFactors = F) %>%
  mutate(Growth_Males = (Male_PeriodEnd/Male_PeriodStart)^(1/(ForecastYearEnd-ForecastYearStart))-1,
         Growth_Females = (Female_PeriodEnd/Female_PeriodStart)^(1/(ForecastYearEnd-ForecastYearStart))-1)

## --------------------------- Create Variant Table ---------------------------

summary.table.chosenVariant = cbind.data.frame(CoC %>%
                                                 filter(Year == ForecastYearStart, 
                                                        FertilityVariant == fertility_variant,
                                                        MortalityVariant == mortality_variant,
                                                        MigrationVariant == migration_variant) %>%
                                                 select(Sex, Age, Year, Population) %>%
                                                 mutate(AgeGroup = cut(Age, 
                                                                       breaks = c(seq.int(from = 0, to = 100, by = 5), 125 + 1), 
                                                                       right = F,
                                                                       labels = ageLabels)) %>%
                                                 select(-Age, Year) %>%
                                                 group_by(Sex, AgeGroup) %>%
                                                 summarise(Population = sum(Population)) %>%
                                                 ungroup() %>%
                                                 mutate(Sex = ifelse(Sex == 1, "Male_PeriodStart", "Female_PeriodStart")) %>%
                                                 pivot_wider(names_from = Sex, values_from = Population),
                                               CoC %>%
                                                 filter(Year == ForecastYearEnd, 
                                                        FertilityVariant == fertility_variant,
                                                        MortalityVariant == mortality_variant,
                                                        MigrationVariant == migration_variant) %>%
                                                 select(Sex, Age, Year, Population) %>%
                                                 mutate(AgeGroup = cut(Age, 
                                                                       breaks = c(seq.int(from = 0, to = 100, by = 5), 125 + 1), 
                                                                       right = F,
                                                                       labels = ageLabels)) %>%
                                                 select(-Age, Year) %>%
                                                 group_by(Sex, AgeGroup) %>%
                                                 summarise(Population = sum(Population)) %>%
                                                 ungroup() %>%
                                                 mutate(Sex = ifelse(Sex == 1, "Male_PeriodEnd", "Female_PeriodEnd")) %>%
                                                 pivot_wider(names_from = Sex, values_from = Population) %>%
                                                 select(-AgeGroup),
                                               stringsAsFactors = F) %>%
  mutate(Growth_Males = (Male_PeriodEnd/Male_PeriodStart)^(1/(ForecastYearEnd-ForecastYearStart))-1,
         Growth_Females = (Female_PeriodEnd/Female_PeriodStart)^(1/(ForecastYearEnd-ForecastYearStart))-1)

## --------------------------- Create Principal Age Table ---------------------------

# Summary Charts
chart.sexComp.allPrincipal = CoC %>%
  filter(Year == ForecastYearEnd, 
         FertilityVariant == "Principal",
         MortalityVariant == "Principal",
         MigrationVariant == "Principal") %>%
  select(Sex, Age, Year, Population) %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(seq.int(from = 0, to = 100, by = 5), 125 + 1), 
                        right = F,
                        labels = ageLabels)) %>%
  select(-Age, Year) %>%
  group_by(Sex, AgeGroup) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(Sex = as.factor(ifelse(Sex == 1, "Male", "Female")),
         Population = ifelse(Sex == "Male", -Population, Population)) %>%
  ggplot(aes(x = AgeGroup, y = Population, group = Sex, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = rev(ageLabels)) +
  scale_y_continuous(breaks = seq(-3000000, 3000000, 1000000), 
                     labels = abs(seq(-3000000, 3000000, 1000000))) +
  #labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Age Group", y = "Population",
       title = paste0("Population Age Profile - " , ForecastYearEnd),
       subtitle = paste0("Fertility: ", "Principal", "; Mortality: ", "Principal", "; Migration: ", "Principal")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Male", "Female"),
                    labels=c("Male", "Female")) 


## --------------------------- Create Variant Age Table ---------------------------

chart.sexComp.chosenVariant = CoC %>%
  filter(Year == ForecastYearEnd, 
         FertilityVariant == fertility_variant,
         MortalityVariant == mortality_variant,
         MigrationVariant == migration_variant) %>%
  select(Sex, Age, Year, Population) %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(seq.int(from = 0, to = 100, by = 5), 125 + 1), 
                        right = F,
                        labels = ageLabels)) %>%
  select(-Age, Year) %>%
  group_by(Sex, AgeGroup) %>%
  summarise(Population = sum(Population)) %>%
  ungroup() %>%
  mutate(Sex = as.factor(ifelse(Sex == 1, "Male", "Female")),
         Population = ifelse(Sex == "Male", -Population, Population)) %>%
  
  ggplot(aes(x = AgeGroup, y = Population, group = Sex, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = rev(ageLabels)) +
  scale_y_continuous(breaks = seq(-3000000, 3000000, 1000000), 
                     labels = abs(seq(-3000000, 3000000, 1000000))) +
  #labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Age Group", y = "Population", 
       title = paste0("Population Age Profile - " , ForecastYearEnd),
       subtitle = paste0("Fertility: ", fertility_variant, "; Mortality: ", mortality_variant, "; Migration: ", migration_variant)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90")) +
  scale_fill_manual(values=c("red", "blue"),
                    name="",
                    breaks=c("Male", "Female"),
                    labels=c("Male", "Female")) 

grid.arrange(chart.sexComp.allPrincipal,chart.sexComp.chosenVariant, ncol=2)

## --------------------------- Create Principal Growth table ---------------------------

chart.growthComp.allPrincipal = summary.table.allPrincipal %>%
  select(AgeGroup, starts_with("Growth")) %>%
  pivot_longer(-AgeGroup, names_to = "Sex", values_to = "Growth") %>%
  mutate(Sex = ifelse(Sex == "Growth_Males", "Male", "Female")) %>%
  ggplot(aes(x = AgeGroup, y = Growth, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  labs(x = "Age Group", y = "Growth", 
       title = paste0("Average Annual Growth - " , ForecastYearStart, " to ", ForecastYearEnd),
       subtitle = paste0("Fertility: ", "Principal", "; Mortality: ", "Principal", "; Migration: ", "Principal"))


## --------------------------- Create Variant Growth Table ---------------------------

chart.growthComp.chosenVariant = summary.table.chosenVariant %>%
  select(AgeGroup, starts_with("Growth")) %>%
  pivot_longer(-AgeGroup, names_to = "Sex", values_to = "Growth") %>%
  mutate(Sex = ifelse(Sex == "Growth_Males", "Male", "Female")) %>%
  ggplot(aes(x = AgeGroup, y = Growth, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  labs(x = "Age Group", y = "Growth", 
       title = paste0("Average Annual Growth - " , ForecastYearStart, " to ", ForecastYearEnd),
       subtitle = paste0("Fertility: ", fertility_variant, "; Mortality: ", mortality_variant, "; Migration: ", migration_variant))


grid.arrange(chart.growthComp.allPrincipal, chart.growthComp.chosenVariant, ncol=2)

## --------------------------- Create Population Change Chart ---------------------------

chart.population.time.series = cbind.data.frame(CoC %>%
                                                  filter(FertilityVariant == "Principal",
                                                         MortalityVariant == "Principal",
                                                         MigrationVariant == "Principal") %>%
                                                  group_by(Year) %>%
                                                  summarise(Population = sum(Population)) %>%
                                                  mutate(Population_Estimates = ifelse(Year <= ForecastYearStart - 1, Population, NA),
                                                         Principal_Projection = ifelse(Year >= ForecastYearStart - 1, Population, NA)),
                                                CoC %>%
                                                  filter(FertilityVariant == fertility_variant,
                                                         MortalityVariant == mortality_variant,
                                                         MigrationVariant == migration_variant) %>%
                                                  group_by(Year) %>%
                                                  summarise(Population = sum(Population)) %>%
                                                  mutate(Variant_Projection = ifelse(Year >= ForecastYearStart - 1, Population, NA)) %>%
                                                  select(Variant_Projection)) %>%
  select(-Population) %>%
  pivot_longer(-Year, names_to = "Data", values_to = "Population") %>%
  ggplot(aes(x = Year, y = Population, colour = Data)) +
  geom_line()

print(chart.population.time.series)

## --------------------------- Tidy and reformat ---------------------------

# Correct for sex and age
activity_data = activity_data_raw %>%
  filter(AGE_CORR_ORIG %in% c(0:110,"Missing")) %>%
  mutate(sex_corrected = ifelse(SEX_CORR_ORIG %in% c(1, 2), as.character(SEX_CORR_ORIG), "Missing"),
         age_corrected = ifelse(treatmentarea == "AE" & AGE_CORR_ORIG %in% c(0:114, 700), 
                                as.character(AGE_CORR_ORIG), 
                                ifelse(treatmentarea != "AE" & AGE_CORR_ORIG %in% c(0:110, 700),
                                       as.character(AGE_CORR_ORIG),
                                       "Missing")),
         age_corrected = ifelse(age_corrected == "700", "0", age_corrected)) %>%
  select(treatmentarea, sex_corrected, age_corrected, Setting, yr, activities, cost) %>%
  pivot_longer(-c(treatmentarea, sex_corrected, age_corrected, Setting, yr), names_to = "AC", values_to = "value") %>%
  # create grouped totals
  group_by(treatmentarea, sex_corrected, age_corrected, Setting, yr, AC) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # create attribute known age unknown sex column
  group_by(treatmentarea, age_corrected, Setting, yr, AC) %>%
  mutate(attKnownAgeUnknownSex_TotalExclMissing = sum(value[sex_corrected != "Missing"]),
         attKnownAgeUnknownSex_TotalMissing = sum(value[sex_corrected == "Missing"])) %>%
  ungroup() %>%
  mutate(attKnownAgeUnknownSex = value / attKnownAgeUnknownSex_TotalExclMissing * attKnownAgeUnknownSex_TotalMissing) %>%
  # create attribute known sex unknown age column
  group_by(treatmentarea, sex_corrected, Setting, yr, AC) %>%
  mutate(attKnownSexUnknownAge_TotalExclMissing = sum(value[age_corrected != "Missing"]),
         attKnownSexUnknownAge_TotalMissing = sum(value[age_corrected == "Missing"])) %>% 
  ungroup() %>%
  mutate(attKnownSexUnknownAge = value / attKnownSexUnknownAge_TotalExclMissing * attKnownSexUnknownAge_TotalMissing) %>%
  # create attribute known sex unknown age column
  group_by(treatmentarea, Setting, yr, AC) %>%
  mutate(attUnknownAgeUnknownSex_TotalExclMissing = sum(value[sex_corrected != "Missing" & age_corrected != "Missing"]),
         attUnknownAgeUnknownSex_TotalMissing = sum(value[sex_corrected == "Missing" & age_corrected == "Missing"])) %>%
  ungroup() %>%
  mutate(attUnknownAgeUnknownSex = value / attUnknownAgeUnknownSex_TotalExclMissing * attUnknownAgeUnknownSex_TotalMissing,
         Counts = value + attKnownAgeUnknownSex + attKnownSexUnknownAge + attUnknownAgeUnknownSex)

## Create reduced data frame without Missings.  Contains Totals with imputation from SUS data 
tblCount <- activity_data %>% 
  filter(sex_corrected != "Missing" & age_corrected != "Missing") %>% 
  mutate(sex_corrected = as.numeric(sex_corrected),
         age_corrected = as.numeric(age_corrected)) %>% 
  select(treatmentarea, sex_corrected, age_corrected, Setting, yr, AC, Counts)
names(tblCount) <- c("Treatment", "Sex", "Age", "Setting", "Year", "ValueType", "value")

## Create data frame to run calculation on.  For this need additional fake 5 years of Age data 
## either side - set to Age 100 and Age 0. REDO POOR CODING
dfSpline <- tblCount %>% filter(Age <= 100) %>%  left_join(gpRegPopulation,by = c("Sex","Age","Year"))
upper101 <- dfSpline %>% filter(Age == 100) %>% mutate(Age = 101)
upper102 <- dfSpline %>% filter(Age == 100) %>% mutate(Age = 102)
upper103 <- dfSpline %>% filter(Age == 100) %>% mutate(Age = 103)
upper104 <- dfSpline %>% filter(Age == 100) %>% mutate(Age = 104)
upper105 <- dfSpline %>% filter(Age == 100) %>% mutate(Age = 105)
lowerm1 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -1)
lowerm2 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -2)
lowerm3 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -3)
lowerm4 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -4)
lowerm5 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -5)
dfSpline <- rbind(dfSpline,upper101,upper102,upper103,upper104,upper105,lowerm1,lowerm2,lowerm3,lowerm4,lowerm5)

## Filter out Other and Maternity as Age ranges are not the same as other settings
## Can redo these separately if required.
## Calculate Rates using the SUS total over GP Historic Registrations
dfSpline <- dfSpline %>% 
  filter(Treatment != "Other", Treatment != "Maternity") %>%
  mutate(Rate = value/GPHistoric) %>%  
  arrange(Sex, Setting, Treatment, ValueType, Age, Year) %>%
  mutate(Age = Age + 0.5) %>%
  as.data.frame() 

## --------------------------- Main Calculation ---------------------------

## Run a linear fit for the logarithm of the rate dependant on an Age B-spline, the Year 
## and the interaction of these.  This then takes into account the interaction as age 
## cohorts get older through time
fitAll <- dfSpline %>%
  group_by(Sex,Setting,Treatment,ValueType) %>%
  do(fit = lm(log(Rate) ~ bs(Age, knots=c(0.5, 4.5, 14.5, 34.5, 54.5, 64.5, 84.5, 100.5)) + 
                Year + 
                Year*bs(Age, knots=c(0.5, 4.5, 14.5, 34.5, 54.5, 64.5, 84.5, 100.5)), weights = GPHistoric, data = .))

## All base Age splines the same so take any  
baseAgeSpline <- bs(dfSpline %>% 
                      filter(Sex==1,Setting=="APC",Treatment=="NonElective",ValueType=="activities",Year==2016) %>% 
                      select(Sex,Setting,Treatment,ValueType,Age) %>% pull(), knots=c(0.5, 4.5, 14.5, 34.5, 54.5, 64.5, 84.5, 100.5))

## Annualy growth can be extracted from each model by taking the first deriavative in "Year"
dfGrowth <- data.frame(matrix(nrow = 111, ncol = 28))
for (ii in 1:dim(fitAll)[1]){
  dfGrowth[,ii] <- fitAll$fit[[ii]]$coefficients[13] +
    fitAll$fit[[ii]]$coefficients[14]*baseAgeSpline[,1] + 
    fitAll$fit[[ii]]$coefficients[15]*baseAgeSpline[,2] +
    fitAll$fit[[ii]]$coefficients[16]*baseAgeSpline[,3] +
    fitAll$fit[[ii]]$coefficients[17]*baseAgeSpline[,4] + 
    fitAll$fit[[ii]]$coefficients[18]*baseAgeSpline[,5] + 
    fitAll$fit[[ii]]$coefficients[19]*baseAgeSpline[,6] + 
    fitAll$fit[[ii]]$coefficients[20]*baseAgeSpline[,7] + 
    fitAll$fit[[ii]]$coefficients[21]*baseAgeSpline[,8] + 
    fitAll$fit[[ii]]$coefficients[22]*baseAgeSpline[,9] +
    fitAll$fit[[ii]]$coefficients[23]*baseAgeSpline[,10] + 
    fitAll$fit[[ii]]$coefficients[24]*baseAgeSpline[,11] 
}




## Bodge to add categories to growth calculation
dfGrowth <- dfGrowth %>% 
  gather()
dfGrowth$Treatment <- rep(c(rep("AE",2*111),rep("Elective",2*111),rep("MHLD",2*111),rep("NonElective",2*111),rep("First",2*111),rep("FollowUp",2*111),rep("MHLD",2*111)),2)
dfGrowth$Sex <- c(rep(1,14*111),rep(2,14*111))
dfGrowth$Age <- rep(seq(1:111)-6,28)
dfGrowth$Setting <- rep(c(rep("AE",2*111),rep("APC",6*111),rep("OPA",6*111)),2)
dfGrowth$ValueType <- rep(c(rep("activities",111),rep("cost",111)),14)

## Predict next year by augmenting the cateogrical varibales with Year set to "2019".
# predAll <- dfSpline %>% 
#   filter(Year == 2016) %>% 
#   select(Sex, Setting, Treatment, ValueType, Age, Year, Rate) %>% ## knock out rate
#   mutate(Year = 2019) %>% 
#   group_by(Sex,Setting,Treatment,ValueType) %>% 
#   nest() %>% 
#   full_join(fitAll) %>%
#   group_by(Sex,Setting,Treatment,ValueType) %>% 
#   do(broom::augment(.$fit[[1]], newdata = .$data[[1]]))

## Create summary output data frame
dfOutput <- ons_population %>% 
  filter(Age >= 0 & Age <= 100) %>%
  spread(key = Year, value = Population) %>% 
  right_join(dfSpline %>% 
               mutate(Age = Age - 0.5) %>%
               filter(Age >= 0 & Age <= 100) %>%
               select(Treatment, Sex, Age, Setting, Year, ValueType, value) %>%
               spread(key = Year, value = value), 
                by = c("Sex","Age")) %>%
  left_join(dfSpline %>% 
              mutate(Age = Age - 0.5) %>% 
              filter(Age >= 0 & Age <= 100) %>% 
              select(Treatment, Sex, Age, Setting, Year, ValueType, Rate) %>% 
              spread(key = Year, value = Rate), by = c("Treatment","Sex","Age","Setting","ValueType")) %>%
  left_join(dfGrowth %>% 
              select(-key), by = c("Treatment","Sex","Age","Setting","ValueType")) %>%
  mutate("2018" = `2017.x`*`2017`*(1+value)^(2018-2017),
         "2019" = `2017.x`*`2017`*(1+value)^(2019-2017),
         "2020" = `2017.x`*`2017`*(1+value)^(2020-2017),
         "2021" = `2017.x`*`2017`*(1+value)^(2021-2017),
         "2022" = `2017.x`*`2017`*(1+value)^(2022-2017),
         "2023" = `2017.x`*`2017`*(1+value)^(2023-2017),
         "2024" = `2017.x`*`2017`*(1+value)^(2024-2017),
         "2025" = `2017.x`*`2017`*(1+value)^(2025-2017),
         "2026" = `2017.x`*`2017`*(1+value)^(2026-2017),
         "2027" = `2017.x`*`2017`*(1+value)^(2027-2017),
         "2028" = `2017.x`*`2017`*(1+value)^(2028-2017)
  )

#   dfOutputAllAge <- dfOutput %>% select(Treatment,Year,ValueType,value.x) %>% group_by(Treatment,Year,ValueType) %>% summarise(Value = sum(value.x))
# dfOutputAllAge2 <- dfOutput %>% select(Treatment,predYear,ValueType,predValue) %>% mutate(Year = predYear - 1) %>% group_by(Treatment,Year,ValueType) %>% 
#   summarise(pred = sum(predValue)) %>% left_join(dfOutputAllAge, by = c("Treatment","ValueType","Year")) %>% 
#   mutate(growth = (pred-Value)/Value)                                                                                                                                      

dfAllAge <- dfOutput %>% ##BROKEN
  select("Setting","Treatment","Sex","ValueType","2018","2019","2020","2021","2022","2023","2024","2025","2026","2027","2028") %>%
  group_by("Setting","Treatment","Sex","ValueType") %>%
  summarise_all(list(sum = mean),na.rm = TRUE) %>%
  mutate(growth = (`2028_sum`-`2018_sum`)/(10*`2018_sum`))

# dftimeline <- dfOutput %>% select(Treatment,Sex,Age,Setting,Year,ValueType,value.x) %>% 
#   left_join(dfGrowth, by = c("Setting","Treatment","Sex","Age","ValueType")) %>% 
#   spread(key = Year, value = value.x) %>%
#   left_join(ons_population %>% spread(key = Year, value = Population), by = c("Sex","Age")) %>% 
#   mutate("2019" = Population*value.x*(1+value.y)^(2019-2016),
#          "2020" = Population*value.x*(1+value.y)^(2020-2016),
#          "2021" = Population*value.x*(1+value.y)^(2021-2016),
#          "2022" = Population*value.x*(1+value.y)^(2022-2016),
#          "2023" = Population*value.x*(1+value.y)^(2023-2016),
#          "2024" = Population*value.x*(1+value.y)^(2024-2016),
#          "2025" = Population*value.x*(1+value.y)^(2025-2016),
#          "2026" = Population*value.x*(1+value.y)^(2026-2016),
#          "2027" = Population*value.x*(1+value.y)^(2027-2016),
#          "2028" = Population*value.x*(1+value.y)^(2028-2016)
#   )

ggplot(dfttt %>% filter(ValueType == "cost" & Setting == "APC" & Treatment == "NonElective" & Sex == 1), aes(x=Rate, y = Age, z = value.x)) + stat_density2d(aes(fill = ..level..), geom = "polygon")

## --------------------------- plots ---------------------------

## growth plots of the activities
dfGrowth %>% filter(ValueType == "activities", Age >=0, Age <= 100) %>%
  ggplot(aes(Age, value)) + 
  geom_line(size = 1, color = "red") +
  facet_wrap(~Sex+Treatment+Setting)

## growth plots of the costs
dfGrowth %>% filter(ValueType == "cost", Age >=0, Age <= 100) %>%
  ggplot(aes(Age, value)) + 
  geom_line(size = 1, color = "red") +
  facet_wrap(~Sex+Treatment+Setting)

# ## rate plots of the activities
# predAll %>% filter(ValueType == "activities", Age >=0, Age <= 100) %>%
#   ggplot(aes(Age, Rate)) + 
#   geom_point(shape = 1) + 
#   geom_line(aes(Age, exp(.fitted)), size = 1, color = "red") +
#   facet_wrap(~Sex+Treatment+Setting)
# 
# ## rate plots of the costs
# predAll %>% filter(ValueType == "cost", Age >=0, Age <= 100) %>%
#   ggplot(aes(Age, Rate)) + 
#   geom_point(shape = 1) + 
#   geom_line(aes(Age, exp(.fitted)), size = 1, color = "red") +
#   facet_wrap(~Sex+Treatment+Setting)