################################################################################
## Title: Tables
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

ageLabels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
              "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
              "80-84", "85-89", "90+")

## --------------------------- Create Principal Table ---------------------------

summary.table.allPrincipal = cbind.data.frame(CoC %>%
                                                filter(year == forecastYearStart, 
                                                       fertilityVariant == "Principal",
                                                       mortalityVariant == "Principal",
                                                       migrationVariant == "Principal") %>%
                                                select(sex, age, year, population) %>%
                                                mutate(ageGroup = cut(age, 
                                                                      breaks = c(seq.int(from = 0, to = 90, by = 5), 90 + 1), 
                                                                      right = F,
                                                                      labels = ageLabels)) %>%
                                                select(-age, year) %>%
                                                group_by(sex, ageGroup) %>%
                                                summarise(population = sum(population)) %>%
                                                ungroup() %>%
                                                mutate(sex = ifelse(sex == 1, "Male_PeriodStart", "Female_PeriodStart")) %>%
                                                pivot_wider(names_from = sex, values_from = population),
                                              CoC %>%
                                                filter(year == forecastYearEnd, 
                                                       fertilityVariant == "Principal",
                                                       mortalityVariant == "Principal",
                                                       migrationVariant == "Principal") %>%
                                                select(sex, age, year, population) %>%
                                                mutate(ageGroup = cut(age, 
                                                                      breaks = c(seq.int(from = 0, to = 90, by = 5), 90 + 1), 
                                                                      right = F,
                                                                      labels = ageLabels)) %>%
                                                select(-age, year) %>%
                                                group_by(sex, ageGroup) %>%
                                                summarise(population = sum(population)) %>%
                                                ungroup() %>%
                                                mutate(sex = ifelse(sex == 1, "Male_PeriodEnd", "Female_PeriodEnd")) %>%
                                                pivot_wider(names_from = sex, values_from = population) %>%
                                                select(-ageGroup),
                                              stringsAsFactors = F) %>%
  mutate(growth_Males = (Male_PeriodEnd/Male_PeriodStart)^(1/(forecastYearEnd-forecastYearStart))-1,
         growth_Females = (Female_PeriodEnd/Female_PeriodStart)^(1/(forecastYearEnd-forecastYearStart))-1)

## --------------------------- Create Variant Table ---------------------------

summary.table.chosenVariant = cbind.data.frame(CoC %>%
                                                 filter(year == forecastYearStart, 
                                                        fertilityVariant == fertility_variant,
                                                        mortalityVariant == mortality_variant,
                                                        migrationVariant == migration_variant) %>%
                                                 select(sex, age, year, population) %>%
                                                 mutate(ageGroup = cut(age, 
                                                                       breaks = c(seq.int(from = 0, to = 90, by = 5), 90 + 1), 
                                                                       right = F,
                                                                       labels = ageLabels)) %>%
                                                 select(-age, year) %>%
                                                 group_by(sex, ageGroup) %>%
                                                 summarise(population = sum(population)) %>%
                                                 ungroup() %>%
                                                 mutate(sex = ifelse(sex == 1, "Male_PeriodStart", "Female_PeriodStart")) %>%
                                                 pivot_wider(names_from = sex, values_from = population),
                                               CoC %>%
                                                 filter(year == forecastYearEnd, 
                                                        fertilityVariant == fertility_variant,
                                                        mortalityVariant == mortality_variant,
                                                        migrationVariant == migration_variant) %>%
                                                 select(sex, age, year, population) %>%
                                                 mutate(ageGroup = cut(age, 
                                                                       breaks = c(seq.int(from = 0, to = 90, by = 5), 90 + 1), 
                                                                       right = F,
                                                                       labels = ageLabels)) %>%
                                                 select(-age, year) %>%
                                                 group_by(sex, ageGroup) %>%
                                                 summarise(population = sum(population)) %>%
                                                 ungroup() %>%
                                                 mutate(sex = ifelse(sex == 1, "Male_PeriodEnd", "Female_PeriodEnd")) %>%
                                                 pivot_wider(names_from = sex, values_from = population) %>%
                                                 select(-ageGroup),
                                               stringsAsFactors = F) %>%
  mutate(growth_Males = (Male_PeriodEnd/Male_PeriodStart)^(1/(forecastYearEnd-forecastYearStart))-1,
         growth_Females = (Female_PeriodEnd/Female_PeriodStart)^(1/(forecastYearEnd-forecastYearStart))-1)
