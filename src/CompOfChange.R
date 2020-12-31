################################################################################
## Title: Components of Change
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

## --------------------------- Population variant model ------------------------
CoC = data.frame()

for(ff in 1:length(options)){
  fertilityVariant = options[ff]
  
  for(mm in 1:length(options)){
    mortalityVariant = options[mm]
    
    for(ii in 1:length(options)){
      migrationVariant = options[ii]
      
      # fertility variant
      fertRate_Var = ons_fertilityRate %>%
        mutate(VarRate = ons_CoCFertility %>% filter(Variant == fertilityVariant) %>% select(Rate) %>% pull(),
               PrincipalVarRate = ons_CoCFertility %>% filter(Variant == "Principal") %>% select(Rate) %>% pull(),
               scaling = ifelse((year - fertScalingStartYear) /(fertScalingEndYear - fertScalingStartYear) > 1, 
                                1, 
                                (year - fertScalingStartYear) /(fertScalingEndYear - fertScalingStartYear)),
               multiplier = ((1 - scaling) * PrincipalVarRate + scaling * VarRate) / PrincipalVarRate,
               fertilityRate = fertilityRate * multiplier) %>%
        select(sex, age, year, fertilityRate)
      
      # mortality variant
      mortRate_Var = ons_mortalityRate %>%
        left_join(ons_CoCMortality %>% filter(Type == "Principal") %>% select(sex, age, Rate), by = c("age", "sex")) %>%
        rename("Principal" = "Rate") %>%
        left_join(ons_CoCMortality %>% filter(Type == mortalityVariant) %>% select(sex, age, Rate), by = c("age", "sex")) %>%
        mutate(x = year - mortScalingStartYear,
               scaling = ifelse(CoC_mortality_function$a + x * CoC_mortality_function$b + x^2 * CoC_mortality_function$c + x^3 * CoC_mortality_function$d < 0,
                                0,
                                CoC_mortality_function$a + x * CoC_mortality_function$b + x^2 * CoC_mortality_function$c + x^3 * CoC_mortality_function$d),
               mortalityRatePer100K_new = ifelse(year <= mortScalingStartYear + 1, mortalityRatePer100K, NA))
      
      for(rr in 1:nrow(mortRate_Var)){
        if(mortRate_Var$year[rr] > mortScalingStartYear + 1){
          mortRate_Var$mortalityRatePer100K_new[rr] = ((mortRate_Var$Rate[rr] - mortRate_Var$Principal[rr]) *
                                                         (1 - mortRate_Var$scaling[rr]) +
                                                         (mortRate_Var$mortalityRatePer100K[rr] / mortRate_Var$mortalityRatePer100K[rr - 1])) *
            mortRate_Var$mortalityRatePer100K_new[rr - 1]
        }
      }
      
      mortRate_Var = mortRate_Var %>%
        mutate(mortalityRatePer100K = mortalityRatePer100K_new) %>%
        select(sex, age, year, mortalityRatePer100K)
      
      # migration variant
      migration_Var = ons_migration %>%
        mutate(scaling = ((year - migrationScalingStartYear) / (migrationScalingEndYear - migrationScalingStartYear)),
               scaling = ifelse(scaling >= 0 & scaling <= 1,
                                scaling,
                                ifelse(scaling < 0, 0, 1)),
               rate = ons_CoCMigration %>% filter(Variant == migrationVariant) %>% select(Rate) %>% pull(),
               multiplier = rate * (1 - scaling) + migrationPopScale$rate[migrationPopScale$Type == migrationVariant] * scaling,
               netMigration = ifelse(year > migrationScalingStartYear, netMigration * multiplier, netMigration)) %>%
        select(sex, age, year, netMigration)
      
      sex = 1:2
      ages = seq.int(from = min(ons_maternities$age, ons_deaths$age, ons_migration$age, ons_population$age),
                     to = max(ons_maternities$age, ons_deaths$age, ons_migration$age, ons_population$age), 
                     by = 1)
      years = seq.int(from = min(ons_maternities$year, ons_deaths$year, ons_migration$year, ons_population$year),
                      to = max(ons_maternities$year, ons_deaths$year, ons_migration$year, ons_population$year), 
                      by = 1)
      
      ons_CoC = data.frame(sex = rep(sex,
                                     each = length(ages) * length(years)),
                           age = rep(ages,
                                     each = length(years),
                                     times = length(sex)),
                           year = years) %>%
        left_join(ons_population, by = c("sex", "age", "year")) %>%
        left_join(ons_maternities, by = c("sex", "age", "year")) %>%
        left_join(ons_deaths, by = c("sex", "age", "year")) %>%
        left_join(ons_migration, by = c("sex", "age", "year"))
      
      lastYearBirths = max(as.numeric(ons_maternities$year))
      yearsToCalc = (lastYearBirths + 1):lastOutputYear
      
      for(yy in 1:length(yearsToCalc)){
        estimatedBirths = ons_CoC %>%
          filter(year %in% c(yearsToCalc[yy], yearsToCalc[yy] + 1)) %>%
          left_join(fertRate_Var %>% filter(year == yearsToCalc[yy]), by = c("sex", "age", "year")) %>%
          group_by(age, sex) %>%
          mutate(estPop = sum(population)) %>%
          filter(year == yearsToCalc[yy]) %>%
          mutate(births_new = round(estPop * fertilityRate / 1000 / 2),
                 deaths_new = NA) %>%
          select(-fertilityRate, -estPop, -maternities, -deaths) %>%
          filter(!is.na(births_new))
        if(yy > 1){
          init = data.frame(sex = c(1, 2),
                            age = -1,
                            year = yearsToCalc[yy],
                            weight = c(maleWeight, femaleWeight)) %>%
            left_join(mortRate_Var, by = c("sex", "age", "year")) %>%
            mutate(births_new = sum(estimatedBirths$births_new) * (weight/ sum(weight)),
                   deaths_new = round(births_new * mortalityRatePer100K / 100000)) %>%
            select(-weight, -mortalityRatePer100K)
          
          initPop = init %>%
            mutate(age = 0) %>%
            left_join(migration_Var, by = c("sex", "age", "year")) %>%
            mutate(year = year + 1,
                   population_new = births_new + netMigration / 2 - deaths_new) %>%
            select(sex, age, year, population_new)
          
          ons_CoC = ons_CoC %>%
            left_join(init, by = c("sex", "age", "year")) %>%
            left_join(initPop, by = c("sex", "age", "year")) %>%
            mutate(births = ifelse(!is.na(births_new), births_new, maternities),
                   deaths = ifelse(!is.na(deaths_new), deaths_new, deaths),
                   population = ifelse(!is.na(population_new), population_new, population)) %>%
            select(-ends_with("_new")) %>%
            arrange(sex, age, year)
        }
        
        estimatedDeaths = ons_CoC %>%
          filter(year == (yearsToCalc[yy] + 1)) %>%
          left_join(mortRate_Var %>% filter(year == yearsToCalc[yy]+1), by = c("sex", "age", "year")) %>%
          mutate(births_new = NA,
                 deaths_new = round((population + 0.5 * netMigration) * mortalityRatePer100K / 100000)) %>%
          select(-mortalityRatePer100K, -maternities, -deaths)
        
        ons_CoC = ons_CoC %>%
          left_join(select(rbind.data.frame(estimatedBirths, estimatedDeaths), sex, age, year, births_new, deaths_new),
                    by = c("sex", "age", "year")) %>%
          mutate(births = ifelse(!is.na(births_new), births_new, maternities),
                 deaths = ifelse(!is.na(deaths_new), deaths_new, deaths)) %>%
          select(-ends_with("_new"))
        
        estimatedPopulation_OnePlus = ons_CoC %>%
          filter(year == (yearsToCalc[yy] + 1)) %>%
          group_by(sex) %>%
          mutate(year = (yearsToCalc[yy] + 2),
                 population_new = lag(population, 1) + (lag(netMigration, 1) + netMigration) / 2 - lag(deaths, 1)) %>%
          ungroup()
        
        ons_CoC = ons_CoC %>%
          left_join(select(estimatedPopulation_OnePlus, sex, age, year, population_new),
                    by = c("sex", "age", "year")) %>%
          mutate(population = ifelse(!is.na(population_new), population_new, population)) %>%
          select(-ends_with("_new"))
        
      }
      
      ons_CoC = ons_CoC %>%
        filter(age >= 0) %>%
        mutate(fertilityVariant = fertilityVariant,
               mortalityVariant = mortalityVariant,
               migrationVariant = migrationVariant)
      
      CoC = rbind.data.frame(CoC, ons_CoC, stringsAsFactors = F)
    }
  }
}
