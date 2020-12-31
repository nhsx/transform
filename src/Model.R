################################################################################
## Title: Model
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

## --------------------------- Tidy and reformat ---------------------------

# Correct for sex and age
activity_data = dfSUSFake %>%
  filter(Age %in% c(0:90,"Missing")) %>%
  mutate(Sex = ifelse(Sex %in% c(1, 2), as.character(Sex), "Missing"),
         Age = ifelse(Treatment == "AE" & Age %in% c(0:90, 700), 
                                as.character(Age), 
                                ifelse(Treatment != "AE" & Age %in% c(0:90, 700),
                                       as.character(Age),
                                       "Missing")),
         Age = ifelse(Age == "700", "0", Age)) %>%
  select(Treatment, Sex, Age, Setting, Year, Activities, Cost) %>%
  pivot_longer(-c(Treatment, Sex, Age, Setting, Year), names_to = "AC", values_to = "value") %>%
  # create grouped totals
  group_by(Treatment, Sex, Age, Setting, Year, AC) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  # create attribute known age unknown sex column
  group_by(Treatment, Age, Setting, Year, AC) %>%
  mutate(attKnownAgeUnknownSex_TotalExclMissing = sum(value[Sex != "Missing"]),
         attKnownAgeUnknownSex_TotalMissing = sum(value[Sex == "Missing"])) %>%
  ungroup() %>%
  mutate(attKnownAgeUnknownSex = value / attKnownAgeUnknownSex_TotalExclMissing * attKnownAgeUnknownSex_TotalMissing) %>%
  # create attribute known sex unknown age column
  group_by(Treatment, Sex, Setting, Year, AC) %>%
  mutate(attKnownSexUnknownAge_TotalExclMissing = sum(value[Age != "Missing"]),
         attKnownSexUnknownAge_TotalMissing = sum(value[Age == "Missing"])) %>% 
  ungroup() %>%
  mutate(attKnownSexUnknownAge = value / attKnownSexUnknownAge_TotalExclMissing * attKnownSexUnknownAge_TotalMissing) %>%
  # create attribute known sex unknown age column
  group_by(Treatment, Setting, Year, AC) %>%
  mutate(attUnknownAgeUnknownSex_TotalExclMissing = sum(value[Sex != "Missing" & Age != "Missing"]),
         attUnknownAgeUnknownSex_TotalMissing = sum(value[Sex == "Missing" & Age == "Missing"])) %>%
  ungroup() %>%
  mutate(attUnknownAgeUnknownSex = value / attUnknownAgeUnknownSex_TotalExclMissing * attUnknownAgeUnknownSex_TotalMissing,
         Counts = value + attKnownAgeUnknownSex + attKnownSexUnknownAge + attUnknownAgeUnknownSex)

## Create reduced data frame without Missings.  Contains Totals with imputation from SUS data 
tblCount <- activity_data %>% 
  filter(Sex != "Missing" & Age != "Missing") %>% 
  mutate(Sex = as.numeric(Sex),
         Age = as.numeric(Age)) %>% 
  select(Treatment, Sex, Age, Setting, Year, AC, Counts)
names(tblCount) <- c("Treatment", "Sex", "Age", "Setting", "Year", "ValueType", "value")

## Create data frame to run calculation on.  For this need additional fake 5 years of Age data 
## either side - set to Age 90 and Age 0. REDO POOR CODING
dfSpline <- tblCount %>% filter(Age <= 90) %>%  left_join(ons_GPRegAdjustmentRatio,by = c("Sex" = "sex","Age" = "age"))
upper91 <- dfSpline %>% filter(Age == 90) %>% mutate(Age = 91)
upper92 <- dfSpline %>% filter(Age == 90) %>% mutate(Age = 92)
upper93 <- dfSpline %>% filter(Age == 90) %>% mutate(Age = 93)
upper94 <- dfSpline %>% filter(Age == 90) %>% mutate(Age = 94)
upper95 <- dfSpline %>% filter(Age == 90) %>% mutate(Age = 95)
lowerm1 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -1)
lowerm2 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -2)
lowerm3 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -3)
lowerm4 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -4)
lowerm5 <- dfSpline %>% filter(Age == 0) %>% mutate(Age = -5)
dfSpline <- rbind(dfSpline,upper91,upper92,upper93,upper94,upper95,lowerm1,lowerm2,lowerm3,lowerm4,lowerm5)

## Filter out Other and Maternity as Age ranges are not the same as other settings
## Can redo these separately if required.
## Calculate Rates using the SUS total over GP Historic Registrations
dfSpline <- dfSpline %>% 
  filter(Treatment != "Other", Treatment != "Maternity") %>%
  mutate(Rate = value/Ratio_GPRegVsONSPop) %>%  
  arrange(Sex, Setting, Treatment, ValueType, Age, Year) %>%
  mutate(Age = Age + 0.5) %>%
  as.data.frame() 

## --------------------------- Main Calculation ---------------------------

## Run a linear fit for the logarithm of the rate dependant on an Age B-spline, the Year 
## and the interaction of these.  This then takes into account the interaction as age 
## cohorts get older through time
fitAll <- dfSpline %>%
  group_by(Sex,Setting,Treatment,ValueType) %>%
  do(fit = lm(log(Rate) ~ splines::bs(Age, knots=c(0.5, 4.5, 14.5, 34.5, 54.5, 64.5, 80.5)) + 
                Year + 
                Year*splines::bs(Age, knots=c(0.5, 4.5, 14.5, 34.5, 54.5, 64.5, 80.5)), weights = Ratio_GPRegVsONSPop, data = .))

## All base Age splines the same so take any  
baseAgeSpline <- splines::bs(dfSpline %>% 
                      filter(Sex==1,Setting=="APC",Treatment=="NonElective",ValueType=="Activities",Year==2016) %>% 
                      select(Sex,Setting,Treatment,ValueType,Age) %>% pull(), knots=c(0.5, 4.5, 14.5, 34.5, 54.5, 64.5, 80.5))

## Annually growth can be extracted from each model by taking the first deriavative in "Year"
dfGrowth <- data.frame(matrix(nrow = 91, ncol = 28))
for (ii in 1:dim(fitAll)[1]){
  dfGrowth[,ii] <- fitAll$fit[[ii]]$coefficients[12] +
    fitAll$fit[[ii]]$coefficients[13]*baseAgeSpline[6:96,1] + 
    fitAll$fit[[ii]]$coefficients[14]*baseAgeSpline[6:96,2] +
    fitAll$fit[[ii]]$coefficients[15]*baseAgeSpline[6:96,3] +
    fitAll$fit[[ii]]$coefficients[16]*baseAgeSpline[6:96,4] + 
    fitAll$fit[[ii]]$coefficients[17]*baseAgeSpline[6:96,5] + 
    fitAll$fit[[ii]]$coefficients[18]*baseAgeSpline[6:96,6] + 
    fitAll$fit[[ii]]$coefficients[19]*baseAgeSpline[6:96,7] + 
    fitAll$fit[[ii]]$coefficients[20]*baseAgeSpline[6:96,8] + 
    fitAll$fit[[ii]]$coefficients[21]*baseAgeSpline[6:96,9] +
    fitAll$fit[[ii]]$coefficients[22]*baseAgeSpline[6:96,10]
}

## Bodge to add categories to growth calculation
dfGrowth <- dfGrowth %>% 
  gather()
dfGrowth$Sex <- c(rep(1,91*14),rep(2,91*14))
dfGrowth$Treatment <- rep(c(rep("AE",2*91),
                            rep("Elective",2*91),
                            rep("MHLD",2*91),
                            rep("NonElective",2*91),
                            rep("First",2*91),
                            rep("FollowUp",2*91),
                            rep("MHLD",2*91)),2)
dfGrowth$Age <- rep(seq(1:91)-1,2*14)
dfGrowth$Setting <- c(rep("AE",2*91),rep("APC",6*91),rep("OPA",6*91))
dfGrowth$ValueType <- rep(c(rep("Activities",91),rep("Cost",91)),14)

## Create summary output data frame
dfOutput <- ons_population %>% 
  filter(age >= 0 & age <= 90) %>%
  spread(key = year, value = population) %>% 
  right_join(dfSpline %>% 
               mutate(Age = Age - 0.5) %>%
               filter(Age >= 0 & Age <= 90) %>%
               select(Treatment, Sex, Age, Setting, Year, ValueType, value) %>%
               spread(key = Year, value = value), 
             by = c("sex" = "Sex","age" = "Age")) %>%
  left_join(dfSpline %>% 
              mutate(Age = Age - 0.5) %>% 
              filter(Age >= 0 & Age <= 90) %>% 
              select(Treatment, Sex, Age, Setting, Year, ValueType, Rate) %>% 
              spread(key = Year, value = Rate), by = c("Treatment","sex" = "Sex","age" = "Age","Setting","ValueType")) %>%
  left_join(dfGrowth %>% 
              select(-key), by = c("Treatment","sex" = "Sex","age" = "Age","Setting","ValueType")) %>%
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

## restructure and summarise for all_age values
dfAllAge <- as.data.frame(dfOutput) %>% 
  select("Setting","Treatment","sex","ValueType","2018","2019","2020","2021","2022","2023","2024","2025","2026","2027","2028") %>%
  group_by(Setting,Treatment,sex,ValueType) %>%
  summarise_all(list(sum = mean)) %>%
  mutate(growth = (`2028_sum`-`2018_sum`)/(10*`2018_sum`)) %>%
  gather(Year, Value, -Setting, -sex, -Treatment, -ValueType)

write.csv(dfGrowth, file = "outputs/dfGrowth.csv")
write.csv(dfOutput, file = "outputs/dfOutput.csv")
write.csv(dfAllAge, file = "outputs/dfAllAge.csv")
