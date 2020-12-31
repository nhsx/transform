################################################################################
## Title: Plots
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

## --------------------------- Create Population Change Chart ------------------

chart.population.time.series <- cbind.data.frame(CoC %>%
                                                  filter(fertilityVariant == "Principal",
                                                         mortalityVariant == "Principal",
                                                         migrationVariant == "Principal") %>%
                                                  group_by(year) %>%
                                                  summarise(population = sum(population)) %>%
                                                  mutate(Population_Estimates = ifelse(year <= forecastYearStart - 1, population, NA),
                                                         Principal_Projection = ifelse(year >= forecastYearStart - 1, population, NA)),
                                                CoC %>%
                                                  filter(fertilityVariant == fertility_variant,
                                                         mortalityVariant == mortality_variant,
                                                         migrationVariant == migration_variant) %>%
                                                  group_by(year) %>%
                                                  summarise(population = sum(population)) %>%
                                                  mutate(Variant_Projection = ifelse(year >= forecastYearStart - 1, population, NA)) %>%
                                                  select(Variant_Projection)) %>%
  select(-population) %>%
  pivot_longer(-year, names_to = "Data", values_to = "Population") %>%
  ggplot(aes(x = year, y = Population, colour = Data)) +
  geom_line()

pdf("outputs/PopTimeSeries.pdf") 
print(chart.population.time.series)
dev.off() 

## -------------- Create Age/Sex Pyramid for principal and variant -------------

chart.sexComp.allPrincipal <- CoC %>%
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
  mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female")),
         population = ifelse(sex == "Male", -population, population)) %>%
  ggplot(aes(x = ageGroup, y = population, group = sex, fill = sex)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = rev(ageLabels)) +
  scale_y_continuous(breaks = seq(-3000000, 3000000, 1000000), 
                     labels = abs(seq(-3000000, 3000000, 1000000))) +
  labs(x = "Age Group", y = "Population",
       title = paste0("Population Age Profile - " , forecastYearEnd),
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

chart.sexComp.chosenVariant <- CoC %>%
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
  mutate(sex = as.factor(ifelse(sex == 1, "Male", "Female")),
         population = ifelse(sex == "Male", -population, population)) %>%
  
  ggplot(aes(x = ageGroup, y = population, group = sex, fill = sex)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = rev(ageLabels)) +
  scale_y_continuous(breaks = seq(-3000000, 3000000, 1000000), 
                     labels = abs(seq(-3000000, 3000000, 1000000))) +
  labs(x = "Age Group", y = "Population", 
       title = paste0("Population Age Profile - " , forecastYearEnd),
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

pdf("outputs/PopPyramidComp.pdf") 
grid.arrange(chart.sexComp.allPrincipal,chart.sexComp.chosenVariant, ncol=2)
dev.off() 

## ------------- Create Age/Sex growth per age group chart ---------------------

chart.growthComp.allPrincipal <- summary.table.allPrincipal %>%
  select(ageGroup, starts_with("Growth")) %>%
  pivot_longer(-ageGroup, names_to = "Sex", values_to = "Growth") %>%
  mutate(Sex = ifelse(Sex == "Growth_Males", "Male", "Female")) %>%
  ggplot(aes(x = ageGroup, y = Growth, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  labs(x = "Age Group", y = "Growth", 
       title = paste0("Average Annual Growth - " , forecastYearStart, " to ", forecastYearEnd),
       subtitle = paste0("Fertility: ", "Principal", "; Mortality: ", "Principal", "; Migration: ", "Principal"))

chart.growthComp.chosenVariant <- summary.table.chosenVariant %>%
  select(ageGroup, starts_with("Growth")) %>%
  pivot_longer(-ageGroup, names_to = "Sex", values_to = "Growth") %>%
  mutate(Sex = ifelse(Sex == "Growth_Males", "Male", "Female")) %>%
  ggplot(aes(x = ageGroup, y = Growth, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  labs(x = "Age Group", y = "Growth", 
       title = paste0("Average Annual Growth - " , forecastYearStart, " to ", forecastYearEnd),
       subtitle = paste0("Fertility: ", fertility_variant, "; Mortality: ", mortality_variant, "; Migration: ", migration_variant))

pdf("outputs/PopGrowthComp.pdf") 
grid.arrange(chart.growthComp.allPrincipal, chart.growthComp.chosenVariant, ncol=2)
dev.off() 

## --------------------------- Predicted growth cahrts -------------------------

chart.growthActivities.grid <- dfGrowth %>% filter(ValueType == "Activities", Age >=0, Age <= 90) %>%
  ggplot(aes(Age, value)) + 
  geom_line(size = 1, color = "red") +
  facet_wrap(~Sex+Treatment+Setting)

pdf("outputs/ActivityGrowthGrid.pdf") 
print(chart.growthActivities.grid)
dev.off() 

chart.growthCost.grid <- dfGrowth %>% filter(ValueType == "Cost", Age >=0, Age <= 90) %>%
  ggplot(aes(Age, value)) + 
  geom_line(size = 1, color = "red") +
  facet_wrap(~Sex+Treatment+Setting)

pdf("outputs/CostGrowthGrid.pdf") 
print(chart.growthCost.grid)
dev.off() 

chart.timeseries <- ggplot(dfAllAge %>% 
         filter(ValueType == "Cost" & Setting == "APC" & Treatment == "NonElective" & sex == 1 & Year != "growth"), aes(x=Value, y = Year)) + 
  geom_line(group=1)

pdf("outputs/ActivityTimeseries.pdf") 
print(chart.timeseries)
dev.off() 

rChartData <- as.data.frame(dfAllAge %>% 
                              filter(sex == 1 & Year == "growth") %>% 
                              mutate(SetTreat = paste0(Setting,Treatment)) %>% 
                              ungroup() %>% 
                              select(-"Setting", -"Treatment", -"sex", -"Year") %>%
                              spread(key = ValueType, value = Value)) 
chart.growth.comparison <- ggplot(rChartData) + 
  geom_segment(aes(x=SetTreat, xend=SetTreat, y=Activities, yend=Cost), color = "grey") + 
  geom_point(aes(x=SetTreat, y = Activities), color=rgb(0.2,0.7,0.1,0.5), size=3) +
  geom_point(aes(x=SetTreat, y = Cost), color=rgb(0.7,0.2,0.1,0.5), size=3) +
  coord_flip() + 
  theme(plot.title = element_text(color="#993333", size=14, face="bold.italic")) +
  xlab("") + 
  ylab("Growth") + 
  ggtitle("Growth per setting/Treatment combination \n {Activites = Green, Cost = Red}")

pdf("outputs/ModelGrowthComp.pdf") 
print(chart.growth.comparison) 
dev.off() 
