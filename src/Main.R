################################################################################
## Title: Main
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

source("src/Packages.R")
source("src/PublicData.R") # CHANGE TO ONS API
source("src/TestData.R")
##source("RealData.R") # Swap with Test Data if access to raw (NOT CODED YET)

# --------------------------- Set Parameters -----------------------------------
maleWeight = 105 # For estimating births 
femaleWeight = 100  # For estimating births 
options = c("Principal", "Low", "High") # Available variant options
CoC_mortality_function = data.frame(a = 0.998385639632051,
                                    b = -0.0524494034902026,
                                    c = -0.0000202560567696707,
                                    d = 0.0000151832259045123)
migrationPopScale = data.frame(Type = c("Principal", "Low", "High"), 
                               Pop = c(144000, 81500, 206500)) %>% 
  mutate(rate = Pop / Pop[1])

fertScalingStartYear = 2018
mortScalingStartYear = 2018
migrationScalingStartYear = 2018
fertScalingEndYear = 2040
migrationScalingEndYear = 2022

## --------------------------- User Input --------------------------------------
forecastYearStart = 2010 # Start of range to build forecast off
forecastYearEnd = 2018 # End of range to build forecast off
lastOutputYear = 2049 # Outputs from last year of birth data up to this year
fertility_variant = "Low"
mortality_variant = "Low"
migration_variant = "Low"

source("src/CompOfChange.R")
source("src/Tables.R")
source("src/Model.R")
source("src/Plots.R")
