################################################################################
## Title: TestData
## Last Update: 31/12/2020
## Version: 2.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

## Generate Fake SUS and GP Registration data

## --------------------------- Generate Fake SUS data  ---------------------------
dfSUSFake <- data.frame(Treatment = rep(c(rep("AE",2*91),
                                          rep("Elective",2*91),
                                          rep("MHLD",2*91),
                                          rep("NonElective",2*91),
                                          rep("First",2*91),
                                          rep("FollowUp",2*91),
                                          rep("MHLD",2*91)),9),
                        Sex = rep(c(rep(1,91),rep(2,91)),7*9),
                        Age = rep(seq(1:91)-1,2*7*9),
                        Year = sort.int(c(rep(seq(from = 2010, to = 2018),2*7*91)),decreasing = F),
                        Setting = rep(c(rep("AE",2*91),rep("APC",6*91),rep("OPA",6*91)),9),
                        Activities = c(0),
                        Cost = c(0))

## USe case when and norm distribution around an average with a growth multiplier for the year
dfSUSFake$Activities[dfSUSFake$Treatment == "AE"] <- (-0.00003*dfSUSFake$Age[dfSUSFake$Treatment == "AE"]**4 +
                                                      0.0157*dfSUSFake$Age[dfSUSFake$Treatment == "AE"]**3 -
                                                      1.9405*dfSUSFake$Age[dfSUSFake$Treatment == "AE"]**2 +
                                                      50.745*dfSUSFake$Age[dfSUSFake$Treatment == "AE"] + 
                                                      3992.1)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "AE"]-2010)*0.03
  
dfSUSFake$Activities[dfSUSFake$Treatment == "Elective"] <- (-0.0091*dfSUSFake$Age[dfSUSFake$Treatment == "Elective"]**4 +
                                                        1.014*dfSUSFake$Age[dfSUSFake$Treatment == "Elective"]**3 -
                                                        18.297*dfSUSFake$Age[dfSUSFake$Treatment == "Elective"]**2 +
                                                        50.745*dfSUSFake$Age[dfSUSFake$Treatment == "Elective"] + 
                                                        11322)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "Elective"]-2010)*0.03

dfSUSFake$Activities[dfSUSFake$Treatment == "MHLD"] <- (-0.0002*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"]**4 +
                                                        0.1026*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"]**3 -
                                                        13.003*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"]**2 +
                                                        573.64*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"] -
                                                        1056.7)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "MHLD"]-2010)*0.06

dfSUSFake$Activities[dfSUSFake$Treatment == "NonElective"] <- (0.0066*dfSUSFake$Age[dfSUSFake$Treatment == "AE"]**4 -
                                                        1.4444*dfSUSFake$Age[dfSUSFake$Treatment == "AE"]**3 +
                                                        109.66*dfSUSFake$Age[dfSUSFake$Treatment == "AE"]**2 -
                                                        3056.4*dfSUSFake$Age[dfSUSFake$Treatment == "AE"] + 
                                                        38140)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "AE"]-2010)*0.01

dfSUSFake$Activities[dfSUSFake$Treatment == "First"] <- (0.0024*dfSUSFake$Age[dfSUSFake$Treatment == "First"]**4 -
                                                        1.7289*dfSUSFake$Age[dfSUSFake$Treatment == "First"]**3 +
                                                        180.73*dfSUSFake$Age[dfSUSFake$Treatment == "First"]**2 -
                                                        4462.6*dfSUSFake$Age[dfSUSFake$Treatment == "First"] + 
                                                        83059)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "First"]-2010)*0.04

dfSUSFake$Activities[dfSUSFake$Treatment == "FollowUp"] <- (-0.0572*dfSUSFake$Age[dfSUSFake$Treatment == "FollowUp"]**4 +
                                                        6.9239*dfSUSFake$Age[dfSUSFake$Treatment == "FollowUp"]**3 +
                                                        165.5*dfSUSFake$Age[dfSUSFake$Treatment == "FollowUp"]**2 +
                                                        303.14*dfSUSFake$Age[dfSUSFake$Treatment == "FollowUp"] + 
                                                        109434)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "FollowUp"]-2010)*0.04

dfSUSFake$Activities[dfSUSFake$Treatment == "MHLD"] <- (-0.0002*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"]**4 +
                                                        0.1026*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"]**3 -
                                                        13.003*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"]**2 +
                                                        573.64*dfSUSFake$Age[dfSUSFake$Treatment == "MHLD"] -
                                                        1056.7)*(1+dfSUSFake$Year[dfSUSFake$Treatment == "MHLD"]-2010)*0.06

## Set any generated negatives to zero
dfSUSFake$Activities[dfSUSFake$Activities < 0 ] <- 1
  
## Assume all costs £100 per activity
dfSUSFake$Cost <- dfSUSFake$Activities*100

## Add noise
dfSUSFake$Activities <- dfSUSFake$Activities*runif(dim(dfSUSFake)[1],min = 0.95,max = 1.05)
dfSUSFake$Cost <- dfSUSFake$Cost*runif(dim(dfSUSFake)[1],min = 0.95,max = 1.05)

## -------------------- Generate Fake GP Registration History  ------------------
dfGPRegFake <- data.frame(Sex = rep(c(rep(1,91),rep(2,91)),9),
                        Age = rep(seq(1:91)-1,2*9),
                        Year = sort.int(c(rep(seq(from = 2010, to = 2018),2*91)),decreasing = F),
                        Registered = c(0))

dfGPRegFake$Registered <- ons_population$population*rep(ons_GPRegAdjustmentRatio$Ratio_GPRegVsONSPop,9)

