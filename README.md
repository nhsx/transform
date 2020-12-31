# transform
Summary: Methodology to calculate impact of aging population on activity forecasts.  

Data: Uses publically available data from ONS for population figures and variants (calculating using the components of change methodology).  Activity and cost data comes from the secondary users services.  For the purposes of collaboration we have created a very naive set of fake data here.

Method: The main calculation fits a b-spline to the activity/cost over age.  This is then used in a linear model considering age, year and age\*year.  This allows forecasts to be made which take into account not just the growth of activity/cost for a particular setting (APC, AE, OPA) and Treatment (AE, MHLD, Non-Elective, First Appoitnment etc..), but also takes into account the predicted changing population due to fluctuations in births, deaths and migration. 
