@startuml
scale max 1200 height
skinparam activity {
 BorderColor DarkSlateGray
}
skinparam ArrowColor DarkSlateGray
title Modeling Workflow
partition "Data Preparation" {
#cbf2d0:Prepare:
 - Weather Data
 - Google Search Data
 - Chicago City Zoning Data
 - Socio-Economic Status Data]
#cbf2d0:Remove Duplicates
Impute missing values
Data Quality Checks]
}
partition "Feature\nEngineering" {
#cbf2d0:Add: 
 - Temporal Features
 - Geospatial Features
 - Lagged Weather Features
 - Weight Of Evidence Features]
#cbf2d0:Add dummy variables for categorical
features with < 30 levels]
#cbf2d0:Optionally: scale & center variables
Add Principal Components]
}
partition Modeling {
#cbf2d0:Split dataset into train, validation & test]
split 
:**Regression:**
**Predict No of Mosquitos**;
#cbf2d0:Determine variable importance]
#cbf2d0:Reduce variables by filtering
or variable selection procedures]
:Apply 
- list regression models here]
:MORE TASKS??]
split again
:**Classification:**
**Predict Presence of WNV**;
#cbf2d0:Determine variable importance]
#cbf2d0:Reduce variables by filtering
or variable selection procedures]
#cbf2d0:Apply these learners:
 - Logistic Regression
 - Linear Discriminant Analysis
 - Negative Binomial
 - Recursive Partition Trees
 - Random Forest
 - XG Boost
 - Extra Trees]
#cbf2d0:Evalaute performance using
**kappa, TPR, FPR** and **AUC ROC**]
:Investigate hyperparameter tuning,
threshold selection, and 
class re-balance strategies;
:Select best learner, tuning parameter 
& model setup]

split end
#c1e3ff://Strech Goal// : Perform ensemble models
over both regr & class models|
#c1e3ff:Feed output of regression model
into classification model to predict
location & presence of WNV in Chicago|
}
@enduml
