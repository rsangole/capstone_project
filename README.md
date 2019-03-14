# Predict 498 Capstone Project

* **Final Team Paper** : [Link](https://github.com/rsangole/capstone_project/blob/master/reports/West%20Nile%20Final%20Paper%20(team%2054).pdf)
* **Team Presentation** : [Link](https://www.youtube.com/watch?v=Gl2StkLlVqU)

## Repository Details

* **data** : Holds all the raw and post processed datasets to be used for visualization, modeling and dashboarding. `raw` holds the original raw datasets, `processed` holds the post processed datasets which go into modeling, EDA and dashboarding.
* **munge** : Holds the scripts which converts the `raw` data to `processed` data. This allows for end to end reproduceability. Basically, `raw` + `munge` = `processed`
* **images** : Holds images from EDA and modeling activities
* **src** : Holds all the scripts for EDA, plotting, modeling and dashboarding. Does not hold scripts for converting data from `raw` to `processed` state. These scripts reside in `munge`.
* **kaggle_original_data** : Holds data and scripts pulled from the West Nile Virus Kaggle competition.
* **docs** : Holds literature research and other project related information
* **reports** : Holds markdown, jupyter notebooks, dashboards and PDF reports created throughout the project
