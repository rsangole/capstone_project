# Ted Inciong
library(RCurl)
library(dplyr)
library(ggplot2)
library(Rmisc) #used for multiplot - issue with ggplot

github <- "https://raw.githubusercontent.com/rsangole/capstone_project/master/data/raw/chi_dept_public_health/West_Nile_Virus__WNV__Mosquito_Test_Results.csv"

x <- getURL(github)

out <- read.csv(textConnection(x))

head(out)

## plotting key variables against WNV result

# ---- YEAR
p_YR = ggplot(out, mapping = aes(x = SEASON.YEAR, fill = RESULT)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Year') +
  scale_fill_discrete(name="Result") 

# ---- WEEK
p_WEEK = ggplot(out, mapping = aes(x = WEEK, fill = RESULT)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Week') +
  scale_fill_discrete(name="Result") 

# ---- SPECIES  
p_species = ggplot(out, aes(SPECIES, fill = RESULT)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Species') +
  theme(legend.position = "none")
p_species <- p_species +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# ---- PLOT
Rmisc::multiplot(p_YR, p_WEEK, p_species, cols=1)