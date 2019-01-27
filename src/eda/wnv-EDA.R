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

# -------------------------------------------------------------------------------
# RAHUL --> ggthemr for nice plots
# -------------------------------------------------------------------------------

# install.packages("cowplot")
library("cowplot")
# devtools::install_github('cttobin/ggthemr')
library("ggthemr")
library("tidyverse")

ggthemr("fresh")

df <- as_tibble(out)

df <- df %>% mutate(
    YR_WK = as.POSIXct(paste0(SEASON.YEAR," ",WEEK, " 1"), format = "%Y %U %u"),
    RESULT = factor(stringr::str_to_title(RESULT), levels = c("Negative","Positive"))
) %>%
    arrange(YR_WK)


# ---- YEAR
p_YR = ggplot(df, mapping = aes(x = SEASON.YEAR, fill = RESULT)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'Year', y = "Mosquitos with WNV") +
    scale_fill_discrete(name="")+
    scale_x_continuous(breaks = seq(2007,2018))+
    scale_y_continuous(labels = scales::percent)

# ---- WEEK
p_WEEK = ggplot(df, mapping = aes(x = WEEK, fill = RESULT)) +
    geom_bar(stat='count', position='fill') +
    labs(x = 'Week Number', y = "Mosquitos with for WNV") +
    scale_fill_discrete(name="")+
    scale_y_continuous(labels = scales::percent)

cowplot::plot_grid(p_YR, p_WEEK, ncol = 1)

# ---- SPECIES and TS PLOT
df %>%
    group_by(YR_WK, SPECIES, RESULT) %>%
    dplyr::summarize(avg_mos = mean(NUMBER.OF.MOSQUITOES)) %>%
    ggplot()+
    geom_point(aes(YR_WK, avg_mos,color=forcats::fct_rev(RESULT)),alpha = 0.6)+
    facet_wrap(~SPECIES,ncol = 3)+
    scale_x_datetime(date_breaks = "year",date_labels = "%Y")+
    scale_color_discrete(name="West Nile Detected")+
    labs(y="Average number of mosquitoes tested",x="")+
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, hjust = 1))
df %>%
    group_by(SEASON.YEAR, SPECIES, RESULT) %>%
    dplyr::summarize(avg_mos = mean(NUMBER.OF.MOSQUITOES), sd_mos = sd(NUMBER.OF.MOSQUITOES)) %>%
    ggplot(aes(SEASON.YEAR, avg_mos,color=forcats::fct_rev(RESULT)))+
    geom_point()+
    geom_line()+
    geom_ribbon(aes(ymax=avg_mos+sd_mos,ymin=avg_mos-sd_mos),alpha=0.2,linetype=0)+
    facet_wrap(~SPECIES,ncol = 3)+
    scale_x_continuous(breaks = seq(2007,2018))+
    scale_color_discrete(name="West Nile Detected")+
    labs(y="Average number of mosquitoes tested",x="")+
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, hjust = 1))
