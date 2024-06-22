# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(sasLM)) install.packages("sasLM")
library(rstatix); library(ggpubr); library(sasLM)

# Loading dataset
load(file = "Date 4.RData"); head(ds)

# I. Setting-up a long data structure ####
ds <- ds %>% 
  gather(key = "Moment", value = "Stress", Initial, Intermediar, Final) %>% 
  convert_as_factor(ID, Moment, Grup)
ds$Moment <- ordered(ds$Moment, c("Initial", "Intermediar", "Final"))

# II. Descriptive analysis and visualization system ####
dsc <- ds %>% 
  group_by(Grup, Moment) %>% 
  get_summary_stats(Stress, type = "full"); dsc

grp.1 <- ggboxplot(data = ds, y = "Stress", x = "Moment", color = "Grup",
                   bxp.errorbar = TRUE, orientation = "vertical",
                   ylab = "Punctaj stres (0-10)", 
                   xlab = FALSE, add = c("mean", "jitter"),
                   ggtheme = theme_pubr(), palette = "jco"); grp.1

grp.2 <- ggboxplot(data = ds, y = "Stress", x = "Grup", color = "Moment",
                   bxp.errorbar = TRUE, orientation = "vertical",
                   ylab = "Punctaj stres (0-10)", 
                   xlab = FALSE, add = c("mean", "jitter"),
                   ggtheme = theme_pubr(), palette = "jco"); grp.2

# II. Assumption analysis ####
ds %>% 
  group_by(Grup, Moment) %>% 
  identify_outliers(Stress)

ds %>%
  group_by(Grup, Moment) %>% 
  shapiro_test(Stress)
ggqqplot(data = ds, x = "Stress", color = "dark green",
         xlab = "Distributia normala teoretica", ylab = "Distributia aprecierii") +
  facet_grid(Grup ~ Moment)

# Testing symmetry and excess
sasLM::Skewness(ds$Stress[ds$Moment == "Initial" & ds$Grup == "Kitaro first"])
sasLM::SkewnessSE(ds$Stress[ds$Moment == "Initial" & ds$Grup == "Kitaro first"])
sasLM::Kurtosis(ds$Stress[ds$Moment == "Initial" & ds$Grup == "Kitaro first"])
sasLM::KurtosisSE(ds$Stress[ds$Moment == "Initial" & ds$Grup == "Kitaro first"])

