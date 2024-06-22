# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
library(rstatix); library(ggpubr)

# Loading dataset
load(file = "Date 3.RData"); names(ds)

# I. Descriptive analysis and visualization system ####
dsc <- ds %>% 
  group_by(Gen, Educatie) %>% 
  get_summary_stats(Apreciere, type = "full"); dsc
  
grp.1 <- ggboxplot(data = ds, y = "Apreciere", x = "Gen", color = "Educatie",
                 bxp.errorbar = TRUE, orientation = "vertical",
                 ylab = "Punctaj apreciere (0-10)", 
                 xlab = FALSE, add = c("mean", "jitter"),
                 ggtheme = theme_pubr(), palette = "jco"); grp.1

grp.2 <- ggboxplot(data = ds, y = "Apreciere", x = "Educatie", color = "Gen",
                   bxp.errorbar = TRUE, orientation = "vertical",
                   ylab = "Punctaj apreciere (0-10)", 
                   xlab = FALSE, add = c("mean", "jitter"),
                   ggtheme = theme_pubr(), palette = "jco"); grp.2

# II. Assumption analysis ####
ds %>% 
  group_by(Gen, Educatie) %>% 
  identify_outliers(Apreciere)

ds %>%
  group_by(Gen, Educatie) %>% 
  shapiro_test(Apreciere)
ggqqplot(data = ds, x = "Apreciere", color = "dark green",
         xlab = "Distributia normala teoretica", ylab = "Distributia aprecierii") +
  facet_grid(Gen ~Educatie)

ds %>% levene_test(Apreciere ~ Gen * Educatie)


