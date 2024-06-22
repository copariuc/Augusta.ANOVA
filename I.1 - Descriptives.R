# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
library(rstatix); library(ggpubr)

# Loading dataset
load(file = "Date 1.RData"); names(ds)

# I. Descriptive analysis and visualization system ####
dsc <- ds %>% 
  group_by(Grup) %>% 
  get_summary_stats(Punctaj, type = "full"); dsc

grp <- ggboxplot(data = ds, x = "Grup", y = "Punctaj", fill = "Grup",
                 bxp.errorbar = TRUE, orientation = "vertical",
                 ylab = "Punctajul obtinut (0-100 puncte)", 
                 xlab = FALSE, add = c("mean", "jitter"),
                 ggtheme = theme_pubr(), palette = "jco"); grp

# II. Assumption analysis ####
ds %>% 
  group_by(Grup) %>% 
  identify_outliers(Punctaj)
ds %>%
  group_by(Grup) %>% 
  shapiro_test(Punctaj)
ggqqplot(data = ds, x = "Punctaj", color = "dark green", facet.by = "Grup",
         xlab = "Distributia normala teoretica", ylab = "Distributia punctajului (0-100)")
ds %>% levene_test(Punctaj ~ Grup)
