# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
library(rstatix); library(ggpubr)

# Loading dataset 
load(file = "Date 2.RData"); names(ds)
colnames(ds) <- c("ID", "Initial", "Final", "Follow")

# I. Converting dataset in a long format and defining factors
ds <- ds %>% 
  gather(key = "Moment", value = "Wellbeeing", Initial, Final, Follow,
         factor_key = TRUE) %>% 
  convert_as_factor(ID, Moment); head(ds)

# II. Descriptive analysis and visualization system ####
dsc <- ds %>% 
  group_by(Moment) %>% 
  get_summary_stats(Wellbeeing, type = "full"); dsc
grp <- ggboxplot(data = ds, x = "Moment", y = "Wellbeeing", fill = "Moment",
                 bxp.errorbar = TRUE, orientation = "vertical",
                 ylab = "Punctajul obtinut - Starea de bine", 
                 xlab = FALSE, add = c("mean", "jitter"),
                 ggtheme = theme_pubr(), palette = "jco"); grp

# III. Assumption analysis ####
ds %>% 
  group_by(Moment) %>% 
  identify_outliers(Wellbeeing)
ds %>%
  group_by(Moment) %>% 
  shapiro_test(Wellbeeing)
ggqqplot(data = ds, x = "Wellbeeing", color = "dark green", facet.by = "Moment",
         xlab = "Distributia normala teoretica", ylab = "Distributia punctajului")


