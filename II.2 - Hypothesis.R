# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
library(rstatix); library(ggpubr)

# IV. Assess One-Way RM ANOVA Model ####
owrma <- ds %>% 
  anova_test(dv = Wellbeeing, within = Moment, wid = ID,
             effect.size = "ges", type = 3, detailed = T); owrma

## Assumptions not fulfilled - Non-parametric approach
fri <- ds %>% 
  friedman_test(formula = Wellbeeing ~ Moment | ID); fri
fri.es <- ds %>% 
  friedman_effsize(formula = Wellbeeing ~ Moment | ID); fri.es

# V. Post-hoc tests ####
pht <- ds %>% pairwise_t_test(formula = Wellbeeing ~ Moment, 
                              paired = TRUE, p.adjust.method = "bonferroni"); pht

## Assumptions not fulfilled - Non-parametric approach
wil <- ds %>% 
  wilcox_test(formula = Wellbeeing ~ Moment, paired = TRUE,
              p.adjust.method = "bonferroni"); wil

sig <- ds %>% 
  sign_test(formula = Wellbeeing ~ Moment, p.adjust.method = "bonferroni"); sig

## Updating boxplot
pht <- pht %>% add_xy_position(x = "Moment")
grp <- grp +
  stat_pvalue_manual(pht, hide.ns = TRUE) +
  labs(subtitle = get_test_label(owrma, detailed = TRUE),
       caption = get_pwc_label(pht)); grp
