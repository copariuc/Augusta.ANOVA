# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
library(rstatix); library(ggpubr)

# III. Assess One-Way ANOVA Model ####
owa <- ds %>% 
  anova_test(formula = Punctaj ~ Grup,
             effect.size = "ges", type = 1, detailed = T); owa

## Homogeneity of variance not fulfilled
owa.nh <- ds %>% 
  welch_anova_test(formula = Punctaj ~ Grup); owa.nh

## Assumptions not fulfilled - Non-parametric approach
kwt <- ds %>% 
  kruskal_test(formula = Punctaj ~ Grup); kwt
kwt.es <- ds %>% 
  kruskal_effsize(formula = Punctaj ~ Grup); kwt.es

# IV. Post-hoc tests ####
pht <- ds %>% tukey_hsd(Punctaj ~ Grup); pht

## Homogeneity of variance not fulfilled
ptt <- ds %>% pairwise_t_test(Punctaj ~ Grup, p.adjust.method = "bonferroni"); ptt
ght <- ds %>% games_howell_test(Punctaj ~ Grup, detailed = T); ght

## Assumptions not fulfilled - Non-parametric approach
dun <- ds %>% 
  dunn_test(formula = Punctaj ~ Grup, p.adjust.method = "bonferroni"); dun
wil <- ds %>% 
  wilcox_test(formula = Punctaj ~ Grup, p.adjust.method = "bonferroni"); wil

## Updating boxplot
pht <- pht %>% add_xy_position(x = "Grup")
grp <- grp +
  stat_pvalue_manual(pht, hide.ns = TRUE) +
  labs(subtitle = get_test_label(owa, detailed = TRUE),
    caption = get_pwc_label(pht)); grp


