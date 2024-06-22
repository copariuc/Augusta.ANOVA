# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
library(rstatix); library(ggpubr)

# III. Assess Two-Way RM ANOVA Model ####
twrma <- ds %>% 
  anova_test(dv = Stress, within = c(Grup, Moment), wid = ID,
             effect.size = "ges", type = 3, detailed = T); twrma

# IV. Simple main effect of moment ####
sme.mom <- ds %>% 
  group_by(Grup) %>% 
  anova_test(dv = Stress, within = Moment, wid = ID,  
             effect.size = "ges", type = 3, detailed = T) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni"); sme.mom

## Post-hoc tests
pht <- ds %>%
  group_by(Grup) %>% 
  pairwise_t_test(formula = Stress ~ Moment, paired = T,
                  p.adjust.method = "bonferroni"); pht

# Updating boxplot
pht <- pht %>% add_xy_position(x = "Moment")
grp.1 <- grp.1 +
  stat_pvalue_manual(pht, hide.ns = FALSE) +
  labs(subtitle = get_test_label(twrma, detailed = TRUE),
       caption = get_pwc_label(pht)); grp.1

## Interaction plot
interaction.plot(response= ds$Stress,
                 x.factor = ds$Grup, trace.factor = ds$Moment,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"), pch=c(19, 17, 15),
                 fixed=TRUE, leg.bty = "o",
                 xlab = "Grup", ylab = "Medii ale stresului", 
                 trace.label = "Momentul colectarii")

# IV. Simple main effect of group ####
sme.grp <- ds %>% 
  group_by(Moment) %>% 
  anova_test(dv = Stress, within = Grup, wid = ID,  
             effect.size = "ges", type = 3, detailed = T) %>% 
  get_anova_table() %>% 
  adjust_pvalue(method = "bonferroni"); sme.grp

## Post-hoc tests
pht <- ds %>%
  group_by(Moment) %>% 
  pairwise_t_test(formula = Stress ~ Grup, paired = T,
                  p.adjust.method = "bonferroni"); pht

# Updating boxplot
pht <- pht %>% add_xy_position(x = "Grup")
grp.2 <- grp.2 +
  stat_pvalue_manual(pht, hide.ns = FALSE) +
  labs(subtitle = get_test_label(twrma, detailed = TRUE),
       caption = get_pwc_label(pht)); grp.2

## Interaction plot
interaction.plot(response= ds$Stress,
                 x.factor = ds$Moment, trace.factor = ds$Grup,
                 fun = mean,
                 type="b",
                 col=c("red","green"), pch=c(17, 15),
                 fixed=TRUE, leg.bty = "o",
                 xlab = "Momentul colectarii", ylab = "Medii ale stresului", 
                 trace.label = "Grupul")
