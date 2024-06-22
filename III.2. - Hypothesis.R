# Installing and loading packages
if(!require(rstatix)) install.packages("rstatix")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(emmeans)) install.packages("emmeans")
library(rstatix); library(ggpubr); library(emmeans)

# IV. Assess Two-Way ANOVA Model ####
twa <- ds %>% 
  anova_test(formula = Apreciere ~ Gen * Educatie,
             effect.size = "ges", type = 2, detailed = T); twa

# V. Simple main effect of education ####
sme.edu <- ds %>% 
  group_by(Gen) %>% 
  anova_test(formula = Apreciere ~ Educatie, effect.size = "ges", type = 2, detailed = T,
             error = lm(data = ds,
                        formula = Apreciere ~ Gen * Educatie)); sme.edu
## Post-hoc tests
pht <- ds %>%
  group_by(Gen) %>% 
  pairwise_t_test(formula = Apreciere ~ Educatie, 
                  p.adjust.method = "bonferroni"); pht
pht <- ds %>%
  group_by(Gen) %>% 
  emmeans_test(formula = Apreciere ~ Educatie,  detailed = T,
                  p.adjust.method = "bonferroni",
               model= lm(data = ds,
                    formula = Apreciere ~ Gen * Educatie)); pht 
# Updating boxplot
pht <- pht %>% add_xy_position(x = "Gen")
grp.1 <- grp.1 +
  stat_pvalue_manual(pht, hide.ns = FALSE) +
  labs(subtitle = get_test_label(twa, detailed = TRUE),
       caption = get_pwc_label(pht)); grp.1

## Interaction plot
interaction.plot(response= ds$Apreciere,
                 x.factor = ds$Gen, trace.factor = ds$Educatie,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"), pch=c(19, 17, 15),
                 fixed=TRUE, leg.bty = "o",
                 xlab = "Genul biologic", ylab = "Medii ale aprecierii", 
                 trace.label = "Nivel de educatie")

# V. Simple main effect of gender ####
sme.edu <- ds %>% 
  group_by(Educatie) %>% 
  anova_test(formula = Apreciere ~ Gen, effect.size = "ges", type = 2, detailed = T,
             error = lm(data = ds,
                        formula = Apreciere ~ Gen * Educatie)); sme.edu
## Post-hoc tests
pht <- ds %>%
  group_by(Educatie) %>% 
  pairwise_t_test(formula = Apreciere ~ Gen, 
                  p.adjust.method = "bonferroni"); pht
pht <- ds %>%
  group_by(Educatie) %>% 
  emmeans_test(formula = Apreciere ~ Gen,  detailed = T,
               p.adjust.method = "bonferroni",
               model= lm(data = ds,
                         formula = Apreciere ~ Gen * Educatie)); pht 
# Updating boxplot
pht <- pht %>% add_xy_position(x = "Educatie")
grp.2 <- grp.2 +
  stat_pvalue_manual(pht, hide.ns = TRUE) +
  labs(subtitle = get_test_label(twa, detailed = TRUE),
       caption = get_pwc_label(pht)); grp.2

## Interaction plot
interaction.plot(response= ds$Apreciere,
                 x.factor = ds$Educatie, trace.factor = ds$Gen,
                 fun = mean,
                 type="b",
                 col=c("red","green"), pch=c(17, 15),
                 fixed=TRUE, leg.bty = "o",
                 xlab = "Nivel de educatie", ylab = "Medii ale aprecierii", 
                 trace.label = "Gen biologic")

