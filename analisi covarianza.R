library(tidyr)
library(dplyr)
library(broom)  # per tidy() dei risultati dei test
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)
library(emmeans)

Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 2) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)
#ANOVA 

anova_mck <- aov(arc_mck ~ Micro*Trial, data = Df)
summary(anova_mck)


#ANCOVA 

ancova_ac <- aov(arc_mck ~ Micro*Trial+Acetic+EtOH+`G+F`, data = Df)
summary(ancova_ac)
anova(ancova_ac)

emm <- emmeans(ancova_ac, ~ Trial | Micro)
df_emm <- as.data.frame(emm)

pairs(emmeans(ancova_ac, ~ Trial | Micro))


####################################### TABELLA varianza spiegata NO RESIDUI

aov_sum <- summary(ancova_ac)[[1]]
resid_rows <- grep("Residuals", rownames(aov_sum), ignore.case = TRUE)
aov_sum_no_resid <- aov_sum[-resid_rows, ]
SS_factors <- sum(aov_sum_no_resid$`Sum Sq`)
aov_sum_no_resid$VarianzaSpiegata <- aov_sum_no_resid$`Sum Sq` / SS_factors * 100
aov_sum_no_resid
aov_sum_no_resid <- as.data.frame(aov_sum_no_resid)
aov_sum_no_resid


Df$arc_mck_adj <- predict(
  ancova_ac,
  newdata = transform(
    Df,
    Acetic = mean(Acetic, na.rm = TRUE),
    EtOH   = mean(EtOH,   na.rm = TRUE),
    `G+F`  = mean(`G+F`,  na.rm = TRUE))
  )


cov_model <- aov(arc_mck ~ Acetic + EtOH + `G+F`, data = Df)
summary(cov_model)
Df$arc_mck_ripulito <- residuals(cov_model) + mean(Df$arc_mck)
anova_rip <- aov(arc_mck_ripulito ~ Micro*Trial, data = Df)
summary(anova_rip)


ancova_xl <- lm(
  arc_mck ~ Micro + Trial + Acetic + EtOH + `G+F`,
  data = Df
)

emm <- emmeans(ancova_ac, ~ Trial | Micro)
pw  <- pairs(emm, adjust = "lsd")

plot(pw)
plot(emm, comparisons = TRUE)
emmip(ancova_ac, Micro ~ Trial, CIs = TRUE)
