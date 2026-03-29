library(tidyr)
library(dplyr)
library(broom)  # per tidy() dei risultati dei test
library(readxl)
library(writexl)
library(ggplot2)
library(openxlsx)
library(emmeans)
library(multcomp)
library(multcompView)
library(agricolae)
library(kableExtra)

Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 3) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)
Df$Temperatura_cat <- factor(Df$Temperatura_cat)

#ANCOVA 

ancova_ac <- aov(arc_mck ~ Micro*Temperatura_cat+ Acetic_NT + GF_NT + EtOH_NT+pH_NT+Ac_tot_NT, data = Df)
summary(ancova_ac)

ancova_gf <- aov(arc_mck ~ Temperatura_cat*GF_NT, data = Df)
summary(ancova_gf)
lsd_gf <- LSD.test(ancova_gf, c("Temperatura_cat"), alpha = 0.05)

## TABELLA arc_mck vs Temp_cat 

unique_micros <- unique(Df$Micro)

for (m in unique_micros) {
  
  cat("\n==============================\n")
  cat("Micro:", m, "\n")
  cat("==============================\n\n")
  
  data_micro <- Df %>% filter(Micro == m)
  
  model <- aov(arc_mck ~   Temperatura_cat + Acetic_NT + GF_NT + EtOH_NT +pH_NT+Ac_tot_NT, data = data_micro)
  
  print(summary(model))
}


tabella_output <- Df %>%
  group_by(Micro) %>%
  do(tidy(aov(arc_mck ~ Temperatura_cat + Acetic_NT + GF_NT + EtOH_NT+pH_NT+Ac_tot_NT, data = .))) %>%
  ungroup() %>%
  filter(term != "Residuals") %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    )
  ) %>%
  mutate(across(where(is.numeric), round, 4))

tabella_output %>%
  dplyr::select(Micro, term, df, sumsq, statistic, p.value, stars) %>%
  kable(
    caption = "ANOVA per Micro",
    align = "c",
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )


for (m in unique_micros) {
  
  cat("\n==============================\n")
  cat("Micro:", m, "\n")
  cat("==============================\n\n")
  
  data_micro <- Df %>% filter(Micro == m)
  
  model <- aov(arc_mck ~  Acetic_NT + GF_NT + EtOH_NT + Temperatura_cat, 
               data = data_micro)
  
  # ---- Tabella ANOVA ----
  anova_df <- as.data.frame(anova(model))
  anova_df$Term <- rownames(anova_df)
  rownames(anova_df) <- NULL
  anova_df <- anova_df[anova_df$Term != "Residuals", ]
  
  # ---- SS totale ----
  ss_total <- sum(anova_df$`Sum Sq`)
  
  # ---- Varianza spiegata (SS_i / SS_totale) ----
  anova_df$Varianza_spiegata <- (anova_df$`Sum Sq` / ss_total)*100
  
  
  
  # ---- Arrotondamento a 2 cifre significative ----
  anova_df$`Sum Sq` <- signif(anova_df$`Sum Sq`, 2)
  anova_df$`Mean Sq` <- signif(anova_df$`Mean Sq`, 2)
  anova_df$`F value` <- signif(anova_df$`F value`, 2)
  anova_df$`Pr(>F)` <- signif(anova_df$`Pr(>F)`, 2)
  anova_df$Varianza_spiegata <- signif(anova_df$Varianza_spiegata, 2)

  # ---- Metto Term come prima colonna ----
  anova_df <- anova_df[, c("Term", setdiff(names(anova_df), "Term"))]
  
  print(anova_df)
}

Df_sm <- Df %>% 
  filter(Micro == "T. delbrueckii")
mlcz <- aov(arc_mck ~  Temperatura_cat + Acetic_NT + GF_NT + EtOH_NT+pH_NT+Ac_tot_NT, data = Df_sm)
summary(mlcz)
