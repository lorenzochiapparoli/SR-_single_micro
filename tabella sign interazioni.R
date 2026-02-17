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

Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 2) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)

#ANCOVA 

ancova_ac <- aov(arc_mck ~ Micro*Trial+Acetic+EtOH+`G+F`, data = Df)
summary(ancova_ac)


emm <- emmeans(ancova_ac, ~ Micro * Trial)

# Marginalizza su Trial per ottenere una stima per Micro
emm_micro <- emmeans(ancova_ac, ~ Micro | Trial)
emm_micro_avg <- emmeans(ancova_ac, ~ Micro)

# Test di contrasto con lettere:UNA LETTERA PER MICRO CHE DESCRIVE L'INTERAZIONE
pairs_result <- multcomp::cld(emm_micro_avg, 
                              Letters = letters, 
                              adjust = "none")  # "none" = equivalente a LSD
print(pairs_result)

######################################################################
# TABELLA CON LETTERE SEPARATAMENTE PER TRIAL

emm <- emmeans(ancova_ac, ~ Micro | Trial)
cld_result <- cld(emm, Letters = letters, adjust = "none")

# Converti in dataframe
df_cld <- as.data.frame(cld_result)

# Pulisci le lettere (rimuovi spazi)
df_cld$.group <- trimws(df_cld$.group)

# Crea colonna "media (lettera)"
df_cld$label <- paste0(round(df_cld$emmean, 2), " (", df_cld$.group, ")")

# Pivot in formato wide: righe = Micro, colonne = Trial
tabella <- df_cld %>%
  dplyr::select(Micro, Trial, label) %>%
  pivot_wider(names_from = Trial, values_from = label)

print(tabella)

###########################################################################################################
#VARIABILITà DELLE EMMEANS 

emm <- emmeans(ancova_ac, ~ Micro * Trial)
emm_df <- as.data.frame(emm)

# Per ogni Micro, calcola la variabilità delle emmeans attraverso i Trial
variabilita_emm <- emm_df %>%
  group_by(Micro) %>%
  summarise(
    sd_emmeans = sd(emmean),
    range_emmeans = max(emmean) - min(emmean)
  ) %>%
  arrange(desc(sd_emmeans))

print(variabilita_emm)




library(emmeans)
library(multcomp)
library(dplyr)
library(tidyr)
library(agricolae)

# 1. Emmeans con lettere per Trial dentro ogni Micro
emm <- emmeans(ancova_ac, ~ Trial | Micro)
cld_result <- cld(emm, Letters = letters, adjust = "none")
cld_df <- as.data.frame(cld_result)
cld_df$.group <- trimws(cld_df$.group)
cld_df$label <- paste0(round(cld_df$emmean, 2))

# 2. Pivot wide
tabella_wide <- cld_df %>%
  dplyr::select(Micro, Trial, label) %>%
  tidyr::pivot_wider(names_from = Trial, values_from = label)

# 3. Variabilità per ogni Micro attraverso i Trial
variabilita <- as.data.frame(cld_result) %>%
  group_by(Micro) %>%
  summarise(
    sd_emmeans = sd(emmean),
    range_emmeans = max(emmean) - min(emmean),
    .groups = "drop"
  )

# 4. ANOVA per Trial dentro ogni Micro + significatività
sign_by_micro <- Df %>%
  group_by(Micro) %>%
  group_modify(~ tidy(aov(arc_mck ~ Trial, data = .x))) %>%
  filter(term == "Trial") %>%
  dplyr::select(Micro, statistic, p.value) %>%
  mutate(p.adj = p.adjust(p.value, method = "fdr"))

lsd_lettere <- sign_by_micro %>%
  mutate(
    significatività = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE          ~ "ns"
    )
  ) %>%
  dplyr::select(Micro, significatività)


# 6. Unisci tutto
tabella_finale <- tabella_wide %>%
  left_join(variabilita, by = "Micro") %>%
  left_join(sign_by_micro %>% dplyr::select(Micro, statistic, p.value), by = "Micro") %>%
  left_join(lsd_lettere %>% dplyr::select(Micro, significatività), by = "Micro") %>%
  mutate(
    sd_emmeans = round(sd_emmeans, 2),
    range_emmeans = round(range_emmeans, 2),
    statistic = round(statistic, 2),
    p.value = round(p.value, 4)
  ) %>%
  arrange(desc(sd_emmeans))

print(tabella_finale)







library(car)
library(dplyr)

levene_by_micro <- Df %>%
  group_by(Micro) %>%
  group_modify(~ {
    test <- leveneTest(arc_mck ~ as.factor(Trial), data = .x)
    data.frame(
      F = round(test$`F value`[1], 2),
      p.value = round(test$`Pr(>F)`[1], 4)
    )
  }) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ "ns"
    )
  ) %>%
  arrange(p.value)

print(levene_by_micro)



cv_micro_trial <- Df %>%
  group_by(Micro, Trial) %>%
  summarise(
    media = mean(arc_mck, na.rm = TRUE),
    sd = sd(arc_mck, na.rm = TRUE),
    CV = round((sd / media) * 100, 2),
    .groups = "drop"
  )

# CV medio per Micro attraverso i Trial
cv_micro <- cv_micro_trial %>%
  group_by(Micro) %>%
  summarise(
    CV_mean = round(mean(CV), 2),
    CV_sd = round(sd(CV), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(CV_mean))

print(cv_micro)







