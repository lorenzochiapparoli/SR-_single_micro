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

ancova_ac <- aov(arc_mck ~ Micro*Temperatura_cat+Acetic_NT+GF_NT+EtOH_NT, data = Df)
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
library(dplyr)
library(tidyr)
library(agricolae)
library(broom)

### 1️⃣ ANOVA + LSD per ogni Micro
lsd_results <- Df %>%
  group_by(Micro) %>%
  group_modify(~{
    
    # ANOVA
    model <- aov(arc_mck ~ Trial+Acetic+EtOH+`G+F`, data = .x)
    anova_tab <- broom::tidy(model)
    p_anova <- anova_tab$p.value[anova_tab$term == "Trial"]
    
    # Medie
    means <- .x %>%
      group_by(Trial) %>%
      summarise(medie = mean(arc_mck, na.rm = TRUE),
                .groups = "drop")
    
    # Se significativo → fai LSD
    if (p_anova <= 0.05) {
      
      lsd <- LSD.test(model, "Trial", p.adj = "none")
      letters_df <- as.data.frame(lsd$groups)
      letters_df$Trial <- rownames(letters_df)
      
      final <- means %>%
        left_join(letters_df[, c("Trial", "groups")], by = "Trial") %>%
        mutate(
          label = paste0(round(medie, 2), " (", groups, ")")
        )
      
    } else {
      
      final <- means %>%
        mutate(
          label = paste0(round(medie, 2))
        )
    }
    
    final
  }) %>%
  ungroup()


### 2️⃣ Pivot wide
tabella_wide <- lsd_results %>%
  dplyr::select(Micro, Trial, label) %>%
  pivot_wider(names_from = Trial, values_from = label)

### 3️⃣ Variabilità
variabilita <- lsd_results %>%
  group_by(Micro) %>%
  summarise(
    sd = sd(medie),
    ranges = max(medie) - min(medie),
    .groups = "drop"
  )

### 4️⃣ P-value ANOVA per ogni Micro
sign_by_micro <- Df %>%
  group_by(Micro) %>%
  group_modify(~ tidy(aov(arc_mck ~ Trial+Acetic+EtOH+`G+F`, data = .x))) %>%
  filter(term == "Trial") %>%
  dplyr::select(Micro, statistic, p.value)

### 5️⃣ Unisci tutto
tabella_finale <- tabella_wide %>%
  left_join(variabilita, by = "Micro") %>%
  left_join(sign_by_micro, by = "Micro") %>%
  mutate(
    sd = round(sd, 2),
    ranges = round(ranges, 2),
    statistic = round(statistic, 2),
    p.value = formatC(p.value, format = "e", digits = 2)
  ) %>%
  arrange(desc(sd))

print(tabella_finale)

tabella_finale <- tabella_wide %>%
  left_join(variabilita, by = "Micro") %>%
  left_join(sign_by_micro, by = "Micro") %>%
  mutate(
    significatività = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    sd = round(sd, 2),
    ranges = round(ranges, 2),
    statistic = round(statistic, 2),
    p.value = formatC(p.value, format = "e", digits = 2)
  ) %>%
  arrange(desc(sd))

print(tabella_finale)

library(knitr)
library(kableExtra)

# Supponiamo che tabella_finale sia la tua tabella
tabella_finale %>%
  kable(format = "html", caption = "Tabella finale") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))
