library(tidyr)
library(dplyr)
library(broom)  # per tidy() dei risultati dei test
library(readxl)
library(writexl)
library(ggplot2)
library(bestNormalize)
library(agricolae)  # per LSD test
library(ggrepel)
library(htmltools)
library(car)
library(effectsize)

Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 2) %>% 
  filter(!(Micro %in% c("NT Y", "NT AAB", "NT")))

Df$Micro <- as.factor(Df$Micro)
levels(Df$Micro)

anova_sev <- aov(arc_mck ~ Micro*Trial, data = Df)
summary(anova_sev)

anova_ac <- aov(Acetic ~ Micro*Trial, data = Df)
summary(anova_ac)

anova_et <- aov(EtOH ~ Micro*Trial, data = Df)
summary(anova_et)

anova_gf <- aov(`G+F` ~ Micro*Trial, data = Df)
summary(anova_gf)

anova_ph <- aov(pH ~ Micro*Trial, data = Df)
summary(anova_ph)

Df_long <- Df %>%
  pivot_longer(
    cols = -c(Trial, Micro, Replica),
    names_to = "Parametro",
    values_to = "Valore"
  )


df_clean <- Df_long %>%
  filter(is.finite(Valore)) %>%   # elimina NA o Inf in Valore
  droplevels()                     # elimina eventuali livelli fattore non usati


df_clean$Trial <- as.factor(df_clean$Trial)
df_clean$Parametro <- as.factor(df_clean$Parametro)

options(contrasts = c("contr.sum", "contr.poly"))



# Estrai i livelli del fattore Parametro

lista_risultati <- list()

livelli_parametro <- levels(df_clean$Parametro)

for (p in livelli_parametro) {
  sub <- subset(df_clean, Parametro == p)
  mod <- aov(Valore ~ Micro * Trial, data = sub)
  mod_sum <- summary(mod)[[1]]
  
  # Rimuove i residui
  mod_eff <- mod_sum[-nrow(mod_sum), ]
  
  # Percentuale di varianza spiegata
  mod_eff$Perc_var_spiegata <- mod_eff[["Sum Sq"]] / sum(mod_eff[["Sum Sq"]]) * 100
  
  # Salva in lista con il nome del parametro
  lista_risultati[[p]] <- mod_eff
}

# Esporta in Excel (un foglio per ciascun parametro)
#write_xlsx(lista_risultati, path = "risultati_ANOVA_trial.xlsx")

ancova_ac <- aov(arc_mck ~ Micro*Trial+Acetic+EtOH+`G+F`+ Temperatura, data = Df)
summary(ancova_ac)

Df$arc_mck_adj <- predict(
  ancova_aov_ac,
  newdata = transform(
    Df,
    Acetic = mean(Acetic, na.rm = TRUE),
    EtOH   = mean(EtOH,   na.rm = TRUE),
    `G+F`  = mean(`G+F`,  na.rm = TRUE)
  )
)
