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

unique_micros <- unique(Df$Micro)

for (m in unique_micros) {
  
  cat("\n==============================\n")
  cat("Micro:", m, "\n")
  cat("==============================\n\n")
  
  data_micro <- Df %>% filter(Micro == m)
  
  model <- lm(Acetic ~ arc_mck, data = data_micro)
  
  print(summary(model))
}

tabella_output <- Df %>%
  group_by(Micro) %>%
  do({
    model <- lm(EtOH ~ arc_mck, data = .)
    tidy_model <- tidy(model) %>%
      filter(term != "(Intercept)")  # togli l'intercetta
    # aggiungi R squared
    r2 <- summary(model)$r.squared
    tidy_model <- tidy_model %>%
      mutate(R.squared = round(r2, 4))
    tidy_model
  }) %>%
  ungroup() %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    )
  ) %>%
  mutate(across(where(is.numeric), round, 4))  # arrotonda tutti i numeri

tabella_output %>%
  dplyr::select(Micro, term, estimate, std.error, statistic, p.value, stars, R.squared) %>%
  kable(
    caption = "ANOVA per Micro con R²",
    align = "c",
    booktabs = TRUE
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )

aov_glucosio <- aov(`G+F` ~ Micro, data= Df)
summary(aov_glucosio)

aov_acetic <- aov(Acetic ~ arc_mck, data=Df)
summary(aov_acetic)
