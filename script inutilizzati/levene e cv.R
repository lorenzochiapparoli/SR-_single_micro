library(car)
library(dplyr)
library(tidyr)
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
cv_micro <- Df %>%
  group_by(Micro) %>%
  summarise(
    media = mean(arc_mck, na.rm = TRUE),
    sd = sd(arc_mck, na.rm = TRUE),
    CV = round((sd / media) * 100, 2),
    .groups = "drop"
  ) %>% 
  arrange(CV)

print(cv_micro)