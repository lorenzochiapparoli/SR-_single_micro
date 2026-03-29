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
library(tibble)



Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 2) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)

Df_sev <- Df %>% 
  dplyr:: select(c("Trial", "Micro", "Replica", "arc_mck"))



Df_sev_wide <- Df_sev %>% 
  pivot_wider(names_from = Trial,
              values_from = arc_mck)

means_df <- Df_sev_wide %>%
  group_by(Micro) %>%
  summarise(
    T1 = mean(`1`, na.rm = TRUE),
    T2 = mean(`2`, na.rm = TRUE),
    T3 = mean(`3`, na.rm = TRUE),
    T4 = mean(`4`, na.rm = TRUE),
    .groups = "drop"
  )

mat <- as.matrix(means_df[,-1])
rownames(mat) <- means_df$Micro

mat_scaled <- scale(mat)
hc <- hclust(dist(mat_scaled), method="ward.D2")
plot(hc, horiz = TRUE)