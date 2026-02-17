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

anova_sev <- aov(arc_mck ~ Micro*Trial+Acetic+EtOH+`G+F`, data = Df)
summary(anova_sev)
lsd_sev <- LSD.test(anova_sev, "Micro")
df_lsd <- as.data.frame(lsd_sev$groups)
df_lsd <- rownames_to_column(df_lsd, var = "Micro")
Df$Micro <- factor(Df$Micro, 
                   levels = df_lsd$Micro[order(df_lsd$groups)])



p <- ggplot(Df, aes(x = Micro, y = McKinney)) +
  geom_boxplot(outlier.shape = NA, fill = "grey") +
  geom_jitter(
    aes(color = Trial),
    width = 0.15,
    size = 3.5,
    alpha = 0.6
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,        # X shape (Excel-like)
    size = 2,
    stroke = 0.7,     # thickness of the X
    color = "black"
  ) +
  scale_color_manual(values = c(
    "1" = "orange",
    "2" = "blue",
    "3" = "green",
    "4" = "red"
  )) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1   # allinea meglio quando si ruota
    ))+ geom_text(
  data = df_lsd,
  aes(x = Micro, y = arc_mck + 25, label = groups),  # 0.5 = offset sopra box
  inherit.aes = FALSE,   # importante, non ereditare y del ggplot principale
  size = 5
)
p

df_long <- Df_sev_wide %>%
  mutate(across(`1`:`4`, ~ scale(.)[,1])) %>%   # z-score
  pivot_longer(
    cols = `1`:`4`,
    names_to = "Trial",      # nome nuova colonna (prima era 1-4)
    values_to = "Z_score"   # valori standardizzati
  )   %>%
  mutate(Trial = as.integer(Trial)) %>%   # assicura ordine numerico
  arrange(Micro, Trial)

df_long

Df$Z_score <- na.omit(df_long$Z_score)

anova_sev_z <- aov(Z_score ~ Micro*Trial+Acetic+EtOH+`G+F`, data = Df)
summary(anova_sev_z)
lsd_sev_z <- LSD.test(anova_sev_z, "Micro")
df_lsd_z <- as.data.frame(lsd_sev_z$groups)
df_lsd_z <- rownames_to_column(df_lsd_z, var = "Micro")
Df$Micro <- factor(Df$Micro, 
                   levels = df_lsd_z$Micro[order(df_lsd_z$groups)])


p <- ggplot(Df, aes(x = Micro, y = Z_score)) +
  geom_boxplot(outlier.shape = NA, fill = "grey") +
  geom_jitter(
    aes(color = factor(Trial)),   # 👈 qui
    width = 0.15,
    size = 3.5,
    alpha = 0.6
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 4,
    size = 2,
    stroke = 0.7,
    color = "black"
  ) +
  scale_color_manual(values = c(
    "1" = "orange",
    "2" = "blue",
    "3" = "green",
    "4" = "red"
  )) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  ) +
  geom_text(
    data = df_lsd_z,
    aes(x = Micro, y = Z_score + 1.5, label = groups),
    inherit.aes = FALSE,
    size = 5
  )

p

