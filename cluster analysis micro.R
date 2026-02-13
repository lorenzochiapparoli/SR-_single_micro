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



Df_secv_wide <- Df_sev %>% 
  pivot_wider(names_from = Trial,
              values_from = arc_mck)

means_df <- Df_secv_wide %>%
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

anova_sev <- aov(arc_mck ~ Micro, data = Df)
summary(anova_sev)
lsd_sev <- LSD.test(anova_sev, "Micro")
df_lsd <- as.data.frame(lsd_sev$groups)
df_lsd <- rownames_to_column(df_lsd, var = "Micro")
Df$Micro <- factor(Df$Micro, 
                   levels = df_lsd$Micro[order(df_lsd$groups)])


p <- ggplot(Df, aes(x = Micro, y = arc_mck)) +
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
  aes(x = Micro, y = arc_mck + 15, label = groups),  # 0.5 = offset sopra box
  inherit.aes = FALSE,   # importante, non ereditare y del ggplot principale
  size = 5
)
p

