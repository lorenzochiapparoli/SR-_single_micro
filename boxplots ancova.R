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



Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 3) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)
Df$Temperatura_cat <- factor(Df$Temperatura_cat)

ancova_sev <- aov(arc_mck ~ Micro+ Acetic_NT + GF_NT + EtOH_NT+pH_NT+Ac_tot_NT+Temperatura_cat, data = Df)
summary(ancova_sev)
lsd_sev <- LSD.test(ancova_sev, "Micro", alpha = 0.05)
df_lsd <- as.data.frame(lsd_sev$groups)
df_lsd <- rownames_to_column(df_lsd, var = "Micro")
Df$Micro <- factor(Df$Micro, 
                   levels = df_lsd$Micro[order(df_lsd$groups)])



p <- ggplot(Df, aes(x = Micro, y = arc_mck)) +
  geom_boxplot(outlier.shape = NA, fill = "grey") +
  geom_jitter(
    aes(color = Trial), ### colorare per Trial
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
  )+
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
    )  +
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

######## 
#grafico barre interazione

lsd_sev_int <- LSD.test(ancova_sev, c("Micro", "Temperatura_cat"), alpha = 0.05)
lsd_sev_int

df_lsd_int <- lsd_sev_int$groups
df_lsd_int <- rownames_to_column(df_lsd_int, var = "Micro")

df_lsd_int <- df_lsd_int %>%
  mutate(
    Group = case_when(
      grepl("^T\\. delbrueckii", Micro) ~ "T. delbrueckii",
      grepl("^H\\. uvarum", Micro) ~ "H. uvarum",
      grepl("^C\\. zemplinina", Micro) ~ "C. zemplinina",
      grepl("^P\\. terricola", Micro) ~ "P. terricola",
      grepl("^S\\. vini", Micro) ~ "S. vini",
      grepl("^Z\\. bailii", Micro) ~ "Z. bailii",
      grepl("^G\\. oxydans", Micro) ~ "G. oxydans",
      grepl("^M\\. pulcherrima", Micro) ~ "M. pulcherrima",
      grepl("^P\\. occidentalis", Micro) ~ "P. occidentalis",
      grepl("^A\\. syzygii", Micro) ~ "A. syzygii",
      grepl("^Z\\. hellenicus", Micro) ~ "Z. hellenicus"
    )
  )
df_lsd_int$Group <- as.factor(df_lsd_int$Group)


ggplot(df_lsd_int, aes(x = Micro, y = arc_mck, fill = Group)) +
  geom_col() +
  geom_text(
    aes(x = Micro, y = arc_mck + 1.5, label = groups),
    inherit.aes = FALSE,
    size = 5
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )+
  scale_fill_brewer(palette = "Set3")

emm <- emmeans(ancova_sev, ~ Temperatura_cat | Micro)
df_emm <- as.data.frame(emm)
df_emm$Micro_int <- sort(df_lsd_int$Micro)

ggplot(df_emm, aes(x = Micro_int, y = emmean, fill = Micro)) +
  geom_col() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )+
  scale_fill_brewer(palette = "Set3")

df_emm$emmean

#write_xlsx(df_emm, "Df_emmeans_NT_day5.xlsx")
#write_xlsx(df_lsd_int, "Df_int_micro_day5.xlsx")
