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

Df <- read_excel("dataset microorganismi singoli.xlsx", sheet = 2) 
Df$Trial <- factor(Df$Trial)
Df$Micro <- factor(Df$Micro)
#ANOVA 

anova_mck <- aov(arc_mck ~ Micro*Trial, data = Df)
summary(anova_mck)


#ANCOVA 

ancova_ac <- aov(arc_mck ~ Micro*Trial+Acetic+EtOH+`G+F`, data = Df)

summary(ancova_ac)
anova(ancova_ac)

emm <- emmeans(ancova_ac, ~ Trial | Micro)
df_emm <- as.data.frame(emm)

pairs(emmeans(ancova_ac, ~ Trial | Micro))


####################################### TABELLA varianza spiegata NO RESIDUI

aov_sum <- summary(ancova_ac)[[1]]
resid_rows <- grep("Residuals", rownames(aov_sum), ignore.case = TRUE)
aov_sum_no_resid <- aov_sum[-resid_rows, ]
SS_factors <- sum(aov_sum_no_resid$`Sum Sq`)
aov_sum_no_resid$VarianzaSpiegata <- aov_sum_no_resid$`Sum Sq` / SS_factors * 100
aov_sum_no_resid
aov_sum_no_resid <- as.data.frame(aov_sum_no_resid)
aov_sum_no_resid


Df$arc_mck_adj <- predict(
  ancova_ac,
  newdata = transform(
    Df,
    Acetic = mean(Acetic, na.rm = TRUE),
    EtOH   = mean(EtOH,   na.rm = TRUE),
    `G+F`  = mean(`G+F`,  na.rm = TRUE))
  )


cov_model <- aov(arc_mck ~ Acetic + EtOH + `G+F`, data = Df)
summary(cov_model)
Df$arc_mck_ripulito <- residuals(cov_model) + mean(Df$arc_mck)
anova_rip <- aov(arc_mck_ripulito ~ Micro*Trial, data = Df)
summary(anova_rip)


ancova_xl <- lm(
  arc_mck ~ Micro + Trial + Acetic + EtOH + `G+F`,
  data = Df
)

emm <- emmeans(ancova_ac, ~ Trial | Micro)
pw  <- pairs(emm, adjust = "tukey")



plot(pw)
plot(emm)
plot(emm, comparisons = TRUE)
emmip(ancova_ac, Micro ~ Trial, CIs = TRUE)

summary(pw)


# 2. Confronti pairwise Tukey
emm_contr <- contrast(emm, method = "pairwise", adjust = "tukey")
contr_df <- summary(emm_contr)

# 3. Lista di lettere per Micro
letters_list <- list()
for(m in unique(contr_df$Micro)) {
  sub <- contr_df[contr_df$Micro == m, ]
  
  # crea p-value con nomi dei confronti
  pvals_named <- sub$p.value
  names(pvals_named) <- sub$contrast
  
  # genera lettere
  letters_list[[m]] <- multcompLetters(pvals_named, threshold = 0.05)$Letters
}

# 4. Tabella compatta
emm_df <- as.data.frame(emm)
emm_df$Letter <- NA

for(m in unique(emm_df$Micro)) {
  let <- letters_list[[m]]
  
  # estrai Trials del Micro
  trials_micro <- unique(emm_df$Trial[emm_df$Micro == m])
  
  # assegna lettere a ciascun Trial usando match
  for(tr in trials_micro) {
    # trova tutte le lettere dei confronti che includono questo Trial
    relevant <- sapply(names(let), function(x) grepl(tr, x))
    # assegna la prima lettera trovata (multcompLetters garantisce coerenza)
    emm_df$Letter[emm_df$Micro == m & emm_df$Trial == tr] <- let[which(relevant)[1]]
  }
}

# 5. Visualizza
print(emm_df[, c("Micro", "Trial", "emmean", "SE", "Letter")])

ggplot(emm_df, aes(x = Trial, y = emmean, fill = Trial)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  geom_text(aes(label = Letter, y = emmean + SE + 0.05 * max(emmean)), 
            position = position_dodge(width = 0.9), vjust = 0) +
  facet_wrap(~ Micro, scales = "free_y") +
  labs(y = "Media stimata", x = "Trial") +
  theme_bw() +
  theme(legend.position = "none")


emm_df <- emm_df %>% arrange(Trial,Micro)

# ---- 6. Visualizza tabella compatta ----
tabella_pub <- emm_df %>% 
  dplyr::select(Micro, Trial, emmean, SE, Letter)

print(tabella_pub)

###write_xlsx(tabella_pub, "tukey_medie_corrette.xlsx")

##########################################################################################################
df_emm_sev <- df_emm %>% 
  dplyr:: select(c("Trial", "Micro","emmean"))


df_emm_sev_wide <- df_emm_sev %>% 
  pivot_wider(names_from = Trial,
              values_from = emmean)

means_df_emm <- df_emm_sev_wide %>%
  group_by(Micro) %>%
  summarise(
    T1 = mean(`1`, na.rm = TRUE),
    T2 = mean(`2`, na.rm = TRUE),
    T3 = mean(`3`, na.rm = TRUE),
    T4 = mean(`4`, na.rm = TRUE),
    .groups = "drop"
  )

mat <- as.matrix(means_df_emm[,-1])
rownames(mat) <- means_df_emm$Micro

mat_scaled <- scale(mat)
hc <- hclust(dist(mat_scaled), method="ward.D2")
plot(hc, horiz = TRUE)

lsd_sev <- LSD.test(ancova_ac, "Micro")
df_lsd <- as.data.frame(lsd_sev$groups)
df_lsd <- rownames_to_column(df_lsd, var = "Micro")

medie_ordine <- df_emm_sev %>% 
  group_by(Micro) %>% 
  summarise(media_sev = mean(emmean)) %>% 
  arrange(desc(media_sev))



df_emm_sev$Micro <- factor(df_emm_sev$Micro, 
                   levels = medie_ordine$Micro[order(medie_ordine$media_sev, decreasing = TRUE)])


p <- ggplot(df_emm_sev, aes(x = Micro, y = emmean)) +
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
    ))
  
p
