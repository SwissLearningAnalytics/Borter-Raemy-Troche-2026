library(emmeans)
library(effectsize)
library(psych)
library(car)
library(rstatix)
library(FSA)
library(multcomp)
library(lavaan)
library(tidyverse)
library(openxlsx)

df_merged3 <- read.xlsx("df_merged3.xlsx")

df_merged8 <- df_merged3[, c("FormativZeitFB", "FormativZeit", "FormativLeistung",
                                     "Gruppe", "Vorwissen", "sum_FA",
                                     "SCHLUSS_Note_pruefung", "Probetest_sum","Muhe")]


group_sizes <- c(`1` = 73, `2` = 59, `3` = 122)

###exclude students with NA 
df_merged_full <- df_merged8 %>%
  filter(!is.na(Vorwissen),
         !is.na(sum_FA),
         !is.na(Probetest_sum))

df_numeric <- df_merged_full %>%
  mutate(across(everything(), as.numeric))

PRVorwissen<-df_numeric$Vorwissen/28
PRReasoning<-df_numeric$sum_FA/21
PRProbetest<-df_numeric$Probetest_sum/25

PR<-cbind(PRVorwissen,PRReasoning,PRProbetest)
psych::describe(PR)

aggregate(PRVorwissen ~ df_merged_full$Gruppe, FUN = mean)
aggregate(PRVorwissen ~ df_merged_full$Gruppe, FUN = sd)

aggregate(PRReasoning ~ df_merged_full$Gruppe, FUN = mean)
aggregate(PRReasoning ~ df_merged_full$Gruppe, FUN = sd)

aggregate(PRProbetest ~ df_merged_full$Gruppe, FUN = mean)
aggregate(PRProbetest ~ df_merged_full$Gruppe, FUN = sd)

aggregate(df_merged_full$SCHLUSS_Note_pruefung ~ df_merged_full$Gruppe, FUN = mean)
aggregate(df_merged_full$SCHLUSS_Note_pruefung ~ df_merged_full$Gruppe, FUN = sd)

###prior knowledge
df_merged_full$Gruppe <- factor(df_merged_full$Gruppe)
df_merged_full$Gruppe <- relevel(df_merged_full$Gruppe, ref = "3")
mod <- aov(Vorwissen ~ Gruppe, data = df_merged_full)
summary(mod)
plot(mod, which = 2)
shapiro.test(residuals(mod))
leveneTest(df_merged_full$Vorwissen ~ df_merged_full$Gruppe)
oneway.test(Vorwissen ~ Gruppe, data = df_merged_full, var.equal = FALSE)

##according to our hypothesis
with(df_merged_full, t.test(Vorwissen[Gruppe==3], Vorwissen[Gruppe==1],
                            var.equal=FALSE, alternative="greater"))

with(df_merged_full, t.test(Vorwissen[Gruppe==3], Vorwissen[Gruppe==2],
                            var.equal=FALSE, alternative="greater"))

p <- c(
  with(df_merged_full, t.test(Vorwissen[Gruppe==3], Vorwissen[Gruppe==1],
                              var.equal=FALSE, alternative="greater")$p.value),
  with(df_merged_full, t.test(Vorwissen[Gruppe==3], Vorwissen[Gruppe==2],
                              var.equal=FALSE, alternative="greater")$p.value)
)
p_holm <- p.adjust(p, method = "holm")
p_holm

##exploratory
with(df_merged_full, t.test(Vorwissen[Gruppe==1], Vorwissen[Gruppe==2],
                            var.equal=FALSE))


#library(effectsize)
cohens_d(Vorwissen ~ Gruppe, data = df_merged_full, var.equal = FALSE, ci = NULL)

######reasoning
mod <- aov(sum_FA ~ Gruppe, data = df_merged_full)
summary(mod)

##according to our hypothesis
attach(df_merged_full)

t.test(sum_FA[Gruppe==3], sum_FA[Gruppe==1],
       var.equal=TRUE, alternative="greater")

t.test(sum_FA[Gruppe==3], sum_FA[Gruppe==2],
       var.equal=TRUE, alternative="greater")

p <- c(
  with(df_merged_full, t.test(sum_FA[Gruppe==3], sum_FA[Gruppe==1],
                              var.equal=TRUE, alternative="greater")$p.value),
  with(df_merged_full, t.test(sum_FA[Gruppe==3], sum_FA[Gruppe==2],
                              var.equal=TRUE, alternative="greater")$p.value)
)
p_holm <- p.adjust(p, method = "holm")
p_holm


##exploratory
t.test(
  df_merged_full$sum_FA[df_merged_full$Gruppe == 1],
  df_merged_full$sum_FA[df_merged_full$Gruppe == 2],
  var.equal = TRUE         
)

#effect sizes
em <- emmeans(mod, ~ Gruppe, adjust = "tukey")
eff_size(em, sigma = sigma(mod), edf = df.residual(mod))


#####mock exam
mod <- aov(Probetest_sum ~ Gruppe, data = df_merged_full)
summary(mod)

plot(mod, which = 2)
shapiro.test(residuals(mod))
leveneTest(df_merged_full$Probetest_sum ~ df_merged_full$Gruppe)
hist(df_merged_full$Probetest_sum)

## OVERALL (Welch)
oneway.test(Probetest_sum ~ Gruppe, data = df_merged_full, var.equal = FALSE)


##according to our hypothesis
t.test(df_merged_full$Probetest_sum[Gruppe == 2],
       df_merged_full$Probetest_sum[Gruppe == 3],
       var.equal = FALSE, alternative = "less")

t.test(df_merged_full$Probetest_sum[Gruppe == 2],
       df_merged_full$Probetest_sum[Gruppe == 1],
       var.equal = FALSE, alternative = "less")

p <- c(
  with(df_merged_full, t.test(Probetest_sum[Gruppe==2], Probetest_sum[Gruppe==3],
                              var.equal=FALSE, alternative="less")$p.value),
  with(df_merged_full, t.test(Probetest_sum[Gruppe==2], Probetest_sum[Gruppe==1],
                              var.equal=FALSE, alternative="less")$p.value)
)
p_holm <- p.adjust(p, method = "holm")
p_holm


##exploratory
t.test(
  df_merged_full$Probetest_sum[df_merged_full$Gruppe == 1],
  df_merged_full$Probetest_sum[df_merged_full$Gruppe == 3],
  var.equal = FALSE         
)


cohens_d(Probetest_sum ~ Gruppe, data = df_merged_full,var.equal = FALSE, ci = NULL)


######Grades
aggregate(SCHLUSS_Note_pruefung ~ Gruppe, data = df_merged_full,
          FUN = function(x) c(M = mean(x, na.rm = TRUE),
                              SD = sd(x, na.rm = TRUE)))

mod <- aov(SCHLUSS_Note_pruefung ~ Gruppe, data = df_merged_full)
summary(mod)

plot(mod, which = 2)
shapiro.test(residuals(mod))
leveneTest(df_merged_full$SCHLUSS_Note_pruefung ~ df_merged_full$Gruppe)

##according to our hypothesis
t.test(df_merged_full$SCHLUSS_Note_pruefung[Gruppe == 2],
       df_merged_full$SCHLUSS_Note_pruefung[Gruppe == 3],
       var.equal = TRUE, alternative = "less")

t.test(df_merged_full$SCHLUSS_Note_pruefung[Gruppe == 2],
       df_merged_full$SCHLUSS_Note_pruefung[Gruppe == 1],
       var.equal = TRUE, alternative = "less")


p <- c(
  with(df_merged_full,
       t.test(SCHLUSS_Note_pruefung[Gruppe == 2],
              SCHLUSS_Note_pruefung[Gruppe == 3],
              var.equal = TRUE, alternative = "less")$p.value),  # 2 < 3
  with(df_merged_full,
       t.test(SCHLUSS_Note_pruefung[Gruppe == 2],
              SCHLUSS_Note_pruefung[Gruppe == 1],
              var.equal = TRUE, alternative = "less")$p.value)
)

# Holm-Korrektur
p_holm <- p.adjust(p, method = "holm")
p_holm

##explorativ
t.test(
  df_merged_full$SCHLUSS_Note_pruefung[df_merged_full$Gruppe == 1],
  df_merged_full$SCHLUSS_Note_pruefung[df_merged_full$Gruppe == 3],
  var.equal = TRUE          
)

#effect sizes
em <- emmeans(mod, ~ Gruppe, adjust = "tukey")
eff_size(em, sigma = sigma(mod), edf = df.residual(mod))
