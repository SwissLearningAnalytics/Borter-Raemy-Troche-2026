library(psych)
library(tidyverse)
library(ggplot2)
library(readxl)
library(mice)
library(NbClust)
library(openxlsx)

# read raw data -------------------------------------------------------
df_raw <- read.xlsx("df_raw.xlsx")
all_variables <- read.xlsx("all_variables.xlsx")$all_variables

Pseudo<- df_raw$Pseudo
df_raw_sel<-df_raw[,all_variables]

###Imputation
###only impute formative Assessments
df_raw_sel <- df_raw_sel %>% 
  dplyr::select(-tidyselect::matches("^(T00|T06)"))


# MICE-Algorithmus anwenden
imputed_data <- mice(df_raw_sel, m = 1, maxit = 2, method = "pmm", seed = 245)
  
df_raw_complete <- complete(imputed_data,1)
df_raw_aggregated_filtered_imputed <- cbind(Pseudo,df_raw_complete)

df_raw_aggregated_filtered_imputed_rescaled <- df_raw_aggregated_filtered_imputed |> 
  mutate(across(!Pseudo, ~scale(.)))

##PCA
df_pca <- df_raw_aggregated_filtered_imputed_rescaled %>%
  dplyr::select(-Pseudo)

##For PCA performance no scaling 
df_pca2 <- df_raw_aggregated_filtered_imputed %>%
  dplyr::select(-Pseudo)

variablennamen<-colnames(df_pca)

timeOnFeedback <- variablennamen[grepl("feedback_bz", variablennamen)]
timeExercise <- variablennamen[grepl("_bz", variablennamen) & !grepl("feedback_bz", variablennamen)]

# Performance
leistungVars <- variablennamen[
  !variablennamen %in% c(timeOnFeedback, timeExercise) & 
    !grepl("^T06", variablennamen)
]

#########################time on feedback
feedback_df <- df_pca %>% dplyr::select(all_of(timeOnFeedback))
gruppen <- str_extract(timeOnFeedback, "^T\\d{2}")
gruppen_liste <- split(timeOnFeedback, gruppen)
mittelwerte_pro_termin <- lapply(gruppen_liste, function(spalten) {
  rowMeans(feedback_df[, spalten], na.rm = TRUE)
})
mittelwerte_timeOnFeedback <- as.data.frame(mittelwerte_pro_termin)
names(mittelwerte_timeOnFeedback) <- paste0("mean_", names(mittelwerte_timeOnFeedback), "_timeOnFeedback")
cor(mittelwerte_timeOnFeedback)

#######################time on exercise
exercise_df <- df_pca %>% dplyr::select(all_of(timeExercise))
gruppen_exercise <- stringr::str_extract(timeExercise, "^T\\d{2}")
gruppen_liste_exercise <- split(timeExercise, gruppen_exercise)
mittelwerte_pro_termin_exercise <- lapply(gruppen_liste_exercise, function(spalten) {
  rowMeans(exercise_df[, spalten], na.rm = TRUE)
})

mittelwerte_timeExercise <- as.data.frame(mittelwerte_pro_termin_exercise)

names(mittelwerte_timeExercise) <- paste0("mean_", names(mittelwerte_timeExercise), "_timeExercise")

#########################performance
leistung_df <- df_pca2 %>% dplyr::select(all_of(leistungVars))
gruppen_leistung <- stringr::str_extract(leistungVars, "^T\\d{2}")
gruppen_liste_leistung <- split(leistungVars, gruppen_leistung)
mittelwerte_pro_termin_leistung <- lapply(gruppen_liste_leistung, function(spalten) {
  rowMeans(leistung_df[, spalten], na.rm = TRUE)
})
mittelwerte_leistung <- as.data.frame(mittelwerte_pro_termin_leistung)
names(mittelwerte_leistung) <- paste0("mean_", names(mittelwerte_leistung), "_leistung")
cor(mittelwerte_leistung)
mittelwerte_alle <- cbind(mittelwerte_timeOnFeedback, mittelwerte_timeExercise, mittelwerte_leistung)


#######time on feedback page
pca_result <- prcomp(mittelwerte_timeOnFeedback,center = TRUE, scale. = TRUE)
psych::alpha(mittelwerte_timeOnFeedback)
summary(pca_result)
screeplot(pca_result)
pca_result$rotation[,"PC1"]
FormativZeitFB<-pca_result$x[,1]*-1
boxplot(FormativZeitFB)

##Time on exercise
psych::alpha(mittelwerte_timeExercise)
pca_result <- prcomp(mittelwerte_timeExercise,center = TRUE, scale. = TRUE)
summary(pca_result)
screeplot(pca_result)
pca_result$rotation[,"PC1"]
FormativZeit<-pca_result$x[,1]
boxplot(FormativZeit)

##########performance
psych::alpha(mittelwerte_leistung)
pca_result <- prcomp(mittelwerte_leistung, center = TRUE, scale. = TRUE)
summary(pca_result)
screeplot(pca_result)
pca_result$rotation[,"PC1"]
FormativLeistung<-pca_result$x[,1]* -1

plot(FormativZeit,FormativLeistung)
dat<-cbind(FormativZeitFB,FormativZeit,FormativLeistung)
describe(dat)
hist(dat[,"FormativLeistung"])
boxplot(dat[,"FormativLeistung"])



###exclude students with very low performance
a<-dat[,"FormativLeistung"]>-5
dat<-dat[a,]
cor(dat)

##prepare for cluster analysis
dat<-scale(dat)
dat<-as.data.frame(dat)
describe(dat)
hist(dat[,"FormativLeistung"])
df_raw2<-df_raw[a,]


# Cluster analysis --------------------------------------------------------
set.seed(123)
nbclust_results <- NbClust(dat, method = "kmeans",max.nc = 7)

#nbclust_results <- NbClust(mittelwerte_alle, method = "kmeans")
kmeans_result <- kmeans(dat, centers = 3, nstart = 10)
table(kmeans_result$cluster)

Gruppen<-as.factor(kmeans_result$cluster)
dat$Gruppe<-Gruppen
table(Gruppen)

aggregate(dat$FormativLeistung ~ Gruppen, FUN = mean)
aggregate(dat$FormativLeistung ~ Gruppen, FUN = sd)
shapiro.test(dat$FormativLeistung)
kruskal.test(FormativLeistung ~ Gruppe, data = dat)
pairwise.wilcox.test(dat$FormativLeistung, dat$Gruppe, p.adjust.method = "bonferroni")

aggregate(dat$FormativZeit ~ Gruppen, FUN = mean)
aggregate(dat$FormativZeit ~ Gruppen, FUN = sd)
shapiro.test(dat$FormativZeit)
kruskal.test(dat$FormativZeit ~ Gruppe, data = dat)
pairwise.wilcox.test(dat$FormativZeit, dat$Gruppe, p.adjust.method = "bonferroni")

aggregate(dat$FormativZeitFB ~ Gruppen, FUN = mean)
aggregate(dat$FormativZeitFB ~ Gruppen, FUN = sd)
shapiro.test(dat$FormativZeitFB)
kruskal.test(dat$FormativZeitFB ~ Gruppe, data = dat)
pairwise.wilcox.test(dat$FormativZeitFB, dat$Gruppe, p.adjust.method = "bonferroni")

###Include Pseudo
dat$Pseudo<-df_raw2$Pseudo

####priorknowledge
##time
vars_T00 <- all_variables[grepl("T00", all_variables)]
df_VW<-df_raw2[,vars_T00]
df_VW<-scale(df_VW)
timeOnFeedback <- vars_T00[grepl("feedback_bz", vars_T00)]
timeExercise <- vars_T00[grepl("_bz", vars_T00) & !grepl("feedback_bz", vars_T00)]
#performance
leistungVars <- vars_T00[
  !vars_T00 %in% c(timeOnFeedback, timeExercise) & 
    !grepl("^T06", vars_T00)
]


df_VW_Leistung<-df_raw2[,leistungVars]
Vorwissen<-rowSums(df_VW_Leistung)

df_VW_timeOnFeedback<-df_VW[,timeOnFeedback]
VW_timeOnFeedback<-rowMeans(df_VW_timeOnFeedback)

df_VW_timeExercise<-df_VW[,timeExercise]
VW_timeExercise<-rowMeans(df_VW_timeExercise)

##add prior knowledge
dat$Vorwissen<-Vorwissen
dat$VW_timeExercise<-VW_timeExercise
dat$VW_timeOnFeedback<-VW_timeOnFeedback
dat$Muhe<-df_raw2$Muhe


# Combine with intelligence -----------------------------------------------
df_intelligence <- read_excel("Datensatz_Intelligenz.xlsx") |> 
  dplyr::select(Pseudo, sum_FA, Muehe) |> 
  mutate(Pseudo = tolower(Pseudo))

df_intelligence$Pseudo<-as.numeric(df_intelligence$Pseudo)


df_merged2 <- left_join(
  dat,
  df_intelligence[, c("Pseudo", "sum_FA")],
  by = "Pseudo"
)


df_merged3 <- left_join(df_merged2, df_raw2 |> dplyr::select(Pseudo, SCHLUSS_Note_pruefung, TABS_Code_eingabe,Probetest_sum),
                       by = "Pseudo") 


df_merged3 <- df_merged3 %>%
  mutate(SCHLUSS_Note_pruefung = dplyr::recode(SCHLUSS_Note_pruefung,
                                        `11` = 6.0,
                                        `10` = 5.5,
                                        `9`  = 5.0,
                                        `8`  = 4.5,
                                        `7`  = 4.0,
                                        `6`  = 3.5,
                                        `5`  = 3.0,
                                        .default = NA_real_
  ))




write.xlsx(df_merged3, file = "df_merged3.xlsx", overwrite = TRUE)
