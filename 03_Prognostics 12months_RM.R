
# Regression 12 months

# https://github.com/RitaMorf/LongNeck-Regression-12months/blob/main/Prognostics%2012months.R

# Loading the packages----

library(pacman)
pacman::p_load(tidyverse, lme4, GGally, car, performance, Matrix, 
               generics, tidyselect, brms, sjPlot, Rcpp, rstudioapi,
               bayesplot, todor)


#Umwandlung in NA über alle Variablen durchführen ----

data[] <- lapply(data, function(x) {
  if (is.character(x)) x[x %in% c("", ".", "NA")] <- NA
  return(x)
})

# pain:  Mittelwert von allen 3 paindetect-Variablen erstellen----

# Funktion zur Berechnung des Mittelwerts
berechne_pain_avg <- function(df, zeitpunkt) {
  df %>%
    mutate(!!paste0("pain_avg_", zeitpunkt) := rowMeans(select(., paindetect1, paindetect2, paindetect3), na.rm = TRUE))
}

# Anwendung für alle Zeitpunkte
data_pain_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  select(study_id, paindetect1, paindetect2, paindetect3) %>%
  berechne_pain_avg("T1")

data_pain_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  select(study_id, paindetect1, paindetect2, paindetect3) %>%
  berechne_pain_avg("T2")

data_pain_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  select(study_id, paindetect1, paindetect2, paindetect3) %>%
  berechne_pain_avg("T3")

data_pain_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  select(study_id, paindetect1, paindetect2, paindetect3) %>%
  berechne_pain_avg("T4")




#SCI Umcodierung ----
umkodieren_stress_skalen <- function(df) {
  
  # Umkodierungsfunktion (Skala 0–7 nach 1–7)
  umkodieren <- function(x) {
    return ((7 - 1) * (x - 0) / (7 - 0) + 1)
  }
  
  # Skala 1: Items 1–7
  for (i in 1:7) {
    var_name <- paste0("score_stress_skala1_", i)
    new_name <- paste0(var_name, "_new")
    df[[new_name]] <- umkodieren(as.numeric(df[[var_name]]))
  }
  
  # Skala 2: Items 1–6
  for (i in 1:6) {
    var_name <- paste0("score_stress_skala2_", i)
    new_name <- paste0(var_name, "_new")
    df[[new_name]] <- umkodieren(as.numeric(df[[var_name]]))
  }
  
  # Summenspalten berechnen
  spalten_s1 <- paste0("score_stress_skala1_", 1:7, "_new")
  spalten_s2 <- paste0("score_stress_skala2_", 1:6, "_new")
  
  df$stress_skala1_sum <- rowSums(df[, spalten_s1], na.rm = TRUE)
  df$stress_skala2_sum <- rowSums(df[, spalten_s2], na.rm = TRUE)
  
  return(df)
}
# Funktion testen
data <- umkodieren_stress_skalen(data)



#sample characteristics----
##Table 1 und DataExplorer----
install.packages("DataExplorer")
library(DataExplorer)

create_report(data_T1)
install.packages("tableone")
library(tableone)

###TODO: hier noch anpassen was ich sehen /überprüfen möchte ------
library(tableone)

# Alle Variablen, die du analysieren willst
vars <- c("age", "gender", "smoking", "education", "occupation", "medication", "medcare")

# Angenommen data_cleaned enthält alle diese Spalten
table1 <- CreateTableOne(vars = vars, data = data_cleaned, factorVars = c("gender", "smoking"))

print(table1, showAllLevels = TRUE)


#ausgeschriebener Code
##age----

#  deskriptive Statistiken
age_summary <- list(
  mean = mean(data_age$age, na.rm = TRUE),
  sd = sd(data_age$age, na.rm = TRUE),
  min = min(data_age$age, na.rm = TRUE),
  max = max(data_age$age, na.rm = TRUE),
  range = range(data_age$age, na.rm = TRUE),
  quantiles = quantile(data_age$age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
)

# Ausgabe
print(age_summary)

## Gender----

# Häufigkeiten und Prozentwerte berechnen
gender_counts <- table(data_gender$gender)
gender_percentages <- round(100 * gender_counts / sum(gender_counts), 2)

# DataFrame mit beschrifteten Ergebnissen
gender_df <- data.frame(
  gender = c("männlich", "weiblich"),
  count = as.numeric(gender_counts),
  percentage = as.numeric(gender_percentages)
)

print(gender_df)


## smoking----

# Labels definieren
smoking_labels <- c(
  "1" = "Raucher",
  "2" = "Gelegenheitsraucher",
  "3" = "Nichtraucher"
)

# Häufigkeiten berechnen
smoking_counts <- table(data_smoking$smoking, useNA = "no")

# Prozentwerte berechnen
smoking_percentages <- round(smoking_counts / sum(smoking_counts) * 100, 2)

# DataFrame erstellen
smoking_df <- data.frame(
  smoking_status = smoking_labels[names(smoking_counts)],
  count = as.vector(smoking_counts),
  percentage = as.vector(smoking_percentages)
)

# Ausgabe
print(smoking_df)



## education----

# Labels für die Bildungskategorien
education_labels <- c(
  "1" = "Keine Ausbildung",
  "2" = "Lehre",
  "3" = "Fachhochschule / Universität",
  "4" = "Sonstiges"
)

# Zähle die Häufigkeiten
education_counts <- table(data_education$education, useNA = "no")

# Berechne die Prozentsätze
education_percentages <- round(education_counts / sum(education_counts) * 100, 2)

# Erstelle DataFrame
education_df <- data.frame(
  education = education_labels[names(education_counts)],
  count = as.vector(education_counts),
  percentage = as.vector(education_percentages)
)

# Ausgabe
print(education_df)


## occuptation----

# Labels für die Bildungskategorien
occupation_labels <- c(
  "1" = "Teilzeit",
  "2" = "Vollzeit",
  "3" = "Arbeitslos",
  "4" = "in Ausbildung",
  "5" = "Hausfrau/mann",
  "6" = "IV",
  "7" = "Rente",
)

# Zähle die Häufigkeiten
occupation_counts <- table(data_occupation$occupation, useNA = "no")

# Berechne die Prozentsätze
occupation_percentages <- round(occupation_counts / sum(occupation_counts) * 100, 2)

# Erstelle DataFrame
occupation_df <- data.frame(
  occupation = occupation_labels[names(occupation_counts)],
  count = as.vector(occupation_counts),
  percentage = as.vector(occupation_percentages)
)
#Ausgabe
print(occupation_df)



##medication----

# Zählen und Prozentberechnung
count_and_percent <- function(x) {
  counts <- table(x, useNA = "no")
  percent <- round(counts / sum(counts) * 100, 2)
  data.frame(
    response = c("No", "Yes"),
    count = as.vector(counts),
    percentage = as.vector(percent)
  )
}

# Datenframes erstellen
medication_none_df <- count_and_percent(data_nomedication$medication_none)
medication_pain_df <- count_and_percent(data_painkillers$medication_painkillers)
medication_opioid_df <- count_and_percent(data_opioids$medication_opioid)
medication_depr_df <- count_and_percent(data_antidepressants$medication_antidepressants)
medication_MmRelax_df <- count_and_percent(data_musclerelaxants$medication_MmRelax)
medication_Cann_df <- count_and_percent(data_cannabis$medication_Cann)

# Ausgabe der Häufigkeiten und Prozentwerte
print(medication_none_df)
print(medication_pain_df)
print(medication_opioid_df)
print(medication_depr_df)
print(medication_MmRelax_df)
print(medication_Cann_df)

# Mittelwert und SD berechnen
mean_sd <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  return(c(mean = m, sd = s))
}

# Ergebnisse berechnen
medication_stats <- list(
  "Keine Medikamente" = mean_sd(data_nomedication$medication_none),
  "Schmerzmedikation" = mean_sd(data_painkillers$medication_painkillers),
  "Opioide" = mean_sd(data_opioids$medication_opioid),
  "Antidepressiva" = mean_sd(data_antidepressants$medication_antidepressants),
  "Muskelrelaxanzien" = mean_sd(data_musclerelaxants$medication_MmRelax),
  "Cannabis" = mean_sd(data_cannabis$medication_Cann)
)

# Ausgabe der Mittelwerte und Standardabweichungen
for (name in names(medication_stats)) {
  cat(name, ":\n")
  cat("  Mittelwert: ", round(medication_stats[[name]]["mean"], 2), "\n")
  cat("  Standardabweichung: ", round(medication_stats[[name]]["sd"], 2), "\n\n")
}





##medcare----

# Zählen der Werte in den Spalten Care
care_none_counts <- table(data_medcare_none$care_none, useNA = "no")
care_GP_counts <- table(data_medcare_generalpracticioner$care_GP, useNA = "no")
care_Specialist_counts <- table(data_medcare_specialist$care_Specialist, useNA = "no")
care_PT_counts <- table(data_physiotherapy$care_PT, useNA = "no")
care_Chiro_counts <- table(data_chiropractic$care_Chiro, useNA = "no")
care_Psych_counts <- table(data_psychiatrist$care_Psych, useNA = "no")
care_Mass_counts <- table(data_massage$care_Mass, useNA = "no")

# Berechnung der Prozentsätze
care_none_percentages <- round(care_none_counts / sum(care_none_counts) * 100, 2)
care_GP_percentages <- round(care_GP_counts / sum(care_GP_counts) * 100, 2)
care_Specialist_percentages <- round(care_Specialist_counts / sum(care_Specialist_counts) * 100, 2)
care_PT_percentages <- round(care_PT_counts / sum(care_PT_counts) * 100, 2)
care_Chiro_percentages <- round(care_Chiro_counts / sum(care_Chiro_counts) * 100, 2)
care_Psych_percentages <- round(care_Psych_counts / sum(care_Psych_counts) * 100, 2)
care_Mass_percentages <- round(care_Mass_counts / sum(care_Mass_counts) * 100, 2)

# Erstellung der Ergebnis-DataFrames
care_none_df <- data.frame(care_none = c("Yes", "No"), count = as.vector(care_none_counts), percentage = as.vector(care_none_percentages))
care_GP_df <- data.frame(care_GP = c("No", "Yes"), count = as.vector(care_GP_counts), percentage = as.vector(care_GP_percentages))
care_Specialist_df <- data.frame(care_Specialist = c("No", "Yes"), count = as.vector(care_Specialist_counts), percentage = as.vector(care_Specialist_percentages))
care_PT_df <- data.frame(care_PT = c("No", "Yes"), count = as.vector(care_PT_counts), percentage = as.vector(care_PT_percentages))
care_Chiro_df <- data.frame(care_Chiro = c("No", "Yes"), count = as.vector(care_Chiro_counts), percentage = as.vector(care_Chiro_percentages))
care_Psych_df <- data.frame(care_Psych = c("No", "Yes"), count = as.vector(care_Psych_counts), percentage = as.vector(care_Psych_percentages))
care_Mass_df <- data.frame(care_Mass = c("No", "Yes"), count = as.vector(care_Mass_counts), percentage = as.vector(care_Mass_percentages))

# Ausgabe der Häufigkeiten und Prozentsätze
print(care_none_df)
print(care_GP_df)
print(care_Specialist_df)
print(care_PT_df)
print(care_Chiro_df)
print(care_Psych_df)
print(care_Mass_df)

# Berechnung von Mittelwert und Standardabweichung
care_none_mean <- mean(data_medcare_none$care_none, na.rm = TRUE)
care_none_sd <- sd(data_medcare_none$care_none, na.rm = TRUE)
care_GP_mean <- mean(data_medcare_generalpracticioner$care_GP, na.rm = TRUE)
care_GP_sd <- sd(data_medcare_generalpracticioner$care_GP, na.rm = TRUE)
care_Specialist_mean <- mean(data_medcare_specialist$care_Specialist, na.rm = TRUE)
care_Specialist_sd <- sd(data_medcare_specialist$care_Specialist, na.rm = TRUE)
care_PT_mean <- mean(data_physiotherapy$care_PT, na.rm = TRUE)
care_PT_sd <- sd(data_physiotherapy$care_PT, na.rm = TRUE)
care_Chiro_mean <- mean(data_chiropractic$care_Chiro, na.rm = TRUE)
care_Chiro_sd <- sd(data_chiropractic$care_Chiro, na.rm = TRUE)
care_Psych_mean <- mean(data_psychiatrist$care_Psych, na.rm = TRUE)
care_Psych_sd <- sd(data_psychiatrist$care_Psych, na.rm = TRUE)
care_Mass_mean <- mean(data_massage$care_Mass, na.rm = TRUE)
care_Mass_sd <- sd(data_massage$care_Mass, na.rm = TRUE)

# Ausgabe der Mittelwerte und Standardabweichungen
cat("Keine Care:\n")
cat("  Mittelwert: ", round(care_none_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_none_sd, 2), "\n\n")

cat("Hausärzt:in (GP):\n")
cat("  Mittelwert: ", round(care_GP_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_GP_sd, 2), "\n\n")

cat("Fachärzt:in:\n")
cat("  Mittelwert: ", round(care_Specialist_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_Specialist_sd, 2), "\n\n")

cat("Physiotherapie:\n")
cat("  Mittelwert: ", round(care_PT_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_PT_sd, 2), "\n\n")

cat("Chiropraktik:\n")
cat("  Mittelwert: ", round(care_Chiro_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_Chiro_sd, 2), "\n\n")

cat("Psychologische Betreuung:\n")
cat("  Mittelwert: ", round(care_Psych_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_Psych_sd, 2), "\n\n")

cat("Massage:\n")
cat("  Mittelwert: ", round(care_Mass_mean, 2), "\n")
cat("  Standardabweichung: ", round(care_Mass_sd, 2), "\n\n")


##pain----


#Funktion zur Zusammenfassung
summarize_pain <- function(df, timepoint) {
  vars <- c("paindetect1", "paindetect2", "paindetect3")
  
  purrr::map_dfr(vars, function(var) {
    values <- df[[var]]
    n <- sum(!is.na(values))
    mean_val <- mean(values, na.rm = TRUE)
    sd_val <- sd(values, na.rm = TRUE)
    range_val <- range(values, na.rm = TRUE)
    quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
    
    tibble(
      timepoint = timepoint,
      variable = var,
      n = n,
      mean = round(mean_val, 2),
      sd = round(sd_val, 2),
      range = paste(range_val, collapse = " - "),
      q_2.5 = quantiles[1],
      q_25 = quantiles[2],
      q_50 = quantiles[3],
      q_75 = quantiles[4],
      q_97.5 = quantiles[5]
    )
  })
}

# Anwendung auf alle Zeitpunkte
pain_summary <- bind_rows(
  summarize_pain(data_pain_T1, "T1"),
  summarize_pain(data_pain_T2, "T2"),
  summarize_pain(data_pain_T3, "T3"),
  summarize_pain(data_pain_T4, "T4")
)

# Ausgabe anzeigen
print(pain_summary)


##disability----
# Schritt 1: Sicherstellen, dass ndiscore numerisch ist
data_disability_T1 <- data_disability_T1 %>% mutate(ndiscore = as.numeric(ndiscore))
data_disability_T2 <- data_disability_T2 %>% mutate(ndiscore = as.numeric(ndiscore))
data_disability_T3 <- data_disability_T3 %>% mutate(ndiscore = as.numeric(ndiscore))
data_disability_T4 <- data_disability_T4 %>% mutate(ndiscore = as.numeric(ndiscore))

# Schritt 2: Funktion zur Auswertung
summarize_disability <- function(df, timepoint) {
  values <- df$ndiscore
  n <- sum(!is.na(values))
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  range_val <- range(values, na.rm = TRUE)
  quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  
  tibble(
    timepoint = timepoint,
    n = n,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    range = paste(range_val, collapse = " - "),
    q_2.5 = quantiles[1],
    q_25 = quantiles[2],
    q_50 = quantiles[3],
    q_75 = quantiles[4],
    q_97.5 = quantiles[5]
  )
}

# Schritt 3: Zusammenfassung aller Zeitpunkte
disability_summary <- bind_rows(
  summarize_disability(data_disability_T1, "T1"),
  summarize_disability(data_disability_T2, "T2"),
  summarize_disability(data_disability_T3, "T3"),
  summarize_disability(data_disability_T4, "T4")
)

# Schritt 4: Ergebnis anzeigen
print(disability_summary)


##depression-scale: DASS21----

# Funktion zur Auswertung
summarize_depression <- function(df, timepoint) {
  values <- df$dass21_depression
  n <- sum(!is.na(values))
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  range_val <- range(values, na.rm = TRUE)
  quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  
  tibble(
    timepoint = timepoint,
    n = n,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    range = paste(range_val, collapse = " - "),
    q_2.5 = round(quantiles[1], 2),
    q_25 = round(quantiles[2], 2),
    q_50 = round(quantiles[3], 2),
    q_75 = round(quantiles[4], 2),
    q_97.5 = round(quantiles[5], 2)
  )
}

# Zusammenfassung aller Zeitpunkte
depression_summary <- bind_rows(
  summarize_depression(data_depression_T1, "T1"),
  summarize_depression(data_depression_T2, "T2"),
  summarize_depression(data_depression_T3, "T3"),
  summarize_depression(data_depression_T4, "T4")
)

# Ausgabe
print(depression_summary)


##stress-scale: DASS21----

# Zusammenfassungsfunktion
summarize_stress <- function(df, timepoint) {
  values <- df$dass21_stress
  n <- sum(!is.na(values))
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  range_val <- range(values, na.rm = TRUE)
  quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  
  tibble(
    timepoint = timepoint,
    n = n,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    range = paste(range_val, collapse = " - "),
    q_2.5 = round(quantiles[1], 2),
    q_25 = round(quantiles[2], 2),
    q_50 = round(quantiles[3], 2),
    q_75 = round(quantiles[4], 2),
    q_97.5 = round(quantiles[5], 2)
  )
}

#  Zusammenfassung für alle Zeitpunkte
stress_summary <- bind_rows(
  summarize_stress(data_DASS_stress_T1, "T1"),
  summarize_stress(data_DASS_stress_T2, "T2"),
  summarize_stress(data_DASS_stress_T3, "T3"),
  summarize_stress(data_DASS_stress_T4, "T4")
)

# Ausgabe
print(stress_summary)


##Anxiety: STAI-S----

# Funktion zur Zusammenfassung
summarize_stai <- function(df, timepoint) {
  values <- df$stais_score
  n <- sum(!is.na(values))
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  range_val <- range(values, na.rm = TRUE)
  quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  
  tibble(
    timepoint = timepoint,
    n = n,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    range = paste(range_val, collapse = " - "),
    q_2.5 = round(quantiles[1], 2),
    q_25 = round(quantiles[2], 2),
    q_50 = round(quantiles[3], 2),
    q_75 = round(quantiles[4], 2),
    q_97.5 = round(quantiles[5], 2)
  )
}

# Zusammenfassungen berechnen
summary_T1 <- summarize_stai(data_STAIanxiety_T1, "T1")
summary_T2 <- summarize_stai(data_STAIanxiety_T2, "T2")
summary_T3 <- summarize_stai(data_STAIanxiety_T3, "T3")
summary_T4 <- summarize_stai(data_STAIanxiety_T4, "T4")

# Alles zusammenführen
stais_all_timepoints <- bind_rows(summary_T1, summary_T2, summary_T3, summary_T4)

# Ausgabe
print(stais_all_timepoints)



##Pain Vigilance: PVAQ----

# Funktion zur Auswertung
summarize_pvaq <- function(df, timepoint) {
  values <- df$pvaq_score_total
  n <- sum(!is.na(values))
  mean_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)
  range_val <- range(values, na.rm = TRUE)
  quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  
  tibble(
    timepoint = timepoint,
    n = n,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    range = paste(range_val, collapse = " - "),
    q_2.5 = round(quantiles[1], 2),
    q_25 = round(quantiles[2], 2),
    q_50 = round(quantiles[3], 2),
    q_75 = round(quantiles[4], 2),
    q_97.5 = round(quantiles[5], 2)
  )
}

# Anwendung auf alle Zeitpunkte
summary_pvaq_T1 <- summarize_pvaq(data_PVAQPainVigilance_T1, "T1")
summary_pvaq_T2 <- summarize_pvaq(data_PVAQPainVigilance_T2, "T2")
summary_pvaq_T3 <- summarize_pvaq(data_PVAQPainVigilance_T3, "T3")
summary_pvaq_T4 <- summarize_pvaq(data_PVAQPainVigilance_T4, "T4")

# Zusammenführen
pvaq_all_timepoints <- bind_rows(
  summary_pvaq_T1,
  summary_pvaq_T2,
  summary_pvaq_T3,
  summary_pvaq_T4
)

# Ausgabe
print(pvaq_all_timepoints)



##Physical Activity----

# Werte extrahieren
values <- data_physicalactivity$met_total

# Anzahl gültiger Werte
n <- sum(!is.na(values))

# Berechnungen
mean_val <- mean(values, na.rm = TRUE)
sd_val <- sd(values, na.rm = TRUE)
range_val <- range(values, na.rm = TRUE)
quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)

# Ausgabe als tibble (optional)
physical_activity_summary <- tibble::tibble(
  timepoint = "T1",
  n = n,
  mean = round(mean_val, 2),
  sd = round(sd_val, 2),
  range = paste(range_val, collapse = " - "),
  q_2.5 = round(quantiles[1], 2),
  q_25 = round(quantiles[2], 2),
  q_50 = round(quantiles[3], 2),
  q_75 = round(quantiles[4], 2),
  q_97.5 = round(quantiles[5], 2)
)

# Ausgabe
print(physical_activity_summary)



##Sedentary activity----

# Werte extrahieren
values <- data_sedentaryactivity$total_sitting_mins

# Anzahl gültiger Werte
n <- sum(!is.na(values))

# Berechnungen
mean_val <- mean(values, na.rm = TRUE)
sd_val <- sd(values, na.rm = TRUE)
range_val <- range(values, na.rm = TRUE)
quantiles <- quantile(values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)

# Zusammenfassen in DataFrame oder tibble
sedentary_summary <- tibble::tibble(
  timepoint = "T1",
  n = n,
  mean = round(mean_val, 2),
  sd = round(sd_val, 2),
  range = paste(range_val, collapse = " - "),
  q_2.5 = round(quantiles[1], 2),
  q_25 = round(quantiles[2], 2),
  q_50 = round(quantiles[3], 2),
  q_75 = round(quantiles[4], 2),
  q_97.5 = round(quantiles[5], 2)
)

# Ausgabe
print(sedentary_summary)



##Sleep----

# Werte extrahieren
sleep_values <- data_sleep$sleepquality

# Anzahl gültiger Werte
n <- sum(!is.na(sleep_values))

# Berechnungen
mean_val <- mean(sleep_values, na.rm = TRUE)
sd_val <- sd(sleep_values, na.rm = TRUE)
range_val <- range(sleep_values, na.rm = TRUE)
quantiles <- quantile(sleep_values, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)

# Zusammenfassen in ein tibble
sleep_summary <- tibble::tibble(
  timepoint = "T1",
  n = n,
  mean = round(mean_val, 2),
  sd = round(sd_val, 2),
  range = paste(range_val, collapse = " - "),
  q_2.5 = round(quantiles[1], 2),
  q_25 = round(quantiles[2], 2),
  q_50 = round(quantiles[3], 2),
  q_75 = round(quantiles[4], 2),
  q_97.5 = round(quantiles[5], 2)
)

# Ausgabe
print(sleep_summary)



##Stress:SCI----
###uncertainty----

# Hilfsfunktion für Zusammenfassung
summarize_stress_score <- function(vec, timepoint) {
  n <- sum(!is.na(vec))
  mean_val <- mean(vec, na.rm = TRUE)
  sd_val <- sd(vec, na.rm = TRUE)
  range_val <- range(vec, na.rm = TRUE)
  quantiles <- quantile(vec, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  
  data.frame(
    timepoint = timepoint,
    n = n,
    mean = round(mean_val, 2),
    sd = round(sd_val, 2),
    range = paste(range_val, collapse = " - "),
    q_2.5 = round(quantiles[1], 2),
    q_25 = round(quantiles[2], 2),
    q_50 = round(quantiles[3], 2),
    q_75 = round(quantiles[4], 2),
    q_97.5 = round(quantiles[5], 2)
  )
}

# Anwendung auf alle Zeitpunkte
stress_uncertainty_T1 <- summarize_stress_score(data_stress_uncertainty_T1$score_stress_skala_1, "T1")
stress_uncertainty_T2 <- summarize_stress_score(data_stress_uncertainty_T2$score_stress_skala_1, "T2")
stress_uncertainty_T3 <- summarize_stress_score(data_stress_uncertainty_T3$score_stress_skala_1, "T3")
stress_uncertainty_T4 <- summarize_stress_score(data_stress_uncertainty_T4$score_stress_skala_1, "T4")

# Zusammenführen
stress_uncertainty_all <- rbind(stress_uncertainty_T1,
                                stress_uncertainty_T2,
                                stress_uncertainty_T3,
                                stress_uncertainty_T4)

# Ausgabe
print(stress_uncertainty_all)



###excessive demands----
# Hilfsfunktion zur Auswertung
auswertung_score <- function(df, varname, zeitpunkt) {
  df[[varname]] <- as.numeric(df[[varname]])
  n <- sum(!is.na(df[[varname]]))
  mean_val <- mean(df[[varname]], na.rm = TRUE)
  sd_val <- sd(df[[varname]], na.rm = TRUE)
  range_val <- range(df[[varname]], na.rm = TRUE)
  quantiles <- quantile(df[[varname]], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  z <- qnorm(0.975)
  ci_lower <- mean_val - z * sd_val / sqrt(n)
  ci_upper <- mean_val + z * sd_val / sqrt(n)
  
  cat("=== Score Skala 2 (Excessive Demands) -", zeitpunkt, "===\n")
  cat("Mean:", round(mean_val, 2), "| SD:", round(sd_val, 2), "\n")
  cat("Range:", paste(range_val, collapse = " - "), "\n")
  cat("Quantiles (2.5%–97.5%):", paste(round(quantiles, 2), collapse = " | "), "\n")
  cat("95% CI: (", round(ci_lower, 2), ",", round(ci_upper, 2), ")\n\n")
}


# Auswertungen durchführen
auswertung_score(data_stress_excessive_demands_T1, "score_stress_skala_2", "T1")
auswertung_score(data_stress_excessive_demands_T2, "score_stress_skala_2", "T2")
auswertung_score(data_stress_excessive_demands_T3, "score_stress_skala_2", "T3")
auswertung_score(data_stress_excessive_demands_T4, "score_stress_skala_2", "T4")



###symptoms----
# Hilfsfunktion zur Auswertung
auswertung_stress <- function(df, varname, zeitpunkt) {
  df[[varname]] <- as.numeric(df[[varname]])
  n <- sum(!is.na(df[[varname]]))
  mean_val <- mean(df[[varname]], na.rm = TRUE)
  sd_val <- sd(df[[varname]], na.rm = TRUE)
  range_val <- range(df[[varname]], na.rm = TRUE)
  quantiles <- quantile(df[[varname]], probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  z <- qnorm(0.975)
  ci_lower <- mean_val - z * sd_val / sqrt(n)
  ci_upper <- mean_val + z * sd_val / sqrt(n)
  
  cat("=== Gesamtscore Stresssymptome -", zeitpunkt, "===\n")
  cat("Mean:", round(mean_val, 2), "| SD:", round(sd_val, 2), "\n")
  cat("Range:", paste(range_val, collapse = " - "), "\n")
  cat("Quantiles (2.5%–97.5%):", paste(round(quantiles, 2), collapse = " | "), "\n")
  cat("95% CI: (", round(ci_lower, 2), ",", round(ci_upper, 2), ")\n\n")
}

# Anwendung auf alle vier Zeitpunkte
auswertung_stress(data_stress_symptoms_T1, "gesamtscore_symptom", "T1")
auswertung_stress(data_stress_symptoms_T2, "gesamtscore_symptom", "T2")
auswertung_stress(data_stress_symptoms_T3, "gesamtscore_symptom", "T3")
auswertung_stress(data_stress_symptoms_T4, "gesamtscore_symptom", "T4")


##activity patterns----
###Anzahl der Aktivitätsmuster pro Zeitpukt----
berechne_aktivitaetsmuster <- function(df) {
  df$FAR <- ifelse(df$score_pps < 3 & df$dms_score == 2, TRUE, FALSE)
  df$DER <- ifelse(df$score_pps >= 3 & df$dms_score == 2, TRUE, FALSE)
  df$EER <- ifelse(df$score_pps >= 3 & df$dms_score < 2, TRUE, FALSE)
  df$AR  <- ifelse(df$score_pps < 3 & df$dms_score < 2, TRUE, FALSE)
  
  summarised <- data.frame(
    Gruppe = c("FAR", "DER", "EER", "AR"),
    Anzahl = c(
      sum(df$FAR, na.rm = TRUE),
      sum(df$DER, na.rm = TRUE),
      sum(df$EER, na.rm = TRUE),
      sum(df$AR, na.rm = TRUE)
    )
  )
  
  return(summarised)
}

aktiv_T1 <- berechne_aktivitaetsmuster(data_activity_patterns_T1)
aktiv_T2 <- berechne_aktivitaetsmuster(data_activity_patterns_T2)
aktiv_T3 <- berechne_aktivitaetsmuster(data_activity_patterns_T3)
aktiv_T4 <- berechne_aktivitaetsmuster(data_activity_patterns_T4)
# Ausgabe der Ergebnisse
aktiv_T1$Zeitpunkt <- "T1"
aktiv_T2$Zeitpunkt <- "T2"
aktiv_T3$Zeitpunkt <- "T3"
aktiv_T4$Zeitpunkt <- "T4"

aktivitaetsmuster_gesamt <- rbind(aktiv_T1, aktiv_T2, aktiv_T3, aktiv_T4)
print(aktivitaetsmuster_gesamt)



###pps/dms Berechnungen----
berechne_pps_dms_stats <- function(df, zeitpunkt = "T1") {
  # Häufigkeiten
  pps_counts <- table(df$score_pps, useNA = "no")
  dms_counts <- table(df$dms_score, useNA = "no")
  
  # Prozentwerte
  pps_percentages <- round(pps_counts / sum(pps_counts) * 100, 2)
  dms_percentages <- round(dms_counts / sum(dms_counts) * 100, 2)
  
  # Ergebnis-DataFrames
  pps_df <- data.frame(score_pps = names(pps_counts),
                       count = as.vector(pps_counts),
                       percentage = as.vector(pps_percentages))
  
  dms_df <- data.frame(dms_score = names(dms_counts),
                       count = as.vector(dms_counts),
                       percentage = as.vector(dms_percentages))
  
  # Ausgabe Tabellen
  cat("\n====", zeitpunkt, "- Häufigkeiten ====\n")
  print(pps_df)
  print(dms_df)
  
  # Statistiken PPS
  n_pps <- sum(!is.na(df$score_pps))
  mean_pps <- mean(df$score_pps, na.rm = TRUE)
  sd_pps <- sd(df$score_pps, na.rm = TRUE)
  range_pps <- range(df$score_pps, na.rm = TRUE)
  quantiles_pps <- quantile(df$score_pps, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  ci_lower_pps <- mean_pps - qnorm(0.975) * sd_pps / sqrt(n_pps)
  ci_upper_pps <- mean_pps + qnorm(0.975) * sd_pps / sqrt(n_pps)
  
  # Statistiken DMS
  n_dms <- sum(!is.na(df$dms_score))
  mean_dms <- mean(df$dms_score, na.rm = TRUE)
  sd_dms <- sd(df$dms_score, na.rm = TRUE)
  range_dms <- range(df$dms_score, na.rm = TRUE)
  quantiles_dms <- quantile(df$dms_score, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE)
  ci_lower_dms <- mean_dms - qnorm(0.975) * sd_dms / sqrt(n_dms)
  ci_upper_dms <- mean_dms + qnorm(0.975) * sd_dms / sqrt(n_dms)
  
  # Ausgabe der statistischen Kennzahlen
  cat("===", zeitpunkt, "- PPS Score ===\n")
  cat("Mean:", round(mean_pps, 2), " | SD:", round(sd_pps, 2), "\n")
  cat("Range:", paste(range_pps, collapse = " - "), "\n")
  cat("Quantiles (2.5%–97.5%):", paste(round(quantiles_pps, 2), collapse = " | "), "\n")
  cat("95% CI: (", round(ci_lower_pps, 2), ", ", round(ci_upper_pps, 2), ")\n\n")
  
  cat("===", zeitpunkt, "- DMS Score ===\n")
  cat("Mean:", round(mean_dms, 2), " | SD:", round(sd_dms, 2), "\n")
  cat("Range:", paste(range_dms, collapse = " - "), "\n")
  cat("Quantiles (2.5%–97.5%):", paste(round(quantiles_dms, 2), collapse = " | "), "\n")
  cat("95% CI: (", round(ci_lower_dms, 2), ", ", round(ci_upper_dms, 2), ")\n")
}
#Zeitpunkte ausgeben
berechne_pps_dms_stats(data_activity_patterns_T1, "T1")
berechne_pps_dms_stats(data_activity_patterns_T2, "T2")
berechne_pps_dms_stats(data_activity_patterns_T3, "T3")
berechne_pps_dms_stats(data_activity_patterns_T4, "T4")


##HCC----

# Hilfsfunktion
berechne_stats_nach_gender <- function(df, var, gender_var = "gender") {
  df %>%
    mutate(gender = factor(!!sym(gender_var), levels = c(1, 2), labels = c("Männlich", "Weiblich"))) %>%
    group_by(gender) %>%
    summarise(
      n = sum(!is.na(.data[[var]])),
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE),
      min = min(.data[[var]], na.rm = TRUE),
      max = max(.data[[var]], na.rm = TRUE),
      q025 = quantile(.data[[var]], 0.025, na.rm = TRUE),
      q25 = quantile(.data[[var]], 0.25, na.rm = TRUE),
      median = quantile(.data[[var]], 0.5, na.rm = TRUE),
      q75 = quantile(.data[[var]], 0.75, na.rm = TRUE),
      q975 = quantile(.data[[var]], 0.975, na.rm = TRUE),
      ci_lower = mean - qnorm(0.975) * sd / sqrt(n),
      ci_upper = mean + qnorm(0.975) * sd / sqrt(n)
    ) %>%
    mutate(variable = var)
}
# Anwendung auf alle Zeitpunkte

# T1
stats_cortisol_T1 <- berechne_stats_nach_gender(data_cortisol_T1, "cortisol1")
stats_cortisone_T1 <- berechne_stats_nach_gender(data_cortisone_T1, "cortisone1")

# T3
stats_cortisol_T3 <- berechne_stats_nach_gender(data_cortisol_T3, "cortisol2")
stats_cortisone_T3 <- berechne_stats_nach_gender(data_cortisone_T3, "cortisone2")

# T6
stats_cortisol_T6 <- berechne_stats_nach_gender(data_cortisol_T6, "cortisol3")
stats_cortisone_T6 <- berechne_stats_nach_gender(data_cortisone_T6, "cortisone3")


#Ergebnisse zusammenfassen
stats_all <- bind_rows(
  stats_cortisol_T1 %>% mutate(zeitpunkt = "T1"),
  stats_cortisol_T3 %>% mutate(zeitpunkt = "T3"),
  stats_cortisol_T6 %>% mutate(zeitpunkt = "T6"),
  stats_cortisone_T1 %>% mutate(zeitpunkt = "T1"),
  stats_cortisone_T3 %>% mutate(zeitpunkt = "T3"),
  stats_cortisone_T6 %>% mutate(zeitpunkt = "T6")
)

# Ausgabe anzeigen
print(stats_all)

## Session Info exportieren ----
# Erstelle einen Dateinamen mit aktuellem Datum
date_stamp <- format(Sys.Date(), "%Y-%m-%d")
filename <- paste0("session_info_", date_stamp, ".txt")

# Speichere die SessionInfo als Textdatei
sink(filename)
sessionInfo()
sink()



#LMM ----

library(dplyr)
library(car)

## Modellprüfung für pain----

pruefe_pain_modell <- function(
    data_pain,
    data_disability,
    data_stress_uncertainty,
    data_stress_excessive_demands,
    data_stress_symptoms,
    data_activity_patterns,
    data_cortisol,
    data_cortisone,
    data_age,
    data_depression,
    data_gender,
    pain_variable_name
) {
  # Daten zusammenführen
  model_data <- data_pain %>%
    left_join(data_disability, by = "study_id") %>%
    left_join(data_stress_uncertainty, by = "study_id") %>%
    left_join(data_stress_excessive_demands, by = "study_id") %>%
    left_join(data_stress_symptoms, by = "study_id") %>%
    left_join(data_activity_patterns, by = "study_id") %>%
    left_join(data_cortisol, by = "study_id") %>%
    left_join(data_cortisone, by = "study_id") %>%
    left_join(data_age, by = "study_id") %>%
    left_join(data_depression, by = "study_id") %>%
    left_join(data_gender, by = "study_id") %>%
    mutate(gender = factor(gender, levels = c(1, 2), labels = c("male", "female"))) %>%
    filter(complete.cases(.))
  
  # Gender als Faktor
  model_data <- model_data %>%
    mutate(gender = factor(gender, levels = c(1, 2), labels = c("male", "female")))
  
  # Dynamische Formel bauen
  formel <- as.formula(paste0(
    pain_variable_name, " ~ ndiscore + stress_skala1_sum + stress_skala2_sum + ",
    "stress_skala_symptome_sum + score_pps + dms_score + cortisol1 + cortisone1 + ",
    "age + gender + dass21_depression"
  ))
  
  # Modell schätzen
  lm_model <- lm(formel, data = model_data)
  
  # VIF-Werte
  vif_values <- vif(lm_model)
  vif_df <- data.frame(
    Variable = names(vif_values),
    VIF = round(as.numeric(vif_values), 2)
  )
  print(paste("📌 VIFs für:", pain_variable_name))
  print(vif_df)
  
  
  # Anwendung auf T1
  vif_T1 <- pruefe_multikollinearitaet(
    data_pain_T1,
    data_disability_T1,
    data_stress_uncertainty_T1,
    data_stress_excessive_demands_T1,
    data_stress_symptoms_T1,
    data_activity_patterns_T1,
    data_cortisol_T1,
    data_cortisol_T1,  
    data_age,
    data_depression_T1,
    data_gender,
    "pain_avg_T1"
  )
  
  # Anwendung auf T2
  vif_T2 <- pruefe_multikollinearitaet(
    data_pain_T2,
    data_disability_T2,
    data_stress_uncertainty_T2,
    data_stress_excessive_demands_T2,
    data_stress_symptoms_T2,
    data_activity_patterns_T2,
    data_cortisol_T2,
    data_cortisol_T2,
    data_age,
    data_depression_T2,
    data_gender,
    "pain_avg_T2"
  )
  
  # Anwendung auf T3
  vif_T3 <- pruefe_multikollinearitaet(
    data_pain_T3,
    data_disability_T3,
    data_stress_uncertainty_T3,
    data_stress_excessive_demands_T3,
    data_stress_symptoms_T3,
    data_activity_patterns_T3,
    data_cortisol_T3,
    data_cortisol_T3,
    data_age,
    data_depression_T3,
    data_gender,
    "pain_avg_T3"
  )
  
  # Anwendung auf T4
  vif_T4 <- pruefe_multikollinearitaet(
    data_pain_T4,
    data_disability_T4,
    data_stress_uncertainty_T4,
    data_stress_excessive_demands_T4,
    data_stress_symptoms_T4,
    data_activity_patterns_T4,
    data_age,
    data_depression_T4,
    data_gender,
    "pain_avg_T4"
  )
  # Residualdiagnostik
  res <- residuals(lm_model)
  
  # Histogramm
  hist(res, main = paste("Histogramm der Residuen:", pain_variable_name), xlab = "Residuen")
  
  # Q-Q-Plot
  qqnorm(res, main = paste("Q-Q-Plot:", pain_variable_name))
  qqline(res, col = "red")
  
  # Shapiro-Wilk-Test (bei kleineren Stichproben)
  if (length(res) <= 5000) {
    cat("\n📈 Shapiro-Wilk-Test:\n")
    print(shapiro.test(res))
  }
  
  invisible(lm_model)  # Modell zurückgeben, wenn gewünscht
}


##Modellprüfung für disability----
pruefe_multikollinearitaet_disability <- function(
    data_disability,
    data_pain,
    data_stress_uncertainty,
    data_stress_excessive_demands,
    data_stress_symptoms,
    data_activity_patterns,
    data_cortisol,
    data_cortisone,
    data_age,
    data_depression,
    data_gender,
    pain_variable_name
) {
  # Variablen zusammenführen
  model_data <- data_disability %>%
    left_join(data_pain, by = "study_id") %>%
    left_join(data_stress_uncertainty, by = "study_id") %>%
    left_join(data_stress_excessive_demands, by = "study_id") %>%
    left_join(data_stress_symptoms, by = "study_id") %>%
    left_join(data_activity_patterns, by = "study_id") %>%
    left_join(data_cortisol, by = "study_id") %>%
    left_join(data_cortisone, by = "study_id") %>%
    left_join(data_age, by = "study_id") %>%
    left_join(data_depression, by = "study_id") %>%
    left_join(data_gender, by = "study_id")
  
  # Gender als Faktor
  model_data <- model_data %>%
    mutate(gender = factor(gender, levels = c(1, 2), labels = c("male", "female")))
  
  # Formel dynamisch bauen
  formel <- as.formula(paste0(
    "ndiscore ~ ", pain_variable_name, " + stress_skala1_sum + stress_skala2_sum + ",
    "stress_skala_symptome_sum + score_pps + dms_score + cortisol1 + cortisone1 + ",
    "age + gender + dass21_depression"
  ))
  
  # Modell schätzen
  lm_model <- lm(formel, data = model_data)
  
  # VIF berechnen
  vif_values <- vif(lm_model)
  vif_df <- data.frame(
    Variable = names(vif_values),
    VIF = round(as.numeric(vif_values), 2)
  )
  
  # Diagnostik-Plot
  par(mfrow = c(1, 2))
  hist(residuals(lm_model), main = paste("Histogram Residuen (", pain_variable_name, ")", sep=""), xlab = "Residuals")
  qqnorm(residuals(lm_model), main = paste("QQ-Plot (", pain_variable_name, ")", sep="")); qqline(residuals(lm_model), col = "red")
  par(mfrow = c(1, 1))
  
  # Shapiro-Wilk-Test
  print(shapiro.test(residuals(lm_model)))
  
  return(vif_df)
}

vif_disability_T1 <- pruefe_multikollinearitaet_disability(
  data_disability_T1,
  data_pain_T1,
  data_stress_uncertainty_T1,
  data_stress_excessive_demands_T1,
  data_stress_symptoms_T1,
  data_activity_patterns_T1,
  data_cortisol_T1,
  data_cortisone_T1,
  data_age,
  data_depression_T1,
  data_gender,
  "pain_avg_T1"
)

vif_disability_T2 <- pruefe_multikollinearitaet_disability(
  data_disability_T2,
  data_pain_T2,
  data_stress_uncertainty_T2,
  data_stress_excessive_demands_T2,
  data_stress_symptoms_T2,
  data_activity_patterns_T2,
  data_cortisol_T2,
  data_cortisone_T2,
  data_age,
  data_depression_T2,
  data_gender,
  "pain_avg_T2"
)

vif_disability_T3 <- pruefe_multikollinearitaet_disability(
  data_disability_T3,
  data_pain_T3,
  data_stress_uncertainty_T3,
  data_stress_excessive_demands_T3,
  data_stress_symptoms_T3,
  data_activity_patterns_T3,
  data_cortisol_T3,
  data_cortisone_T3,
  data_age,
  data_depression_T3,
  data_gender,
  "pain_avg_T3"
)

vif_disability_T4 <- pruefe_multikollinearitaet_disability(
  data_disability_T4,
  data_pain_T4,
  data_stress_uncertainty_T4,
  data_stress_excessive_demands_T4,
  data_stress_symptoms_T4,
  data_activity_patterns_T4,
  data_age,
  data_depression_T4,
  data_gender,
  "pain_avg_T4"
)


##check model----
install.packages ("performance")
install.packages ("easystats")
install.packages ("see")


# Laden
library(performance)
library(see)
library(easystats)


# Lade Pakete, falls noch nicht geladen
library(dplyr)
library(performance)
library(see)

# Funktion zur Modell-Erstellung und Diagnose
modell_und_diagnose <- function(
    zeitpunkt,                 # z. B. "T1"
    zielvariable               # z. B. "pain_avg_T1" oder "ndiscore"
) {
  # Dynamische Objektnamen zusammensetzen
  data_pain <- get(paste0("data_pain_", zeitpunkt))
  data_disability <- get(paste0("data_disability_", zeitpunkt))
  data_stress_uncertainty <- get(paste0("data_stress_uncertainty_", zeitpunkt))
  data_stress_excessive_demands <- get(paste0("data_stress_excessive_demands_", zeitpunkt))
  data_stress_symptoms <- get(paste0("data_stress_symptoms_", zeitpunkt))
  data_activity_patterns <- get(paste0("data_activity_patterns_", zeitpunkt))
  data_cortisol <- get(paste0("data_cortisol_", zeitpunkt))
  data_cortisone <- get(paste0("data_cortisone_", zeitpunkt))
  data_depression <- get(paste0("data_depression_", zeitpunkt))
  
  # Gemeinsames Modell-DataFrame
  model_data <- data_pain %>%
    left_join(data_disability, by = "study_id") %>%
    left_join(data_stress_uncertainty, by = "study_id") %>%
    left_join(data_stress_excessive_demands, by = "study_id") %>%
    left_join(data_stress_symptoms, by = "study_id") %>%
    left_join(data_activity_patterns, by = "study_id") %>%
    left_join(data_cortisol, by = "study_id") %>%
    left_join(data_cortisone, by = "study_id") %>%
    left_join(data_age, by = "study_id") %>%
    left_join(data_depression, by = "study_id") %>%
    left_join(data_gender, by = "study_id") %>%
    filter(complete.cases(.)) %>%
    mutate(gender = factor(gender, levels = c(1, 2), labels = c("male", "female")))
  
  # Dynamisch Formel bauen
  formel <- as.formula(paste0(
    zielvariable, " ~ ndiscore + stress_skala1_sum + stress_skala2_sum + ",
    "stress_skala_symptome_sum + score_pps + dms_score + cortisol1 + cortisone1 + ",
    "age + gender + dass21_depression"
  ))
  
  # Modell berechnen
  modell <- lm(formel, data = model_data)
  
  # Diagnose anzeigen
  check_model(modell,
              check = c("linearity", "normality", "homogeneity",
                        "outliers", "collinearity", "check_predictions"))
  
  # Optionale Rückgabe
  return(modell)
}

#Anwendung auf die Zeitpunkte
modell_pain_T1 <- modell_und_diagnose("T1", "pain_avg_T1")
modell_pain_T2 <- modell_und_diagnose("T2", "pain_avg_T2")
modell_pain_T3 <- modell_und_diagnose("T3", "pain_avg_T3")
modell_pain_T4 <- modell_und_diagnose("T4", "pain_avg_T4")

modell_disability_T1 <- modell_und_diagnose("T1", "ndiscore")
modell_disability_T2 <- modell_und_diagnose("T2", "ndiscore")
modell_disability_T3 <- modell_und_diagnose("T3", "ndiscore")
modell_disability_T4 <- modell_und_diagnose("T4", "ndiscore")
