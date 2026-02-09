
# Regression 12 months

# https://github.com/RitaMorf/LongNeck-Regression-12months/blob/main/Prognostics%2012months.R

# Loading the packages----
pacman::p_load(tidyverse, lme4, GGally, car, performance, Matrix, 
               generics, tidyselect, brms, sjPlot, Rcpp, rstudioapi,
               bayesplot, todor)
install.packages("easystats")
install.packages("tableone")
install.packages("DataExplorer")
install.packages("datawizard")
install.packages("performance")
install.packages("zoo")
install.packages("rstatix")
install.packages("ggpubr")
install.packages("emmeans")
install.packages("lmtest")
easystats::easystats_update()
install.packages(c("insight", "modelbased", "performance", "parameters", "report"))

library(lmtest)
library(pacman)
library(easystats)
library(tableone)
library(DataExplorer)
library(datawizard)
library(parallel)
library(performance)
library(brms)
library(VIM)
library(bayesplot)
library(dplyr)  
library(lme4)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(car)
library(ggplot2)
library(VIM)
library(zoo)
library(rstatix)
library(ggpubr)
library(emmeans)
library(lmtest)
library(sandwich)




#Laden des Pakets
data <- read.csv("C:/Temp/data_ohne WAD, nur bis ID 645.csv",
                 sep = ",",
                 header = TRUE)

str(data)


#Vorbereitungen----
## pain ----
# Mittelwert aus paindetect1-3 
data <- data %>%
  mutate(
    pain_avg = rowMeans(across(c(paindetect1, paindetect2, paindetect3)), na.rm = TRUE)
  )
ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"
T2 <- "fragebogen_t2_arm_1"
T3 <- "fragebogen_t3_arm_1"
T4 <- "finaler_fragebogen_arm_1" 


#sample characteristics----

##age----
# Eventspalte und gewünschtes Event definieren
ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"

# Nur die Zeilen aus dem T1-Fragebogen auswählen
data_age <- data %>%
  filter(.data[[ev]] == T1) %>%
  mutate(
    age = as.numeric(age),     # sicherstellen, dass age numerisch ist
    gender = case_when(        # 1=male, 2=female recode
      gender == 1 ~ "male",
      gender == 2 ~ "female",
      TRUE ~ NA_character_
    )
  )

# Hilfsfunktion für zusammenfassung
summarize_age <- function(df, group_label) {
  v <- df$age
  
  qs <- if (all(is.na(v))) rep(NA_real_, 3) else
    as.numeric(quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))
  
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    group  = group_label,
    n      = sum(!is.na(v)),
    mean   = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd     = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range  = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    median = sprintf("%.2f", qs[2]),
    q_97.5 = sprintf("%.2f", qs[3])
  )
}

# Gesamte + nach Gender getrennte Ausgabe
age_summary <- bind_rows(
  summarize_age(data_age, "Total"),
  summarize_age(filter(data_age, gender == "female"), "Female"),
  summarize_age(filter(data_age, gender == "male"), "Male")
)

# Ausgabe
print(age_summary)


## Gender----
ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"

# Nur Zeilen aus dem Fragebogen T1 auswählen
data_gender <- data %>%
  filter(.data[[ev]] == T1) %>%
  mutate(gender = as.factor(gender))

# Häufigkeiten und Prozentwerte berechnen
gender_counts <- table(data_gender$gender, useNA = "no")
gender_percentages <- round(100 * gender_counts / sum(gender_counts), 2)

# DataFrame mit beschrifteten Ergebnissen
gender_df <- data.frame(
  gender = names(gender_counts),
  count = as.numeric(gender_counts),
  percentage = as.numeric(gender_percentages)
)

# Ausgabe
print(gender_df)


## smoking----

# Labels definieren
smoking_labels <- c(
  "1" = "Raucher",
  "2" = "Gelegenheitsraucher",
  "3" = "Nichtraucher"
)
ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"

# Nur Zeilen aus dem Fragebogen T1 auswählen
data_smoking <- data %>%
  filter(.data[[ev]] == T1) %>%
  mutate(smoking = as.character(smoking)) 

# Häufigkeiten & Prozente berechnen
smoking_counts <- table(data_smoking$smoking, useNA = "no")
smoking_percentages <- round(100 * smoking_counts / sum(smoking_counts), 2)

# DataFrame mit Labels erstellen
smoking_df <- data.frame(
  smoking_code = names(smoking_counts),
  smoking_status = smoking_labels[names(smoking_counts)],
  count = as.vector(smoking_counts),
  percentage = as.vector(smoking_percentages)
)

# Ausgabe
print(smoking_df)


## education----
ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"

# Nur Zeilen aus dem Fragebogen T1 auswählen
data_education <- data %>%
  filter(.data[[ev]] == T1) %>%
  mutate(education = as.character(education))  


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
  education_code = names(education_counts),
  education_label = education_labels[names(education_counts)],
  count = as.vector(education_counts),
  percentage = as.vector(education_percentages)
)

# Ausgabe
print(education_df)


## occuptation----
ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"
# Nur Zeilen aus dem Fragebogen T1 auswählen
data_occupation <- data %>%
  filter(.data[[ev]] == T1) %>%
  mutate(occupation = as.character(occupation))
# Labels für die Bildungskategorien
occupation_labels <- c(
  "1" = "Teilzeit",
  "2" = "Vollzeit",
  "3" = "Arbeitslos",
  "4" = "in Ausbildung",
  "5" = "Hausfrau/mann",
  "6" = "IV",
  "7" = "Rente"
)

# Zähle die Häufigkeiten
occupation_counts <- table(data_occupation$occupation, useNA = "no")

# Berechne die Prozentsätze
occupation_percentages <- round(occupation_counts / sum(occupation_counts) * 100, 2)

# Erstelle DataFrame
occupation_df <- data.frame(
  occupation_code = names(occupation_counts),
  occupation_label = occupation_labels[names(occupation_counts)],
  count = as.vector(occupation_counts),
  percentage = as.vector(occupation_percentages)
)
#Ausgabe
print(occupation_df)



##medication----
## medication (nur T1: fragebogen_t1_arm_1) ----

ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"

# 1) Alle Medication-Checkbox-Spalten finden (z.B. medication___0 ... medication___5)
med_cols <- grep("^medication___\\d+$", names(data), value = TRUE)

# 2) Labels für die Suffixe
suffix_labels <- c(
  "0" = "Keine Medikamente",
  "1" = "Schmerzmedikation",
  "2" = "Opioide",
  "3" = "Antidepressiva",
  "4" = "Muskelrelaxanzien",
  "5" = "Cannabis"
)

# 3) T1 filtern und nur die Checkboxen + ID behalten
med_t1 <- data %>%
  filter(.data[[ev]] == T1) %>%
  select(study_id, all_of(med_cols)) %>%
  mutate(across(all_of(med_cols), ~ suppressWarnings(as.numeric(as.character(.)))))


# 4) Lange Form + Summaries
med_summary <- med_t1 %>%
  pivot_longer(cols = all_of(med_cols), names_to = "med_col", values_to = "val") %>%
  mutate(
    suffix = str_extract(med_col, "\\d+$"),
    medication = suffix_labels[suffix]
  ) %>%
  group_by(medication) %>%
  summarise(
    n_nonmissing = sum(!is.na(val)),       # Anzahl mit gültigem Wert
    yes          = sum(val == 1, na.rm = TRUE),
    no           = sum(val == 0, na.rm = TRUE),
    pct_yes      = round(100 * yes / (yes + no), 2),  # unter den gültigen
    pct_no       = round(100 * no  / (yes + no), 2),
    # bei 0/1 entspricht mean dem Anteil "Ja"
    mean         = round(mean(val, na.rm = TRUE), 3),
    sd           = round(sd(val,   na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(yes))

med_summary


##medcare----

ev <- "redcap_event_name"
T1 <- "fragebogen_t1_arm_1"

# 1) alle care-Checkbox-Spalten finden (care___1 ... care___7)
care_cols <- grep("^care___\\d+$", names(data), value = TRUE)

# 2) Labels für die Suffixe (anpassen, falls nötig)
care_labels <- c(
  "1" = "Massage",
  "2" = "Psychologische/Psychiatrische Betreuung",
  "3" = "Chiropraktik",
  "4" = "Physiotherapie",
  "5" = "Fachärzt:in (Spezialist:in)",
  "6" = "Hausärzt:in (GP)",
  "7" = "Keine Versorgung"
)

# 3) T1 filtern
care_t1 <- data %>%
  filter(.data[[ev]] == T1) %>%
  select(study_id, all_of(care_cols)) %>%
  mutate(across(all_of(care_cols), ~ suppressWarnings(as.numeric(as.character(.)))))

# 4) Lange Form + Summaries
care_summary <- care_t1 %>%
  pivot_longer(cols = all_of(care_cols), names_to = "care_col", values_to = "val") %>%
  mutate(
    suffix = str_extract(care_col, "\\d+$"),
    care_label = care_labels[suffix]
  ) %>%
  group_by(care_label) %>%
  summarise(
    n_nonmissing = sum(!is.na(val)),
    yes          = sum(val == 1, na.rm = TRUE),
    no           = sum(val == 0, na.rm = TRUE),
    pct_yes      = round(100 * yes / (yes + no), 2),
    pct_no       = round(100 * no  / (yes + no), 2),
    mean         = round(mean(val, na.rm = TRUE), 3),  # bei 0/1 = Anteil "Ja"
    sd           = round(sd(val,   na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(yes))

care_summary



##pain----

## 1) Eventspalte & Zeitpunkte
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper: Zusammenfassung für gegebene Variablen
summarize_vars <- function(df, tp_label, vars) {
  map_dfr(vars, function(var) {
    v <- df[[var]]
    if (is.null(v)) v <- NA_real_  # falls Spalte fehlt
    qs <- if (all(is.na(v))) rep(NA_real_, 5) else
      as.numeric(stats::quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
    rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
    
    tibble(
      timepoint = tp_label,
      variable  = var,
      n         = sum(!is.na(v)),
      mean      = round(mean(v, na.rm = TRUE), 2),
      sd        = round(stats::sd(v, na.rm = TRUE), 2),
      range     = paste(rng, collapse = " - "),
      q_2.5     = qs[1],
      q_25      = qs[2],
      q_50      = qs[3],
      q_75      = qs[4],
      q_97.5    = qs[5]
    )
  })
}

## 3) Für alle Zeitpunkte zusammenfassen (Items + pain_avg)
vars_to_summarize <- c("pain_avg")

pain_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())  # falls ein Zeitpunkt gar nicht existiert
  summarize_vars(df_tp, tp_name, vars_to_summarize)
})

## 4) Ausgabe
print(pain_summary)

#LongFormat
pain_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    pain_avg = suppressWarnings(as.numeric(pain_avg))
  ) %>%
  dplyr::filter(!is.na(pain_avg))

###Boxplot ----
ggplot(pain_long, aes(x = timepoint, y = pain_avg)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Pain Average across Time Points",
    x = "timepoint",
    y = "Pain Average"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

###ANOVA ----
# Varianzhomogenität oft fraglich -> Welch-Test robuster:
anova_welch <- oneway.test(pain_avg ~ timepoint, data = pain_long, var.equal = FALSE)
anova_welch
# LMM für wiederholte Messungen
m_lmm_pain <- lmer(pain_avg ~ timepoint + (1 | study_id), data = pain_long, REML = TRUE)
summary(m_lmm_pain)
anova(m_lmm_pain)    # Gesamt-Test auf Zeitpunkteffekt
# Post-hoc-Vergleiche zwischen Zeitpunkten (Tukey, mit Satterthwaite-DF über lme4/emmeans)
emm_tp <- emmeans(m_lmm_pain, ~ timepoint)
pairs(emm_tp, adjust = "tukey")
# (Optional) Geschätzte Mittel pro Zeitpunkt
summary(emm_tp)



##disability----

## 1) Eventspalte & Zeitpunkte
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper-Funktion
summarize_disability <- function(df, tp_label) {
  v <- df$ndiscore
  
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n   = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
disability_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())  # falls ein Zeitpunkt fehlt
  summarize_disability(df_tp, tp_name)
})

## 4) Ergebnis
print(disability_summary)


## Long-Format für Grafiken & Modelle  (robuste Variante)
tp_map <- tibble::tibble(
  redcap_event_name = c("fragebogen_t1_arm_1",
                        "fragebogen_t2_arm_1",
                        "fragebogen_t3_arm_1",
                        "finaler_fragebogen_arm_1"),
  timepoint = factor(c("T1","T2","T3","T4"),
                     levels = c("T1","T2","T3","T4"))
)

dis_long <- data %>%
  dplyr::inner_join(tp_map, by = "redcap_event_name") %>%
  dplyr::transmute(
    study_id,
    timepoint,
    ndiscore = suppressWarnings(as.numeric(ndiscore))
  ) %>%
  dplyr::filter(!is.na(ndiscore))

### Boxplot ----
ggplot(dis_long, aes(x = timepoint, y = ndiscore)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Disability across Time Points",
    x = "timepoint",
    y = "NDI Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

###ANOVA----
## 6A) Einfache One-Way-ANOVA (unabhängige Gruppen; ignoriert Wiederholungen)
# Varianzhomogenität oft fraglich -> Welch-Test robuster:
anova_welch <- oneway.test(ndiscore ~ timepoint, data = dis_long, var.equal = FALSE)
anova_welch


## 6B) Wiederholte Messungen: LMM 
# erlaubt unbalancierte Daten (nicht alle haben T1–T4)
m_lmm_ndi <- lmer(ndiscore ~ timepoint + (1 | study_id), data = dis_long, REML = TRUE)
summary(m_lmm_ndi)
anova(m_lmm_ndi)    # Gesamt-Test auf Zeitpunkteffekt

# Post-hoc-Vergleiche zwischen Zeitpunkten (Tukey, mit Satterthwaite-DF über lme4/emmeans)
emm_tp <- emmeans(m_lmm_ndi, ~ timepoint)
pairs(emm_tp, adjust = "tukey")

# (Optional) Geschätzte Mittel pro Zeitpunkt
summary(emm_tp)





##depression-scale: DASS21----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper
summarize_depression <- function(df, tp_label) {
  v <- df$dass21_depression
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n   = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
depression_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_depression(df_tp, tp_name)
})

## 4) Ergebnis
print(depression_summary)



## Long-Format vorbereiten
depr_long <- data %>%
  filter(redcap_event_name %in% timepoints) %>%
  mutate(
    timepoint = dplyr::recode(redcap_event_name,
                              "fragebogen_t1_arm_1" = "T1",
                              "fragebogen_t2_arm_1" = "T2",
                              "fragebogen_t3_arm_1" = "T3",
                              "finaler_fragebogen_arm_1" = "T4"
    )
  ) %>%
  select(study_id, timepoint, dass21_depression) %>%
  mutate(dass21_depression = as.numeric(dass21_depression)) %>%
  filter(!is.na(dass21_depression))


### Boxplot----
ggplot(depr_long, aes(x = timepoint, y = dass21_depression)) +
  geom_boxplot(fill = "#3498db", alpha = 0.6, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  labs(
    title = "Depression scores (DASS-21) over time",
    x = "Time point",
    y = "Depression score"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title.position = "plot")

## Deskriptive Statistik
depr_long %>%
  group_by(timepoint) %>%
  summarise(
    n = n(),
    mean = mean(dass21_depression, na.rm = TRUE),
    sd = sd(dass21_depression, na.rm = TRUE)
  )

###  ANOVA ----
# Prüfen, ob Personen mehrfach gemessen wurden
id_counts <- depr_long %>% count(study_id) %>% pull(n)
if (all(id_counts > 1)) {
  message("→ Wiederholte Messungen pro Person erkannt: within-subject ANOVA")
  
  anova_res <- depr_long %>%
    anova_test(dv = dass21_depression, wid = study_id, within = timepoint)
  
} else {
  message("→ Ungleiche Stichproben: between-subject ANOVA")
  
  anova_res <- depr_long %>%
    anova_test(dv = dass21_depression, between = timepoint)
}

anova_res

###  Post-hoc-Vergleiche
posthoc_res <- depr_long %>%
  tukey_hsd(dass21_depression ~ timepoint)

posthoc_res


##stress-scale: DASS21----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper-Funktion
summarize_stress <- function(df, tp_label) {
  v <- df$dass21_stress
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n    = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
dass21_stress_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_stress(df_tp, tp_name)
})

## 4) Ergebnis
print(dass21_stress_summary)


# Long-Format
stress_long <- data %>%
  dplyr::filter(.data[[ev]] %in% timepoints) %>%
  dplyr::mutate(
    timepoint = dplyr::case_when(
      .data[[ev]] == "fragebogen_t1_arm_1" ~ "T1",
      .data[[ev]] == "fragebogen_t2_arm_1" ~ "T2",
      .data[[ev]] == "fragebogen_t3_arm_1" ~ "T3",
      .data[[ev]] == "finaler_fragebogen_arm_1" ~ "T4",
      TRUE ~ NA_character_
    ),
    dass21_stress = suppressWarnings(as.numeric(dass21_stress))
  ) %>%
  dplyr::filter(!is.na(dass21_stress)) %>%
  dplyr::mutate(timepoint = factor(timepoint, levels = c("T1","T2","T3","T4"))) %>%
  dplyr::select(study_id, timepoint, dass21_stress)

# Boxplot (nach Zeitpunkten)
ggplot2::ggplot(stress_long, ggplot2::aes(x = timepoint, y = dass21_stress)) +
  ggplot2::geom_boxplot(fill = "#3498db", outlier.alpha = 0.35, width = 0.6) +
  ggplot2::geom_jitter(width = 0.08, alpha = 0.35) +
  ggplot2::labs(x = "timepoint", y = "DASS-21 Stress Score", title = "Stress Scores across Time Points") +
  ggplot2::theme_minimal(base_size = 13)

###ANOVA ----
stress_wide <- tidyr::pivot_wider(
  stress_long,
  id_cols = study_id,
  names_from = timepoint,
  values_from = dass21_stress
)

have_all <- stats::complete.cases(stress_wide[, c("T1","T2","T3","T4")])
stress_wide_cc <- stress_wide[have_all, ]

if (nrow(stress_wide_cc) >= 3) {
  # a) klassische wiederholte-Messungen ANOVA
  stress_long_cc <- stress_wide_cc %>%
    tidyr::pivot_longer(cols = c(T1,T2,T3,T4), names_to = "timepoint", values_to = "stress") %>%
    dplyr::mutate(timepoint = factor(timepoint, levels = c("T1","T2","T3","T4")))
  
  aov_stress <- aov(stress ~ timepoint + Error(study_id/timepoint), data = stress_long_cc)
  print(aov_stress)
  
  # b) Post-hoc (gepaarte t-Tests, Holm-korrigiert)
  mat <- as.matrix(stress_wide_cc[, c("T1","T2","T3","T4")])
  posthoc <- pairwise.t.test(x = c(mat),
                             g = factor(rep(colnames(mat), each = nrow(mat)), levels = c("T1","T2","T3","T4")),
                             paired = TRUE, p.adjust.method = "holm")
  print(posthoc)
} else {
  message("Zu wenige vollständige Fälle für gepaarte ANOVA (benötigt >= 3 Proband:innen mit T1–T4).")
}

# RM-ANOVA korrekt mit p-Wert anzeigen
aov_out <- summary(aov_stress)   # statt print(aov_stress)

# p-Wert des Within-Faktors (timepoint) aus dem zweiten Stratum ziehen:
p_rm <- aov_out[[2]][[1]][["Pr(>F)"]][1]

# hübsch formatieren (APA-Konvention)
format_p <- function(p) ifelse(is.na(p), NA_character_,
                               ifelse(p < 0.001, "< .001", sprintf("%.3f", p)))

p_rm_fmt <- format_p(p_rm)

# p-Wert-Spalte an deine Deskriptiv-Tabelle anhängen (gleicher p für alle Reihen)
dass21_stress_summary <- dass21_stress_summary %>%
  dplyr::mutate(p_rm_anova = p_rm_fmt)

print(dass21_stress_summary)




##Anxiety: STAI-S----

## Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## Helper-Funktion
summarize_stai <- function(df, tp_label) {
  v <- df$stais_score
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n    = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## Zusammenfassung über alle Zeitpunkte
stais_all_timepoints <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_stai(df_tp, tp_name)
})

## Ausgabe
print(stais_all_timepoints)

##Long-Format
stai_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    stais_score = suppressWarnings(as.numeric(stais_score))
  ) %>%
  dplyr::filter(!is.na(stais_score))

###Boxplot ----
ggplot(stai_long, aes(x = timepoint, y = stais_score)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Anxiety across Time Points",
    x = "timepoint",
    y = "STAI-S Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

###ANOVA ----
# Varianzhomogenität oft fraglich -> Welch-Test robuster:
anova_welch <- oneway.test(stais_score ~ timepoint, data = stai_long, var.equal = FALSE)
anova_welch
# LMM für wiederholte Messungen
m_lmm_stai <- lmer(stais_score ~ timepoint + (1 | study_id), data = stai_long, REML = TRUE)
summary(m_lmm_stai)
anova(m_lmm_stai)    # Gesamt-Test auf Zeitpunkteffekt






##Pain Vigilance: PVAQ----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper-Funktion
summarize_pvaq <- function(df, tp_label) {
  v <- df$pvaq_score_total
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n    = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
pvaq_all_timepoints <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_pvaq(df_tp, tp_name)
})

## 4) Ausgabe
print(pvaq_all_timepoints)

###Long-Format
pvaq_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    pvaq_score_total = suppressWarnings(as.numeric(pvaq_score_total))
  ) %>%
  dplyr::filter(!is.na(pvaq_score_total))

###Boxplot ----
ggplot(pvaq_long, aes(x = timepoint, y = pvaq_score_total)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Pain Vigilance across Time Points",
    x = "timepoint",
    y = "PVAQ Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

###ANOVA ----
# Welch-Test
anova_welch <- oneway.test(pvaq_score_total ~ timepoint, data = pvaq_long, var.equal = FALSE)
anova_welch
# LMM für wiederholte Messungen
m_lmm_pvaq <- lmer(pvaq_score_total ~ timepoint + (1 | study_id), data = pvaq_long, REML = TRUE)
summary(m_lmm_pvaq)
anova(m_lmm_pvaq)    


##Physical Activity----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper-Funktion
summarize_met_total <- function(df, tp_label) {
  v <- df$met_total
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n    = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
met_total_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_met_total(df_tp, tp_name)
})

## 4) Ausgabe
print(met_total_summary)

#Long-Format
met_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    met_total = suppressWarnings(as.numeric(met_total))
  ) %>%
  dplyr::filter(!is.na(met_total))

###Boxplot ----
met_long_filtered <- met_long %>%
  dplyr::filter(!is.na(met_total) & met_total <= 20000)

ggplot(met_long_filtered, aes(x = timepoint, y = met_total)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.shape = NA) +   
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) + 
  labs(
    title = "Physical Activity across Time Points",
    x = "timepoint",
    y = "MET Total Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

###ANOVA----
# Welch-Test
anova_welch <- oneway.test(met_total ~ timepoint, data = met_long_filtered, var.equal = FALSE)
anova_welch

# LMM für wiederholte Messungen
m_lmm_met <- lmer(met_total ~ timepoint + (1 | study_id), data = met_long_filtered, REML = TRUE)
summary(m_lmm_met)
anova(m_lmm_met)    
# Post-hoc-Vergleiche zwischen Zeitpunkten
emm_tp <- emmeans(m_lmm_met, ~ timepoint)
pairs(emm_tp, adjust = "tukey")




##Sedentary activity----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper-Funktion
summarize_sedentary <- function(df, tp_label) {
  v <- df$total_sitting_mins
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n    = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
sedentary_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_sedentary(df_tp, tp_name)
})

## 4) Ausgabe
print(sedentary_summary)

# wide format for paired test
sed_wide <- sedentary_long %>%
  tidyr::pivot_wider(
    id_cols = study_id,
    names_from = timepoint,
    values_from = total_sitting_mins
  ) %>%
  filter(!is.na(T1), !is.na(T4))   # keep complete cases for T1 & T4

# paired t-test
t_T1_T4 <- t.test(sed_wide$T1, sed_wide$T4, paired = TRUE)

t_T1_T4

p_raw <- t_T1_T4$p.value

format_p <- function(p) {
  if (p < 0.001) "< .001" else sprintf("%.3f", p)
}

p_T1_T4 <- format_p(p_raw)
p_T1_T4

sedentary_summary <- sedentary_summary %>%
  mutate(p_T1_T4 = if_else(timepoint == "T4", p_T1_T4, NA_character_))
print(sedentary_summary)









##Long-Format
sedentary_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    total_sitting_mins = suppressWarnings(as.numeric(total_sitting_mins))
  ) %>%
  dplyr::filter(!is.na(total_sitting_mins))

###Boxplot ----
sedentary_long_filtered <- sedentary_long %>%
  dplyr::filter(!is.na(total_sitting_mins) & total_sitting_mins <= 1500)

ggplot(sedentary_long_filtered, aes(x = timepoint, y = total_sitting_mins)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Sedentary Activity across Time Points",
    x = "timepoint",
    y = "Total Sitting Minutes Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )
###ANOVA----
# Welch-Test
anova_welch <- oneway.test(total_sitting_mins ~ timepoint, data = sedentary_long, var.equal = FALSE)
anova_welch
# LMM für wiederholte Messungen
m_lmm_sedentary <- lmer(total_sitting_mins ~ timepoint + (1 | study_id), data = sedentary_long, REML = TRUE)
summary(m_lmm_sedentary)
anova(m_lmm_sedentary)    
# Post-hoc-Vergleiche zwischen Zeitpunkten
emm_tp <- emmeans(m_lmm_sedentary, ~ timepoint)
pairs(emm_tp, adjust = "tukey")




##Sleep----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper-Funktion
summarize_sleep <- function(df, tp_label) {
  v <- df$sleepquality
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n    = sum(!is.na(v)),
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
sleep_summary <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  if (nrow(df_tp) == 0) return(tibble())
  summarize_sleep(df_tp, tp_name)
})

## 4) Ausgabe
print(sleep_summary)

##Long-Format
sleep_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    sleepquality = suppressWarnings(as.numeric(sleepquality))
  ) %>%
  dplyr::filter(!is.na(sleepquality))

###Boxplot ----
ggplot(sleep_long, aes(x = timepoint, y = sleepquality)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Sleep Quality across Time Points",
    x = "timepoint",
    y = "Sleep Quality Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

###ANOVA----
# Welch-Test
anova_welch <- oneway.test(sleepquality ~ timepoint, data = sleep_long, var.equal = FALSE)
anova_welch
# LMM für wiederholte Messungen
m_lmm_sleep <- lmer(sleepquality ~ timepoint + (1 | study_id), data = sleep_long, REML = TRUE)
summary(m_lmm_sleep)
anova(m_lmm_sleep)    
# Post-hoc-Vergleiche zwischen Zeitpunkten
emm_tp <- emmeans(m_lmm_sleep, ~ timepoint)
pairs(emm_tp, adjust = "tukey")


##SCI----
###uncertainty----
## Helper 
clean_num <- function(x) {
  x <- as.character(x)
  x[x %in% c("NA", ".", "")] <- NA
  suppressWarnings(as.numeric(x))
}

## SCI-Items korrekt auswählen 
s1_items <- paste0("sci_stress1_", 1:7)
s2_items <- paste0("sci_stress2_", 1:6)

# prüfen, welche wirklich existieren
s1_items <- intersect(s1_items, names(data))
s2_items <- intersect(s2_items, names(data))

## Umkodieren 
rec_01_to_1k <- function(x, k) { ((k - 1) * (x - 0) / (k - 0)) + 1 }

data <- data %>%
  mutate(across(all_of(c(s1_items, s2_items)), clean_num)) %>%
  mutate(
    across(all_of(s1_items), ~ rec_01_to_1k(., 7), .names = "{.col}_new"),
    across(all_of(s2_items), ~ rec_01_to_1k(., 6), .names = "{.col}_new")
  )

## Summen bilden 
s1_new <- paste0(s1_items, "_new")
s2_new <- paste0(s2_items, "_new")

data <- data %>%
  mutate(
    stress_skala1_sum = if (length(s1_new)) rowSums(across(all_of(s1_new)), na.rm = TRUE) else NA_real_,
    stress_skala2_sum = if (length(s2_new)) rowSums(across(all_of(s2_new)), na.rm = TRUE) else NA_real_
  )

## Zusammenfassung über Zeitpunkte
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

# Long-Format
stress1_long <- data %>%
  dplyr::filter(.data[[ev]] %in% timepoints) %>%
  dplyr::mutate(
    timepoint = dplyr::recode(.data[[ev]],
                              "fragebogen_t1_arm_1" = "T1",
                              "fragebogen_t2_arm_1" = "T2",
                              "fragebogen_t3_arm_1" = "T3",
                              "finaler_fragebogen_arm_1" = "T4"
    ),
    timepoint = factor(timepoint, levels = c("T1","T2","T3","T4")),
    stress_skala1_sum = as.numeric(stress_skala1_sum)
  ) %>%
  dplyr::select(study_id, timepoint, stress_skala1_sum) %>%
  dplyr::filter(!is.na(stress_skala1_sum))

# Zusammenfassung
stress1_summary <- stress1_long %>%
  dplyr::group_by(timepoint) %>%
  dplyr::summarise(
    n    = sum(!is.na(stress_skala1_sum)),
    mean = sprintf("%.2f", mean(stress_skala1_sum, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(stress_skala1_sum, na.rm = TRUE)),
    range = {
      rng <- range(stress_skala1_sum, na.rm = TRUE)
      paste(sprintf("%.2f", rng), collapse = " - ")
    },
    q_2.5  = sprintf("%.2f", stats::quantile(stress_skala1_sum, 0.025, na.rm = TRUE)),
    q_25   = sprintf("%.2f", stats::quantile(stress_skala1_sum, 0.25,  na.rm = TRUE)),
    q_50   = sprintf("%.2f", stats::quantile(stress_skala1_sum, 0.50,  na.rm = TRUE)),
    q_75   = sprintf("%.2f", stats::quantile(stress_skala1_sum, 0.75,  na.rm = TRUE)),
    q_97.5 = sprintf("%.2f", stats::quantile(stress_skala1_sum, 0.975, na.rm = TRUE)),
    .groups = "drop"
  )

print(stress1_summary)


###Boxplot T1–T4 ----
ggplot2::ggplot(stress1_long, ggplot2::aes(x = timepoint, y = stress_skala1_sum)) +
  ggplot2::geom_boxplot(fill = "#3498db", outlier.alpha = 0.5) +
  ggplot2::geom_jitter(width = 0.12, alpha = 0.35, size = 1) +
  ggplot2::labs(
    title = "Stress due to Uncertainty across Time Points",
    x = "timepoint", y = "SCI: Sum of Items"
  ) +
  ggplot2::theme_minimal(base_size = 12)

###ANOVA----
aov_res <- stats::aov(stress_skala1_sum ~ timepoint, data = stress1_long)
summary(aov_res)

# Kruskal-Wallis
kw_res <- stats::kruskal.test(stress_skala1_sum ~ timepoint, data = stress1_long)
kw_res



###excessive demands----

# FIX: Helper, der einen NUMERISCHEN VEKTOR erwartet
summarize_num_tp <- function(vec, tp_lab) {
  vec <- suppressWarnings(as.numeric(vec))
  if (length(vec) == 0 || all(is.na(vec))) {
    return(tibble::tibble(
      timepoint = tp_lab, n = 0, mean = NA_character_, sd = NA_character_,
      range = "NA - NA", q_2.5 = NA_character_, q_25 = NA_character_,
      q_50 = NA_character_, q_75 = NA_character_, q_97.5 = NA_character_
    ))
  }
  qs  <- stats::quantile(vec, probs = c(.025,.25,.5,.75,.975), na.rm = TRUE)
  rng <- range(vec, na.rm = TRUE)
  tibble::tibble(
    timepoint = tp_lab,
    n    = sum(!is.na(vec)),
    mean = sprintf("%.2f", mean(vec, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(vec,   na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

#AUFRUF: vektor übergeben, nicht den ganzen DF
stress_excessive_demands_all <- purrr::imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- dplyr::filter(data, .data[[ev]] == ev_value)
  summarize_num_tp(df_tp$stress_skala2_sum, tp_name)
})

print(stress_excessive_demands_all)




ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)
# Long-Format
stress2_long <- data %>%
  dplyr::filter(.data[[ev]] %in% timepoints) %>%
  dplyr::mutate(
    timepoint = dplyr::recode(.data[[ev]],
                              "fragebogen_t1_arm_1"       = "T1",
                              "fragebogen_t2_arm_1"       = "T2",
                              "fragebogen_t3_arm_1"       = "T3",
                              "finaler_fragebogen_arm_1"  = "T4"
    ),
    timepoint = factor(timepoint, levels = c("T1","T2","T3","T4")),
    stress_skala2_sum = as.numeric(stress_skala2_sum)
  ) %>%
  dplyr::select(study_id, timepoint, stress_skala2_sum) %>%
  dplyr::filter(!is.na(stress_skala2_sum))


# Boxplot T1–T4
ggplot2::ggplot(stress2_long, ggplot2::aes(x = timepoint, y = stress_skala2_sum)) +
  ggplot2::geom_boxplot(fill = "#3498db", outlier.alpha = 0.5) +
  ggplot2::geom_jitter(width = 0.12, alpha = 0.35, size = 1) +
  ggplot2::labs(
    title = "Stress due to Excessive Demands across Time Points",
    x = "timepoint", y = "SCI: Sum of Items"
  ) +
  ggplot2::theme_minimal(base_size = 12)


### ANOVA----
summary(aov(stress_skala2_sum ~ timepoint, data = stress2_long))
# Robust/ohne Normalannahme:
kruskal.test(stress_skala2_sum ~ timepoint, data = stress2_long)



###symptoms----

## 1) Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

## 2) Helper: Zusammenfassung + 95%-CI (nur wenn n > 1)
summarize_symptoms <- function(df, tp_label) {
  v <- df$gesamtscore_symptom
  n  <- sum(!is.na(v))
  if (n == 0) {
    return(tibble(
      timepoint = tp_label, n = 0, mean = NA_real_, sd = NA_real_,
      range = "NA - NA", q_2.5 = NA_real_, q_25 = NA_real_, q_50 = NA_real_,
      q_75 = NA_real_, q_97.5 = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_
    ))
  }
  m   <- mean(v, na.rm = TRUE)
  s   <- sd(v, na.rm = TRUE)
  rng <- range(v, na.rm = TRUE)
  qs  <- as.numeric(quantile(v, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  z   <- qnorm(0.975)
  ciL <- if (n > 1) m - z * s / sqrt(n) else NA_real_
  ciU <- if (n > 1) m + z * s / sqrt(n) else NA_real_
  
  tibble(
    timepoint = tp_label,
    n = n,
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 3) Zusammenfassung über alle Zeitpunkte
stress_symptoms_all <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  summarize_symptoms(df_tp, tp_name)
})

## 4) Ergebnis
print(stress_symptoms_all)

#Long-Format
symptoms_long <- data %>%
  dplyr::inner_join(
    tibble::tibble(
      redcap_event_name = c("fragebogen_t1_arm_1",
                            "fragebogen_t2_arm_1",
                            "fragebogen_t3_arm_1",
                            "finaler_fragebogen_arm_1"),
      timepoint = factor(c("T1","T2","T3","T4"),
                         levels = c("T1","T2","T3","T4"))
    ),
    by = "redcap_event_name"
  ) %>%
  dplyr::transmute(
    study_id,
    timepoint,
    gesamtscore_symptom = suppressWarnings(as.numeric(gesamtscore_symptom))
  ) %>%
  dplyr::filter(!is.na(gesamtscore_symptom))

###Boxplot ----
ggplot(symptoms_long, aes(x = timepoint, y = gesamtscore_symptom)) +
  geom_boxplot(fill = "#3498db", width = 0.6, outlier.alpha = 0.4) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 1) +
  labs(
    title = "Physical and psychological Stress Symptoms due across Time Points",
    x = "timepoint",
    y = "SCI: Sum of items"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold")
  )

### ANOVA----
summary(aov(gesamtscore_symptom ~ timepoint, data = symptoms_long))
# Robust/ohne Normalannahme:
kruskal.test(gesamtscore_symptom ~ timepoint, data = symptoms_long)


##activity patterns----

## Vorbereitung: Events definieren
ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

# Funktion zur Bildung der Aktivitätsmuster
mk_activity_pattern <- function(df) {
  df %>%
    mutate(
      activity_pattern = case_when(
        score_pps < 3 & dms_score == 2 ~ "FAR",
        score_pps >= 3 & dms_score == 2 ~ "DER",
        score_pps >= 3 & dms_score < 2 ~ "EER",
        score_pps < 3 & dms_score < 2 ~ "AR",
        TRUE ~ NA_character_
      ),
      activity_pattern = factor(activity_pattern, levels = c("AR", "EER", "DER", "FAR"))
    )
}

## Absolute Häufigkeiten pro Zeitpunkt (NA ausschließen) 
activity_counts <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>%
    filter(.data[[ev]] == ev_value) %>%
    mk_activity_pattern() %>%
    filter(!is.na(activity_pattern)) 
  
  df_tp %>%
    count(activity_pattern, name = "n") %>%
    complete(activity_pattern = factor(c("AR", "EER", "DER", "FAR"),
                                       levels = c("AR", "EER", "DER", "FAR")),
             fill = list(n = 0)) %>%
    mutate(timepoint = tp_name, .before = 1)
})

## Prozentwerte berechnen
activity_counts_prop <- activity_counts %>%
  group_by(timepoint) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

#Prozentwerte anzeigen
print(activity_counts_prop)

### Balkendiagramm ----

# Farbpalette 
group_colors <- c(
  "AR"  = "olivedrab1",  
  "EER" = "plum",        
  "DER" = "skyblue",     
  "FAR" = "grey"         
)

# Reihenfolge der Gruppen sicherstellen
activity_counts_prop$activity_pattern <- factor(
  activity_counts_prop$activity_pattern,
  levels = c("AR", "EER", "DER", "FAR")
)

# Plot
ggplot(activity_counts_prop, aes(x = activity_pattern, 
                                 y = percentage, 
                                 fill = activity_pattern)) +
  geom_col(position = position_dodge(width = 0.8),
           color = "black", width = 0.7) +
  facet_wrap(~ timepoint, nrow = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 100)) +
  scale_fill_manual(
    name = "Activity Pattern:",
    values = group_colors,
    labels = c(
      "AR  = Activity Pacing",
      "EER = Eustress Persistence",
      "DER = Distress Persistence",
      "FAR = Fear Avoidance"
    )
  ) +
  labs(
    title = "Activity Patterns across Time Points",
    x = "Activity Pattern",
    y = "Percentage of Participants (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0, size = 13),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right",                  
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 13)
  )

#Breiteres Format
ggsave("activity_patterns.png", width = 12, height = 6, dpi = 300)


## 4) Helper für Verteilungs-Stats
summarize_numeric <- function(v) {
  n <- sum(!is.na(v))
  if (n == 0) {
    return(tibble(
      n = 0, mean = NA_real_, sd = NA_real_, range = "NA - NA",
      q_2.5 = NA_real_, q_25 = NA_real_, q_50 = NA_real_, q_75 = NA_real_, q_97.5 = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_
    ))
  }
  m   <- mean(v, na.rm = TRUE)
  s   <- sd(v, na.rm = TRUE)
  rng <- range(v, na.rm = TRUE)
  qs  <- as.numeric(quantile(v, c(0.025,0.25,0.5,0.75,0.975), na.rm = TRUE))
  z   <- qnorm(0.975)
  tibble(
    n = n,
    mean = sprintf("%.2f", mean(v, na.rm = TRUE)),
    sd   = sprintf("%.2f", sd(v, na.rm = TRUE)),
    range = paste(sprintf("%.2f", rng), collapse = " - "),
    q_2.5  = sprintf("%.2f", qs[1]),
    q_25   = sprintf("%.2f", qs[2]),
    q_50   = sprintf("%.2f", qs[3]),
    q_75   = sprintf("%.2f", qs[4]),
    q_97.5 = sprintf("%.2f", qs[5])
  )
}

## 5) PPS/DMS – Häufigkeiten/Prozente & Verteilungs-Stats pro Zeitpunkt
pps_dms_freq <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  # Häufigkeiten
  pps_tab <- as.data.frame(table(df_tp$score_pps, useNA = "no")) %>%
    rename(level = Var1, count = Freq) %>%
    mutate(level = as.numeric(as.character(level))) %>%
    arrange(level) %>%
    mutate(percentage = round(100 * count / sum(count), 2), var = "score_pps", timepoint = tp_name, .before = 1)
  
  dms_tab <- as.data.frame(table(df_tp$dms_score, useNA = "no")) %>%
    rename(level = Var1, count = Freq) %>%
    mutate(level = as.numeric(as.character(level))) %>%
    arrange(level) %>%
    mutate(percentage = round(100 * count / sum(count), 2), var = "dms_score", timepoint = tp_name, .before = 1)
  
  bind_rows(pps_tab, dms_tab)
})

pps_dms_stats <- imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  bind_rows(
    summarize_numeric(df_tp$score_pps) %>% mutate(var = "score_pps", timepoint = tp_name, .before = 1),
    summarize_numeric(df_tp$dms_score)   %>% mutate(var = "dms_score",   timepoint = tp_name, .before = 1)
  )
})

# Ausgabe 2a: Häufigkeiten/Prozente
print(pps_dms_freq)
# Ausgabe 2b: Lage-/Streuungsmaße + CI
print(pps_dms_stats)

#Veränderung über die Zeit

ev <- "redcap_event_name"
timepoints <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

# Bowker (r×r) bzw. McNemar (2×2):
bowker_or_mcnemar_p <- function(tab) {
  stopifnot(is.matrix(tab) || is.table(tab))
  k <- nrow(tab)
  if (k != ncol(tab)) stop("Tabelle muss quadratisch sein (gleiche Kategorien in Zeilen/Spalten).")
  
  # 2x2 -> McNemar
  if (k == 2) {
    b <- tab[1,2]; c <- tab[2,1]
    # McNemar ohne Kontinuitätskorrektur:
    X2 <- (abs(b - c))^2 / (b + c)
    return(stats::pchisq(X2, df = 1, lower.tail = FALSE))
  }
  
  # r×r -> Bowker: sum_{i<j} ( (n_ij - n_ji)^2 / (n_ij + n_ji) ), df = k*(k-1)/2
  X2 <- 0
  df <- k * (k - 1) / 2
  for (i in 1:(k-1)) {
    for (j in (i+1):k) {
      nij <- tab[i,j]; nji <- tab[j,i]
      denom <- nij + nji
      if (denom > 0) X2 <- X2 + ( (nij - nji)^2 / denom )
    }
  }
  stats::pchisq(X2, df = df, lower.tail = FALSE)
}

# Hilfsfunktion: baut aus deinem Datensatz die T1×T4-Tafel und gibt p zurück
p_change_T1_T4_cat <- function(df, var, ev_col = "redcap_event_name",
                               t1 = "fragebogen_t1_arm_1", t4 = "finaler_fragebogen_arm_1") {
  dat <- df[df[[ev_col]] %in% c(t1, t4), c("study_id", ev_col, var)]
  names(dat) <- c("study_id", "ev", "val")
  dat <- dat[!is.na(dat$val), ]
  if (nrow(dat) == 0) return(NA_real_)
  
  # Long -> Wide (T1/T4), vollständige Paare
  dat$tp <- ifelse(dat$ev == t1, "T1", ifelse(dat$ev == t4, "T4", NA))
  dat <- dat[!is.na(dat$tp), ]
  dat <- dat[!duplicated(dat[c("study_id","tp")]), ]
  wide <- reshape(dat[, c("study_id","tp","val")], idvar = "study_id", timevar = "tp",
                  direction = "wide")
  if (!all(c("val.T1","val.T4") %in% names(wide))) return(NA_real_)
  wide <- wide[!is.na(wide$val.T1) & !is.na(wide$val.T4), ]
  if (nrow(wide) < 3) return(NA_real_)
  
  # Gemeinsame Levels (als Faktoren) festlegen
  lvls <- sort(unique(c(wide$val.T1, wide$val.T4)))
  T1f <- factor(wide$val.T1, levels = lvls)
  T4f <- factor(wide$val.T4, levels = lvls)
  
  tab <- table(T1f, T4f)
  bowker_or_mcnemar_p(tab)
}

# P-Werte für score_pps und dms_score:

data$score_pps_cat <- ifelse(is.na(data$score_pps), NA, round(data$score_pps))
data$dms_score_cat <- data$dms_score

p_pps <- p_change_T1_T4_cat(data, "score_pps_cat", ev, timepoints["T1"], timepoints["T4"])
p_dms <- p_change_T1_T4_cat(data, "dms_score_cat", ev, timepoints["T1"], timepoints["T4"])

format_p <- function(p) ifelse(is.na(p), NA_character_,
                               ifelse(p < 0.001, "< .001", sprintf("%.3f", p)))

pvals_T1_T4 <- tibble::tibble(
  var = c("score_pps", "dms_score"),
  p_value_raw = c(p_pps, p_dms),
  p_value = format_p(c(p_pps, p_dms))
)

print(pvals_T1_T4)




##HCC----

ev <- "redcap_event_name"
EV_T1_Q     <- "fragebogen_t1_arm_1"     
EV_HCC_T1   <- "untersuchung_t1_arm_1"   
EV_HCC_T2   <- "untersuchung_t3_arm_1"   
EV_HCC_T3   <- "untersuchung_t3_arm_1"   

#Gender-Lookup aus
gender_lookup <- data %>%
  filter(.data[[ev]] == EV_T1_Q) %>%
  distinct(study_id, gender)

## Ausreisser: 95%-Cutoff bei Referenzdaten von Dr.Prof Kirschbaum
cutoffs <- tribble(
  ~hormon,     ~sex,     ~p95,
  "cortisol",  "female", 22.14,
  "cortisol",  "male",   24.69,
  "cortisone", "female", 34.24,
  "cortisone", "male",   35.75
)

clean_num <- function(x){
  x <- as.character(x); x[x %in% c("","NA",".")] <- NA
  suppressWarnings(as.numeric(x))
}

recode_sex <- function(g){
  case_when(
    tolower(as.character(g)) %in% c("female","weiblich","f") ~ "female",
    tolower(as.character(g)) %in% c("male","männlich","m")   ~ "male",
    suppressWarnings(as.numeric(g)) == 2 ~ "female",
    suppressWarnings(as.numeric(g)) == 1 ~ "male",
    TRUE ~ NA_character_
  )
}

summarize_vec <- function(v, timepoint, sex, variable){
  v <- as.numeric(v); n <- sum(!is.na(v))
  if(n==0) return(tibble(timepoint, sex, variable, n=0, mean=NA_real_, sd=NA_real_,
                         range="NA - NA", q_2.5=NA_real_, q_25=NA_real_, q_50=NA_real_,
                         q_75=NA_real_, q_97.5=NA_real_, ci_lower=NA_real_, ci_upper=NA_real_))
  m <- mean(v, na.rm=TRUE); s <- sd(v, na.rm=TRUE); rg <- range(v, na.rm=TRUE)
  qs <- quantile(v, probs=c(.025,.25,.5,.75,.975), na.rm=TRUE); z <- qnorm(0.975)
  tibble(
    timepoint, sex, variable,
    n=n, mean=round(m,2), sd=round(s,2),
    range=paste(round(rg[1],2), "-", round(rg[2],2)),
    q_2.5=round(qs[1],2), q_25=round(qs[2],2), q_50=round(qs[3],2),
    q_75=round(qs[4],2), q_97.5=round(qs[5],2),
    ci_lower=round(m - z*s/sqrt(n),2), ci_upper=round(m + z*s/sqrt(n),2)
  )
}

process_hormones <- function(df, timepoint, cortisol_col, cortisone_col){
  df <- df %>%
    mutate(
      cortisol  = clean_num(.data[[cortisol_col]]),
      cortisone = clean_num(.data[[cortisone_col]]),
      sex = recode_sex(gender)
    )
  
  summaries <- map_dfr(c("female","male"), function(sex_i){
    cut_cort  <- cutoffs$p95[cutoffs$hormon=="cortisol"  & cutoffs$sex==sex_i]
    cut_corti <- cutoffs$p95[cutoffs$hormon=="cortisone" & cutoffs$sex==sex_i]
    d <- df %>% filter(sex==sex_i)
    
    v1_all <- d$cortisol;  v2_all <- d$cortisone
    v1_kept <- v1_all[is.na(v1_all) | v1_all <= cut_cort]
    v2_kept <- v2_all[is.na(v2_all) | v2_all <= cut_corti]
    
    bind_rows(
      summarize_vec(v1_kept, timepoint, sex_i, "cortisol"),
      summarize_vec(v2_kept, timepoint, sex_i, "cortisone")
    )
  })
  
  removed <- map_dfr(c("female","male"), function(sex_i){
    cut_cort  <- cutoffs$p95[cutoffs$hormon=="cortisol"  & cutoffs$sex==sex_i]
    cut_corti <- cutoffs$p95[cutoffs$hormon=="cortisone" & cutoffs$sex==sex_i]
    d <- df %>% filter(sex==sex_i)
    
    v1_all <- d$cortisol; v2_all <- d$cortisone
    n1_tot <- sum(!is.na(v1_all)); n2_tot <- sum(!is.na(v2_all))
    n1_in  <- sum(!is.na(v1_all) & v1_all <= cut_cort)
    n2_in  <- sum(!is.na(v2_all) & v2_all <= cut_corti)
    
    tibble(
      timepoint=timepoint, sex=sex_i,
      variable=c("cortisol","cortisone"),
      cutoff=c(cut_cort, cut_corti),
      n_total=c(n1_tot,n2_tot), n_kept=c(n1_in,n2_in),
      n_removed=c(n1_tot-n1_in, n2_tot-n2_in)
    )
  })
  
  list(summary=summaries, removed=removed)
}

#Datensätze strikt nach Event & Spalten 
# T1
data_T1 <- data %>%
  filter(.data[[ev]] == EV_HCC_T1) %>%
  select(study_id, cortisol1, cortisone1) %>%
  left_join(gender_lookup, by = "study_id")

# T2
data_T2 <- data %>%
  filter(.data[[ev]] == EV_HCC_T2) %>%
  filter(if_any(all_of(c("cortisol2","cortisone2")), ~ !is.na(.))) %>%
  select(study_id, cortisol2, cortisone2) %>%
  left_join(gender_lookup, by = "study_id")

# T3
data_T3 <- data %>%
  filter(.data[[ev]] == EV_HCC_T3) %>%
  filter(if_any(all_of(c("cortisol1","cortisone1")), ~ !is.na(.))) %>%
  select(study_id, cortisol1, cortisone1) %>%
  left_join(gender_lookup, by = "study_id")

#Quick-Check 
cat("\n> QUICK CHECK (Rows / non-NA / gender):\n")
cat("T1:", nrow(data_T1),
    "| cortisol1:", sum(!is.na(data_T1$cortisol1)),
    "| cortisone1:", sum(!is.na(data_T1$cortisone1)),
    "| gender:", sum(!is.na(data_T1$gender)), "\n")
cat("T2:", nrow(data_T2),
    "| cortisol2:", sum(!is.na(data_T2$cortisol2)),
    "| cortisone2:", sum(!is.na(data_T2$cortisone2)),
    "| gender:", sum(!is.na(data_T2$gender)), "\n")
cat("T3:", nrow(data_T3),
    "| cortisol1:", sum(!is.na(data_T3$cortisol1)),
    "| cortisone1:", sum(!is.na(data_T3$cortisone1)),
    "| gender:", sum(!is.na(data_T3$gender)), "\n")

#Auswertung & Outlier-Report 
res_T1 <- process_hormones(data_T1, "T1", "cortisol1", "cortisone1")
res_T2 <- process_hormones(data_T2, "T2", "cortisol2", "cortisone2")
res_T3 <- process_hormones(data_T3, "T3", "cortisol1", "cortisone1")

hormone_summary <- bind_rows(res_T1$summary, res_T2$summary, res_T3$summary)
outlier_counts  <- bind_rows(res_T1$removed, res_T2$removed, res_T3$removed) %>%
  mutate(pct_removed = ifelse(n_total>0, round(100*n_removed/n_total,1), NA_real_))

cat("\n===== Zusammenfassung (nach 95%-Cutoff) =====\n")
print(hormone_summary)

cat("\n===== Outlier-Report =====\n")
print(outlier_counts)

###Boxplot Corisol----
#mit Cortisol zu T1, T2, T3 
# Cortisol T1–T3 in Long-Form bringen (mit Geschlecht)
cort_long <- dplyr::bind_rows(
  data %>%
    dplyr::filter(.data[[ev]] == EV_HCC_T1) %>%
    dplyr::select(study_id, cortisol = cortisol1) %>%
    dplyr::mutate(timepoint = "T1"),
  data %>%
    dplyr::filter(.data[[ev]] == EV_HCC_T2) %>%
    dplyr::select(study_id, cortisol = cortisol2) %>%
    dplyr::mutate(timepoint = "T2"),
  data %>%
    dplyr::filter(.data[[ev]] == EV_HCC_T3) %>%
    dplyr::select(study_id, cortisol = cortisol1) %>%
    dplyr::mutate(timepoint = "T3")
) %>%
  dplyr::left_join(gender_lookup, by = "study_id") %>%
  dplyr::mutate(
    cortisol = clean_num(cortisol),
    sex = recode_sex(gender),
    timepoint = factor(timepoint, levels = c("T1","T2","T3"))
  )

# Ausreißer nach 95%-Cutoff (pro Sex) entfernen
cut_cort_f <- cutoffs$p95[cutoffs$hormon == "cortisol" & cutoffs$sex == "female"]
cut_cort_m <- cutoffs$p95[cutoffs$hormon == "cortisol" & cutoffs$sex == "male"]

cort_long_clean <- cort_long %>%
  dplyr::filter(!is.na(cortisol), !is.na(sex)) %>%
  dplyr::filter(
    (sex == "female" & cortisol <= cut_cort_f) |
      (sex == "male"   & cortisol <= cut_cort_m)
  )



ggplot(cort_long_clean, aes(x = sex, y = cortisol, fill = sex)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.85) +
  geom_jitter(width = 0.1, alpha = 0.35, size = 1.4) +
  facet_wrap(~ timepoint, nrow = 1) +
  scale_fill_manual(values = c(female = "#e74c3c", male = "#3498db"), guide = "none") +
  scale_x_discrete(labels = c(female = "Female", male = "Male")) +
  labs(title = "Cortisol levels across Time Points",
       x = NULL, y = "Cortisol (pg/mg)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Überprüfen, ob dieselben IDs mehrfach vorkommen
table_counts <- cort_long_clean %>% count(study_id) %>% pull(n)
if (all(table_counts > 1)) {
  message("→ Mehrfache Messungen pro Person erkannt: wiederholte Mess-ANOVA")
} else {
  message("→ Ungleiche Stichproben über Zeitpunkte: einfache ANOVA pro Geschlecht")
}

### ANOVA Cortisol----
anova_results_cortisol <- cort_long_clean %>%
  group_by(sex) %>%
  anova_test(dv = cortisol, between = timepoint)


anova_results_cortisol

## Post-hoc Cortisol
posthoc_results_cortisol <- cort_long_clean %>%
  group_by(sex) %>%
  tukey_hsd(cortisol ~ timepoint)

posthoc_results_cortisol



### Boxplot Cortisone ----

cortisone_long <- dplyr::bind_rows(
  data %>%
    dplyr::filter(.data[[ev]] == EV_HCC_T1) %>%
    dplyr::select(study_id, cortisone = cortisone1) %>%
    dplyr::mutate(timepoint = "T1"),
  data %>%
    dplyr::filter(.data[[ev]] == EV_HCC_T2) %>%
    dplyr::select(study_id, cortisone = cortisone2) %>%
    dplyr::mutate(timepoint = "T2"),
  data %>%
    dplyr::filter(.data[[ev]] == EV_HCC_T3) %>%
    dplyr::select(study_id, cortisone = cortisone1) %>%
    dplyr::mutate(timepoint = "T3")
) %>%
  dplyr::left_join(gender_lookup, by = "study_id") %>%
  dplyr::mutate(
    cortisone = clean_num(cortisone),
    sex       = recode_sex(gender),
    timepoint = factor(timepoint, levels = c("T1","T2","T3"))
  )

# Ausreißer nach 95%-Cutoff (pro Sex) entfernen
cut_corti_f <- cutoffs$p95[cutoffs$hormon == "cortisone" & cutoffs$sex == "female"]
cut_corti_m <- cutoffs$p95[cutoffs$hormon == "cortisone" & cutoffs$sex == "male"]

cortisone_long_clean <- cortisone_long %>%
  dplyr::filter(!is.na(cortisone), !is.na(sex)) %>%
  dplyr::filter(
    (sex == "female" & cortisone <= cut_corti_f) |
      (sex == "male"   & cortisone <= cut_corti_m)
  )

ggplot(cortisone_long_clean, aes(x = sex, y = cortisone, fill = sex)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.85) +
  geom_jitter(width = 0.1, alpha = 0.35, size = 1.4) +
  facet_wrap(~ timepoint, nrow = 1) +
  scale_fill_manual(values = c(female = "#e74c3c", male = "#3498db"), guide = "none") +
  scale_x_discrete(labels = c(female = "Female", male = "Male")) +
  labs(title = "Cortisone levels across Time Points",
       x = NULL, y = "Cortisone (pg/mg)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Überprüfen, ob dieselben IDs mehrfach vorkommen (für Cortisone)
table_counts_cortisone <- cortisone_long_clean %>% count(study_id) %>% pull(n)
if (all(table_counts_cortisone > 1)) {
  message("→ Mehrfache Messungen pro Person erkannt (Cortisone): wiederholte Mess-ANOVA in Betracht ziehen")
} else {
  message("→ Ungleiche Stichproben über Zeitpunkte (Cortisone): einfache ANOVA pro Geschlecht")
}

### ANOVA Cortisone ----
anova_results_cortisone <- cortisone_long_clean %>%
  group_by(sex) %>%
  anova_test(dv = cortisone, between = timepoint)

anova_results_cortisone

## Post-hoc Cortisone
posthoc_results_cortisone <- cortisone_long_clean %>%
  group_by(sex) %>%
  tukey_hsd(cortisone ~ timepoint)

posthoc_results_cortisone

#outlier count----

EV_T1_Q   <- "fragebogen_t1_arm_1"
EV_T2_Q   <- "fragebogen_t2_arm_1"
EV_T3_Q   <- "fragebogen_t3_arm_1"
EV_T4_Q   <- "finaler_fragebogen_arm_1"

EV_HCC_T1 <- "untersuchung_t1_arm_1"
EV_HCC_T2 <- "untersuchung_t3_arm_1"
EV_HCC_T3 <- "untersuchung_t3_arm_1"   



## Hair Cortisol----

library(dplyr)
library(rlang)

# universelle Helper-Funktion für fehlende HCC-Werte pro Event
count_missing_hair <- function(df, cortisol_var, cortisone_var) {
  
  # 1) passende ID-Spalte finden
  id_var <- "study_id"
  if (!("study_id" %in% names(df))) {
    id_candidates <- intersect(c("record_id", "id", "participant_id"), names(df))
    if (length(id_candidates) == 0) {
      stop("Keine geeignete ID-Spalte gefunden (weder study_id noch record_id/id/participant_id).")
    }
    id_var <- id_candidates[1]
  }
  
  # 2) Symbole für tidy evaluation
  id_sym      <- sym(id_var)
  cort_sym    <- sym(cortisol_var)
  cortis_sym  <- sym(cortisone_var)
  
  # 3) pro Person prüfen
  df %>%
    group_by(!!id_sym) %>%
    summarise(
      all_missing = all(is.na(!!cort_sym) & is.na(!!cortis_sym)),
      .groups = "drop"
    ) %>%
    filter(all_missing) %>%
    nrow()            
}



# T1: cortisol1 / cortisone1
hcc_T1 <- data %>% filter(redcap_event_name == EV_HCC_T1)
n_missing_HCC_T1 <- count_missing_hair(hcc_T1, "cortisol1", "cortisone1")
n_missing_HCC_T1

# T2: cortisol2 / cortisone2
hcc_T2 <- data %>% filter(redcap_event_name == EV_HCC_T2)
n_missing_HCC_T2 <- count_missing_hair(hcc_T2, "cortisol2", "cortisone2")
n_missing_HCC_T2

# T3: cortisol1 / cortisone1 
hcc_T3 <- data %>% filter(redcap_event_name == EV_HCC_T3)
n_missing_HCC_T3 <- count_missing_hair(hcc_T3, "cortisol1", "cortisone1")
n_missing_HCC_T3


## Ausreißer Cortisol / Cortisone ----

if (exists("outlier_counts")) {
  hcc_outliers_summary <- outlier_counts %>%
    dplyr::group_by(timepoint) %>%
    dplyr::summarise(
      total_removed = sum(n_removed, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\n--- Outlier report (HCC) ---\n")
  print(hcc_outliers_summary)
} else {
  cat("\n(Hinweis: 'outlier_counts' existiert nicht im Workspace – falls du den HCC-Outlier-Code vorher nicht laufen lässt, kann hier nichts zusammengefasst werden.)\n")
}


## Pain Intensity ----

# Helper: counts missing pain_avg per timepoint
count_missing_pain <- function(df, tp_label) {
  df %>%
    group_by(study_id) %>%
    summarise(
      has_pain = any(!is.na(pain_avg)),
      .groups = "drop"
    ) %>%
    summarise(
      timepoint        = tp_label,
      n_total          = n(),
      n_missing_pain   = sum(!has_pain),
      n_available_pain = sum(has_pain),
      .groups = "drop"
    )
}

# Apply for each timepoint

# T1
t1_pain <- data %>% filter(redcap_event_name == EV_T1_Q)
miss_T1_pain <- count_missing_pain(t1_pain, "T1")

# T2
t2_pain <- data %>% filter(redcap_event_name == EV_T2_Q)
miss_T2_pain <- count_missing_pain(t2_pain, "T2")

# T3
t3_pain <- data %>% filter(redcap_event_name == EV_T3_Q)
miss_T3_pain <- count_missing_pain(t3_pain, "T3")

# T4
t4_pain <- data %>% filter(redcap_event_name == EV_T4_Q)
miss_T4_pain <- count_missing_pain(t4_pain, "T4")

# Combine results
missing_pain_table <- bind_rows(
  miss_T1_pain,
  miss_T2_pain,
  miss_T3_pain,
  miss_T4_pain
)

print(missing_pain_table)

##  Disability ----

# Helper: counts missing NDI per timepoint
count_missing_ndi <- function(df, tp_label) {
  df %>%
    group_by(study_id) %>%
    summarise(
      has_ndi = any(!is.na(ndiscore)),
      .groups = "drop"
    ) %>%
    summarise(
      timepoint        = tp_label,
      n_total          = n(),
      n_missing_ndi    = sum(!has_ndi),
      n_available_ndi  = sum(has_ndi),
      .groups = "drop"
    )
}

# Apply for each timepoint

# T1
t1_ndi <- data %>% filter(redcap_event_name == EV_T1_Q)
miss_T1_ndi <- count_missing_ndi(t1_ndi, "T1")

# T2
t2_ndi <- data %>% filter(redcap_event_name == EV_T2_Q)
miss_T2_ndi <- count_missing_ndi(t2_ndi, "T2")

# T3
t3_ndi <- data %>% filter(redcap_event_name == EV_T3_Q)
miss_T3_ndi <- count_missing_ndi(t3_ndi, "T3")

# T4
t4_ndi <- data %>% filter(redcap_event_name == EV_T4_Q)
miss_T4_ndi <- count_missing_ndi(t4_ndi, "T4")

# Combine results
missing_ndi_table <- bind_rows(
  miss_T1_ndi,
  miss_T2_ndi,
  miss_T3_ndi,
  miss_T4_ndi
)

print(missing_ndi_table)

##SCI ----


# Helper: counts missing SCI per timepoint
count_missing_SCI <- function(df, tp_label) {
  df %>%
    group_by(study_id) %>%
    summarise(
      has_SCI = any(
        !is.na(stress_skala1_sum) &
          !is.na(stress_skala2_sum) &
          !is.na(gesamtscore_symptom)
      ),
      .groups = "drop"
    ) %>%
    summarise(
      timepoint        = tp_label,
      n_total          = n(),
      n_missing_SCI    = sum(!has_SCI),
      n_available_SCI  = sum(has_SCI),
      .groups          = "drop"
    )
}

# Apply for each timepoint

# T1
t1_SCI <- data %>% filter(redcap_event_name == EV_T1_Q)
miss_T1_SCI <- count_missing_SCI(t1_SCI, "T1")

# T2
t2_SCI <- data %>% filter(redcap_event_name == EV_T2_Q)
miss_T2_SCI <- count_missing_SCI(t2_SCI, "T2")

# T3
t3_SCI <- data %>% filter(redcap_event_name == EV_T3_Q)
miss_T3_SCI <- count_missing_SCI(t3_SCI, "T3")

# T4
t4_SCI <- data %>% filter(redcap_event_name == EV_T4_Q)
miss_T4_SCI <- count_missing_SCI(t4_SCI, "T4")

# Combine results
missing_SCI_table <- bind_rows(
  miss_T1_SCI,
  miss_T2_SCI,
  miss_T3_SCI,
  miss_T4_SCI
)

print(missing_SCI_table)

##Activity Patterns ----

# Helper: counts missing Activity Patterns per timepoint
count_missing_activity <- function(df, tp_label) {
  df %>%
    group_by(study_id) %>%
    summarise(
      has_activity = any(
        !is.na(score_pps) & !is.na(dms_score)
      ),
      .groups = "drop"
    ) %>%
    summarise(
      timepoint            = tp_label,
      n_total              = n(),
      n_missing_activity   = sum(!has_activity),
      n_available_activity = sum(has_activity),
      .groups              = "drop"
    )
}

# T1
t1_act <- data %>% filter(redcap_event_name == EV_T1_Q)
miss_T1_activity <- count_missing_activity(t1_act, "T1")

# T2
t2_act <- data %>% filter(redcap_event_name == EV_T2_Q)
miss_T2_activity <- count_missing_activity(t2_act, "T2")

# T3
t3_act <- data %>% filter(redcap_event_name == EV_T3_Q)
miss_T3_activity <- count_missing_activity(t3_act, "T3")

# T4
t4_act <- data %>% filter(redcap_event_name == EV_T4_Q)
miss_T4_activity <- count_missing_activity(t4_act, "T4")

# Alles zusammenführen
missing_activity_table <- bind_rows(
  miss_T1_activity,
  miss_T2_activity,
  miss_T3_activity,
  miss_T4_activity
)

print(missing_activity_table)


# Missing data exploration----
library(dplyr)
library(tidyr)
library(ggplot2)

# 0) Timepoint mapping
tp_q <- c(
  T1 = "fragebogen_t1_arm_1",
  T2 = "fragebogen_t2_arm_1",
  T3 = "fragebogen_t3_arm_1",
  T4 = "finaler_fragebogen_arm_1"
)

# 1) HCC mapping 

EV_HCC_T1 <- "untersuchung_t1_arm_1"
EV_HCC_T2 <- "untersuchung_t3_arm_1"
EV_HCC_T3 <- "untersuchung_t3_arm_1"


# 2) Ensure pain_avg exists

data <- data %>%
  mutate(
    pain_avg = rowMeans(across(c(paindetect1, paindetect2, paindetect3)), na.rm = TRUE)
  )


# 3) Helper: missing COUNTS per variable

miss_counts <- function(df, vars) {
  tibble(variable = vars) %>%
    rowwise() %>%
    mutate(
      n_total   = nrow(df),
      n_missing = sum(is.na(df[[variable]]))
    ) %>%
    ungroup()
}


# 4) Questionnaire missingness (T1–T4)
#    Activity patterns condensed to ONE availability indicator:
#    available if BOTH score_pps and dms_score are present.

vars_q <- c(
  "pain_avg",
  "ndiscore",
  "stress_skala1_sum",
  "stress_skala2_sum",
  "gesamtscore_symptom",
  "activity_available"
)

miss_q <- lapply(names(tp_q), function(tp) {
  ev <- tp_q[[tp]]
  
  df_tp <- data %>%
    filter(redcap_event_name == ev) %>%
    transmute(
      pain_avg,
      ndiscore,
      stress_skala1_sum,
      stress_skala2_sum,
      gesamtscore_symptom,
      activity_available = ifelse(!is.na(score_pps) & !is.na(dms_score), 1, NA_real_)
    )
  
  if (nrow(df_tp) == 0) {
    return(tibble(timepoint = tp, variable = vars_q, n_total = 0, n_missing = NA_integer_))
  }
  
  miss_counts(df_tp, vars_q) %>%
    mutate(timepoint = tp, .before = 1)
}) %>% bind_rows()


# 5) Hair/HCC missingness (HCC_T1–HCC_T3)

miss_hcc_one <- function(tp_label, ev_name, cortisol_col, cortisone_col) {
  
  df_tp <- data %>%
    filter(redcap_event_name == ev_name) %>%
    transmute(
      cortisol  = suppressWarnings(as.numeric(.data[[cortisol_col]])),
      cortisone = suppressWarnings(as.numeric(.data[[cortisone_col]]))
    )
  
  if (nrow(df_tp) == 0) {
    return(tibble(timepoint = tp_label, variable = c("cortisol","cortisone"),
                  n_total = 0, n_missing = NA_integer_))
  }
  
  miss_counts(df_tp, c("cortisol","cortisone")) %>%
    mutate(timepoint = tp_label, .before = 1)
}

miss_hcc <- bind_rows(
  miss_hcc_one("HCC_T1", EV_HCC_T1, "cortisol1", "cortisone1"),
  miss_hcc_one("HCC_T2", EV_HCC_T2, "cortisol2", "cortisone2"),
  miss_hcc_one("HCC_T3", EV_HCC_T3, "cortisol1", "cortisone1")
)


# 6) Combine + variable labels

miss_all <- bind_rows(miss_q, miss_hcc) %>%
  mutate(
    variable = dplyr::recode(variable,
                             pain_avg            = "Pain intensity",
                             ndiscore            = "Disability (NDI)",
                             stress_skala1_sum   = "SCI: stress due to uncertainty",
                             stress_skala2_sum   = "SCI: stress due to excessive demands",
                             gesamtscore_symptom = "SCI: stress symptoms",
                             activity_available  = "Activity patterns",
                             cortisol            = "Hair cortisol",
                             cortisone           = "Hair cortisone"
    ),
    timepoint = factor(timepoint, levels = c("T1","T2","T3","T4","HCC_T1","HCC_T2","HCC_T3"))
  )

# View table (optional)
print(miss_all)


# 7) ONE figure: Heatmap with labels 

p_miss_counts <- ggplot(miss_all, aes(x = timepoint, y = variable, fill = n_missing)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(
    aes(label = ifelse(is.na(n_missing) | n_total == 0, "NA", n_missing)),
    color = "white", size = 4, fontface = "bold"
  ) +
  scale_fill_gradient(na.value = "grey90") +
  labs(
    title = "Missing data overview by time point (counts)",
    x = "Time point",
    y = NULL,
    fill = "Missing (n)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

p_miss_counts


#Spaghetti plots----


# 0) Timepoint mapping (Questionnaires)

tp_map_q <- tibble(
  redcap_event_name = c("fragebogen_t1_arm_1",
                        "fragebogen_t2_arm_1",
                        "fragebogen_t3_arm_1",
                        "finaler_fragebogen_arm_1"),
  timepoint = factor(c("T1","T2","T3","T4"),
                     levels = c("T1","T2","T3","T4"))
)

# 1) HCC mapping

EV_HCC_T1 <- "untersuchung_t1_arm_1"
EV_HCC_T2 <- "untersuchung_t3_arm_1"
EV_HCC_T3 <- "untersuchung_t3_arm_1"


# 2) Ensure pain_avg exists

data <- data %>%
  mutate(
    pain_avg = rowMeans(across(c(paindetect1, paindetect2, paindetect3)), na.rm = TRUE)
  )


# 3) Activity pattern (AR/EER/DER/FAR) from score_pps + dms_score

mk_activity_pattern <- function(df) {
  df %>%
    mutate(
      activity_pattern = case_when(
        score_pps < 3  & dms_score == 2 ~ "FAR",
        score_pps >= 3 & dms_score == 2 ~ "DER",
        score_pps >= 3 & dms_score <  2 ~ "EER",
        score_pps < 3  & dms_score <  2 ~ "AR",
        TRUE ~ NA_character_
      ),
      activity_pattern = factor(activity_pattern, levels = c("AR","EER","DER","FAR"))
    )
}


# 4) Build LONG dataset for questionnaire spaghetti (T1–T4)

spag_q <- data %>%
  inner_join(tp_map_q, by = "redcap_event_name") %>%
  mk_activity_pattern() %>%
  transmute(
    study_id,
    timepoint,
    pain_avg          = suppressWarnings(as.numeric(pain_avg)),
    ndiscore          = suppressWarnings(as.numeric(ndiscore)),
    sci_uncertainty   = suppressWarnings(as.numeric(stress_skala1_sum)),
    sci_demands       = suppressWarnings(as.numeric(stress_skala2_sum)),
    sci_symptoms      = suppressWarnings(as.numeric(gesamtscore_symptom)),
    activity_pattern  = activity_pattern
  )

# Long for continuous variables
spag_q_cont <- spag_q %>%
  pivot_longer(
    cols = c(pain_avg, ndiscore, sci_uncertainty, sci_demands, sci_symptoms),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = dplyr::recode(variable,
                             pain_avg        = "Pain intensity",
                             ndiscore        = "Disability (NDI)",
                             sci_uncertainty = "SCI: stress due to uncertainty",
                             sci_demands     = "SCI: stress due to excessive demands",
                             sci_symptoms    = "SCI: stress symptoms"
    )
  )

# Long for categorical activity patterns
spag_q_act <- spag_q %>%
  filter(!is.na(activity_pattern))


# 5) Build LONG dataset for HCC spaghetti (HCC_T1–HCC_T3)
#    Mapping: T1 = cortisol1/cortisone1 (untersuchung_t1)
#             T2 = cortisol2/cortisone2 (untersuchung_t3)
#             T3 = cortisol1/cortisone1 (untersuchung_t3)

hcc_long <- bind_rows(
  data %>%
    filter(redcap_event_name == EV_HCC_T1) %>%
    transmute(study_id,
              timepoint = factor("HCC_T1", levels = c("HCC_T1","HCC_T2","HCC_T3")),
              cortisol  = suppressWarnings(as.numeric(cortisol1)),
              cortisone = suppressWarnings(as.numeric(cortisone1))),
  data %>%
    filter(redcap_event_name == EV_HCC_T2) %>%
    transmute(study_id,
              timepoint = factor("HCC_T2", levels = c("HCC_T1","HCC_T2","HCC_T3")),
              cortisol  = suppressWarnings(as.numeric(cortisol2)),
              cortisone = suppressWarnings(as.numeric(cortisone2))),
  data %>%
    filter(redcap_event_name == EV_HCC_T3) %>%
    transmute(study_id,
              timepoint = factor("HCC_T3", levels = c("HCC_T1","HCC_T2","HCC_T3")),
              cortisol  = suppressWarnings(as.numeric(cortisol1)),
              cortisone = suppressWarnings(as.numeric(cortisone1)))
)

hcc_cont <- hcc_long %>%
  pivot_longer(cols = c(cortisol, cortisone), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = dplyr::recode(variable,
                             cortisol  = "Hair cortisol",
                             cortisone = "Hair cortisone"
    )
  )


# 6) Plot functions

make_spaghetti_cont <- function(df, var_label) {
  df_sub <- df %>% filter(variable == var_label)
  
  ggplot(df_sub, aes(x = timepoint, y = value, group = study_id, color = factor(study_id))) +
    geom_line(alpha = 0.45, linewidth = 0.5, show.legend = FALSE) +
    geom_point(alpha = 0.45, size = 1, show.legend = FALSE) +
    labs(title = paste("Spaghetti plot:", var_label),
         x = "Time point", y = var_label) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

make_spaghetti_activity <- function(df_act) {
  ggplot(df_act, aes(x = timepoint, y = activity_pattern, group = study_id, color = factor(study_id))) +
    geom_line(alpha = 0.45, linewidth = 0.5, show.legend = FALSE) +
    geom_point(alpha = 0.60, size = 1.2, show.legend = FALSE) +
    labs(title = "Spaghetti plot: Activity patterns",
         x = "Time point", y = "Activity pattern") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}


# 7) Create plots (Questionnaire)

p_pain <- make_spaghetti_cont(spag_q_cont, "Pain intensity")
p_ndi  <- make_spaghetti_cont(spag_q_cont, "Disability (NDI)")

p_sci1 <- make_spaghetti_cont(spag_q_cont, "SCI: stress due to uncertainty")
p_sci2 <- make_spaghetti_cont(spag_q_cont, "SCI: stress due to excessive demands")
p_sci3 <- make_spaghetti_cont(spag_q_cont, "SCI: stress symptoms")

p_act  <- make_spaghetti_activity(spag_q_act)



# Print 
print(p_pain)
print(p_ndi)
print(p_sci1) 
print(p_sci2) 
print(p_sci3)
print(p_act)


#HCC spaghetti plot
ev <- "redcap_event_name"
EV_T1_Q <- "fragebogen_t1_arm_1"

gender_lookup <- data %>%
  filter(.data[[ev]] == EV_T1_Q) %>%
  distinct(study_id, gender)

# Helper: recode sex robustly
recode_sex <- function(g){
  dplyr::case_when(
    tolower(as.character(g)) %in% c("female","weiblich","f") ~ "female",
    tolower(as.character(g)) %in% c("male","männlich","m")   ~ "male",
    suppressWarnings(as.numeric(g)) == 2 ~ "female",
    suppressWarnings(as.numeric(g)) == 1 ~ "male",
    TRUE ~ NA_character_
  )
}


# 2) 95%-cutoffs 

cutoffs <- tribble(
  ~hormon,     ~sex,     ~p95,
  "cortisol",  "female", 22.14,
  "cortisol",  "male",   24.69,
  "cortisone", "female", 34.24,
  "cortisone", "male",   35.75
)


# 3) HCC event mapping 

EV_HCC_T1 <- "untersuchung_t1_arm_1"
EV_HCC_T2 <- "untersuchung_t3_arm_1"
EV_HCC_T3 <- "untersuchung_t3_arm_1"

# Build long HCC dataset 
hcc_long <- bind_rows(
  data %>%
    filter(.data[[ev]] == EV_HCC_T1) %>%
    transmute(study_id,
              timepoint = "HCC_T1",
              cortisol  = suppressWarnings(as.numeric(cortisol1)),
              cortisone = suppressWarnings(as.numeric(cortisone1))),
  data %>%
    filter(.data[[ev]] == EV_HCC_T2) %>%
    transmute(study_id,
              timepoint = "HCC_T2",
              cortisol  = suppressWarnings(as.numeric(cortisol2)),
              cortisone = suppressWarnings(as.numeric(cortisone2))),
  data %>%
    filter(.data[[ev]] == EV_HCC_T3) %>%
    transmute(study_id,
              timepoint = "HCC_T3",
              cortisol  = suppressWarnings(as.numeric(cortisol1)),
              cortisone = suppressWarnings(as.numeric(cortisone1)))
) %>%
  mutate(timepoint = factor(timepoint, levels = c("HCC_T1","HCC_T2","HCC_T3"))) %>%
  left_join(gender_lookup, by = "study_id") %>%
  mutate(sex = recode_sex(gender))
hcc_long <- hcc_long %>%
  filter(!is.na(sex))



# 4) Apply sex-specific 95% cutoff: values above cutoff -> NA

get_cut <- function(h, s){
  cutoffs$p95[cutoffs$hormon == h & cutoffs$sex == s]
}

hcc_long_clean <- hcc_long %>%
  rowwise() %>%
  mutate(
    cortisol_cut  = ifelse(!is.na(sex), get_cut("cortisol", sex), NA_real_),
    cortisone_cut = ifelse(!is.na(sex), get_cut("cortisone", sex), NA_real_),
    cortisol  = ifelse(!is.na(cortisol_cut)  & !is.na(cortisol)  & cortisol  > cortisol_cut,  NA_real_, cortisol),
    cortisone = ifelse(!is.na(cortisone_cut) & !is.na(cortisone) & cortisone > cortisone_cut, NA_real_, cortisone)
  ) %>%
  ungroup() %>%
  select(-gender, -cortisol_cut, -cortisone_cut)


# 5) Long format for plotting

hcc_cont <- hcc_long_clean %>%
  pivot_longer(cols = c(cortisol, cortisone),
               names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = dplyr::recode(variable,
                             cortisol  = "Hair cortisol",
                             cortisone = "Hair cortisone")
  )


# 6) Spaghetti plot function 

make_spaghetti_cont <- function(df, var_label) {
  df_sub <- df %>% filter(variable == var_label)
  
  ggplot(df_sub, aes(x = timepoint, y = value, group = study_id, color = factor(study_id))) +
    geom_line(alpha = 0.45, linewidth = 0.5, show.legend = FALSE) +
    geom_point(alpha = 0.55, size = 1, show.legend = FALSE) +
    labs(title = paste("Spaghetti plot:", var_label),
         x = "Time point", y = var_label) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

p_cort  <- make_spaghetti_cont(hcc_cont, "Hair cortisol")
p_corti <- make_spaghetti_cont(hcc_cont, "Hair cortisone")

print(p_cort)
print(p_corti)

#Activity Patterns: Alluvial/Sankey Diagram----

install.packages("ggalluvial")
library(ggalluvial)
library(dplyr)
library(tidyr)
library(ggplot2)

# spag_q_act hast du schon: study_id, timepoint, activity_pattern

act_wide <- spag_q_act %>%
  select(study_id, timepoint, activity_pattern) %>%
  pivot_wider(names_from = timepoint, values_from = activity_pattern)

# nur vollständige Verläufe (optional, aber besser lesbar)
act_wide_cc <- act_wide %>% filter(!is.na(T1) & !is.na(T2) & !is.na(T3) & !is.na(T4))

# Häufigkeiten der Pfade
act_paths <- act_wide_cc %>%
  count(T1, T2, T3, T4, name = "n")

ggplot(act_paths,
       aes(axis1 = T1, axis2 = T2, axis3 = T3, axis4 = T4, y = n)) +
  geom_alluvium(aes(fill = T1), alpha = 0.8) +
  geom_stratum(width = 0.18, color = NA) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("T1","T2","T3","T4")) +
  labs(title = "Activity patterns: transitions over time",
       x = "Time point", y = "Number of participants") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())



#Prediction-Plots----
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(emmeans)

# --- Deine Event-Namen ---
EV_T1_Q <- "fragebogen_t1_arm_1"
EV_T2_Q <- "fragebogen_t2_arm_1"
EV_T3_Q <- "fragebogen_t3_arm_1"
EV_T4_Q <- "finaler_fragebogen_arm_1"

# 1) pain_avg sicherstellen (wie bei dir)
data <- data %>%
  mutate(pain_avg = rowMeans(across(c(paindetect1, paindetect2, paindetect3)), na.rm = TRUE))

# 2) Timepoint mapping (Questionnaires)
tp_map_q <- tibble(
  redcap_event_name = c(EV_T1_Q, EV_T2_Q, EV_T3_Q, EV_T4_Q),
  timepoint = factor(c("T1","T2","T3","T4"), levels = c("T1","T2","T3","T4"))
)

# 3) Kovariaten aus T1 (wie bei dir)
covars_T1 <- data %>%
  filter(redcap_event_name == EV_T1_Q) %>%
  transmute(
    study_id,
    age = clean_num(age),
    gender = factor(gender),
    dass21_depression = clean_num(dass21_depression)
  )

# 4) Long-Datensatz für wiederholte Outcomes
dat_long_q <- data %>%
  inner_join(tp_map_q, by = "redcap_event_name") %>%
  transmute(
    study_id,
    timepoint,
    pain_avg = clean_num(pain_avg),
    ndiscore = clean_num(ndiscore)
  ) %>%
  left_join(covars_T1, by = "study_id")

# --- Plot-Funktion: Rohdaten + emmeans-Vorhersage ---
plot_raw_vs_model <- function(df_long, outcome, model_title) {
  
  # LMM (Random Intercept). Wenn du Random Slope willst: siehe Kommentar unten.
  form <- as.formula(paste0(outcome, " ~ timepoint + age + gender + dass21_depression + (1|study_id)"))
  mod  <- lmer(form, data = df_long, REML = TRUE)
  
  emm_df <- as.data.frame(emmeans(mod, ~ timepoint))
  
  ggplot() +
    # Rohdaten
    geom_jitter(
      data = df_long,
      aes(x = timepoint, y = .data[[outcome]]),
      color = "grey70", width = 0.08, alpha = 0.45, size = 1
    ) +
    # Modell-CI
    geom_ribbon(
      data = emm_df,
      aes(x = timepoint, ymin = lower.CL, ymax = upper.CL, group = 1),
      alpha = 0.20
    ) +
    # Modell-Linie
    geom_line(
      data = emm_df,
      aes(x = timepoint, y = emmean, group = 1),
      linewidth = 1
    ) +
    geom_point(
      data = emm_df,
      aes(x = timepoint, y = emmean),
      size = 2
    ) +
    labs(title = model_title, x = "Time point", y = outcome) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

p_pain_pred <- plot_raw_vs_model(dat_long_q, "pain_avg",
                                 "Pain intensity over time: raw data and model-based predictions")

p_ndi_pred  <- plot_raw_vs_model(dat_long_q, "ndiscore",
                                 "Disability (NDI) over time: raw data and model-based predictions")

print(p_pain_pred)
print(p_ndi_pred)

#all in one Diagramm----

# (1) ONE faceted figure: Raw data + model-based predictions


clean_num <- function(x) {
  x <- as.character(x)
  x[x %in% c("NA", ".", "")] <- NA
  suppressWarnings(as.numeric(x))
}

recode_sex <- function(g){
  dplyr::case_when(
    tolower(as.character(g)) %in% c("female","weiblich","f") ~ "female",
    tolower(as.character(g)) %in% c("male","männlich","m")   ~ "male",
    suppressWarnings(as.numeric(g)) == 2 ~ "female",
    suppressWarnings(as.numeric(g)) == 1 ~ "male",
    TRUE ~ NA_character_
  )
}

# Event names
ev <- "redcap_event_name"
EV_T1_Q <- "fragebogen_t1_arm_1"
EV_T2_Q <- "fragebogen_t2_arm_1"
EV_T3_Q <- "fragebogen_t3_arm_1"
EV_T4_Q <- "finaler_fragebogen_arm_1"

EV_HCC_T1 <- "untersuchung_t1_arm_1"
EV_HCC_T2 <- "untersuchung_t3_arm_1"
EV_HCC_T3 <- "untersuchung_t3_arm_1"

tp_map_q <- tibble(
  redcap_event_name = c(EV_T1_Q, EV_T2_Q, EV_T3_Q, EV_T4_Q),
  timepoint = factor(c("T1","T2","T3","T4"), levels = c("T1","T2","T3","T4"))
)

# Derived: pain_avg
data <- data %>%
  mutate(pain_avg = rowMeans(across(c(paindetect1, paindetect2, paindetect3)), na.rm = TRUE))

# Questionnaire long (T1–T4)
q_long <- data %>%
  inner_join(tp_map_q, by = "redcap_event_name") %>%
  transmute(
    study_id,
    timepoint,
    pain_avg            = clean_num(pain_avg),
    ndiscore            = clean_num(ndiscore),
    stress_skala1_sum   = clean_num(stress_skala1_sum),
    stress_skala2_sum   = clean_num(stress_skala2_sum),
    gesamtscore_symptom = clean_num(gesamtscore_symptom)
  )

# Gender lookup for HCC cutoffs
gender_lookup <- data %>%
  filter(.data[[ev]] == EV_T1_Q) %>%
  distinct(study_id, gender) %>%
  mutate(sex = recode_sex(gender)) %>%
  select(study_id, sex)

# 95% cutoffs (Kirschbaum)
cutoffs <- tribble(
  ~hormon,     ~sex,     ~p95,
  "cortisol",  "female", 22.14,
  "cortisol",  "male",   24.69,
  "cortisone", "female", 34.24,
  "cortisone", "male",   35.75
)

cut_cort_f  <- cutoffs$p95[cutoffs$hormon == "cortisol"  & cutoffs$sex == "female"]
cut_cort_m  <- cutoffs$p95[cutoffs$hormon == "cortisol"  & cutoffs$sex == "male"]
cut_corti_f <- cutoffs$p95[cutoffs$hormon == "cortisone" & cutoffs$sex == "female"]
cut_corti_m <- cutoffs$p95[cutoffs$hormon == "cortisone" & cutoffs$sex == "male"]

# HCC long mapped to T1–T4 axis 
hcc_long_raw <- bind_rows(
  data %>%
    filter(.data[[ev]] == EV_HCC_T1) %>%
    transmute(study_id, timepoint = "T1",
              cortisol = clean_num(cortisol1),
              cortisone = clean_num(cortisone1)),
  data %>%
    filter(.data[[ev]] == EV_HCC_T2) %>%
    filter(if_any(all_of(c("cortisol2","cortisone2")), ~ !is.na(.))) %>%
    transmute(study_id, timepoint = "T2",
              cortisol = clean_num(cortisol2),
              cortisone = clean_num(cortisone2)),
  data %>%
    filter(.data[[ev]] == EV_HCC_T3) %>%
    filter(if_any(all_of(c("cortisol1","cortisone1")), ~ !is.na(.))) %>%
    transmute(study_id, timepoint = "T3",
              cortisol = clean_num(cortisol1),
              cortisone = clean_num(cortisone1))
) %>%
  left_join(gender_lookup, by = "study_id") %>%
  mutate(timepoint = factor(timepoint, levels = c("T1","T2","T3","T4")))

# Apply cutoffs 
hcc_long <- hcc_long_raw %>%
  mutate(
    cortisol = ifelse(
      (sex == "female" & !is.na(cortisol)  & cortisol  > cut_cort_f) |
        (sex == "male"  & !is.na(cortisol)  & cortisol  > cut_cort_m),
      NA, cortisol
    ),
    cortisone = ifelse(
      (sex == "female" & !is.na(cortisone) & cortisone > cut_corti_f) |
        (sex == "male"  & !is.na(cortisone) & cortisone > cut_corti_m),
      NA, cortisone
    )
  )

# Fit LMM + emmeans for one continuous variable
fit_pred_cont <- function(df, y, label) {
  d <- df %>%
    select(study_id, timepoint, y = all_of(y)) %>%
    filter(!is.na(y))
  
  if (nrow(d) < 10 || n_distinct(d$timepoint) < 2) return(NULL)
  
  m <- lmer(y ~ timepoint + (1|study_id), data = d, REML = TRUE)
  
  pred <- as.data.frame(emmeans(m, ~ timepoint)) %>%
    transmute(
      variable = label,
      timepoint = factor(timepoint, levels = c("T1","T2","T3","T4")),
      emmean, lower.CL, upper.CL
    )
  
  raw <- d %>%
    transmute(
      variable = label,
      timepoint,
      value = y
    )
  
  list(pred = pred, raw = raw)
}

# Questionnaire outcomes
res_pain  <- fit_pred_cont(q_long, "pain_avg",            "Pain intensity")
res_ndi   <- fit_pred_cont(q_long, "ndiscore",            "Disability (NDI)")
res_sci1  <- fit_pred_cont(q_long, "stress_skala1_sum",   "SCI: stress due to uncertainty")
res_sci2  <- fit_pred_cont(q_long, "stress_skala2_sum",   "SCI: stress due to excessive demands")
res_sympt <- fit_pred_cont(q_long, "gesamtscore_symptom", "SCI: stress symptoms")

# Objective stress (HCC)
res_cort  <- fit_pred_cont(hcc_long, "cortisol",  "Hair cortisol")
res_corti <- fit_pred_cont(hcc_long, "cortisone", "Hair cortisone")

# Combine
all_res <- list(res_pain,res_ndi,res_sci1,res_sci2,res_sympt,res_cort,res_corti)
pred_all <- bind_rows(lapply(all_res, `[[`, "pred"))
raw_all  <- bind_rows(lapply(all_res, `[[`, "raw"))

facet_order <- c(
  "Pain intensity",
  "Disability (NDI)",
  "SCI: stress due to uncertainty",
  "SCI: stress due to excessive demands",
  "SCI: stress symptoms",
  "Hair cortisol",
  "Hair cortisone"
)

pred_all$variable <- factor(pred_all$variable, levels = facet_order)
raw_all$variable  <- factor(raw_all$variable,  levels = facet_order)

# Plot
p_all_no_activity <- ggplot() +
  geom_point(
    data = raw_all,
    aes(x = timepoint, y = value),
    color = "grey70",
    alpha = 0.35,
    size = 1,
    position = position_jitter(width = 0.10, height = 0)
  ) +
  geom_ribbon(
    data = pred_all,
    aes(x = timepoint, ymin = lower.CL, ymax = upper.CL, group = 1),
    fill = "grey70",
    alpha = 0.35
  ) +
  geom_line(
    data = pred_all,
    aes(x = timepoint, y = emmean, group = 1),
    color = "black",
    linewidth = 0.7
  ) +
  geom_point(
    data = pred_all,
    aes(x = timepoint, y = emmean),
    color = "black",
    size = 1.4
  ) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  labs(
    title = "Raw data and model-based predictions over time (T1–T4)",
    x = "Time point",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_all_no_activity)


# (2) Separate figure for Activity patterns ONLY 

# Recreate activity_pattern if not present (uses score_pps + dms_score)
data <- data %>%
  mutate(
    activity_pattern = case_when(
      score_pps < 3 & dms_score == 2 ~ "FAR",
      score_pps >= 3 & dms_score == 2 ~ "DER",
      score_pps >= 3 & dms_score < 2 ~ "EER",
      score_pps < 3 & dms_score < 2 ~ "AR",
      TRUE ~ NA_character_
    ),
    activity_pattern = factor(activity_pattern, levels = c("AR","EER","DER","FAR"))
  )

act_long <- data %>%
  inner_join(tp_map_q, by = "redcap_event_name") %>%
  select(study_id, timepoint, activity_pattern) %>%
  filter(!is.na(activity_pattern))

# Proportions per time point
act_prop <- act_long %>%
  count(timepoint, activity_pattern, name = "n") %>%
  group_by(timepoint) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_activity <- ggplot(act_prop, aes(x = timepoint, y = prop, fill = activity_pattern)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = function(x) paste0(round(100*x), "%")) +
  labs(
    title = "Activity patterns over time (proportions)",
    x = "Time point",
    y = "Proportion of participants",
    fill = "Activity pattern"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_activity)



#LMM/Bayes----


library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

#LONG: Pain 
dat_long_pain <- dat_imp %>%
  transmute(
    study_id,
    pain_T4,
    age, gender, dass21_depression,
    
    pain_T1, pain_T2, pain_T3,
    activity_T1, activity_T2, activity_T3,
    stress1_T1, stress1_T2, stress1_T3,
    stress2_T1, stress2_T2, stress2_T3,
    sympt_T1,  sympt_T2,  sympt_T3,
    cortisol_T1, cortisol_T2, cortisol_T3,
    cortisone_T1, cortisone_T2, cortisone_T3
  ) %>%
  pivot_longer(
    cols = -c(study_id, pain_T4, age, gender, dass21_depression),
    names_to = c(".value", "time"),
    names_pattern = "^(pain|activity|stress1|stress2|sympt|cortisol|cortisone)_T(\\d)$"
  ) %>%
  mutate(
    time = factor(time, levels = c("1","2","3"), labels = c("T1","T2","T3")),
    activity = factor(activity, levels = c("AR","EER","DER","FAR")),
    gender   = factor(gender)
  )

# Check
table(dat_long_pain$time, useNA = "ifany")
dplyr::n_distinct(dat_long_pain$study_id)
nrow(dat_long_pain)

##LMM Pain----
m_pain_lmm <- lmer(
  pain_T4 ~ age + gender + dass21_depression +
    time + pain + activity + stress1 + stress2 + sympt + cortisol + cortisone +
    (1 | study_id),
  data = dat_long_pain,
  REML = TRUE
)

summary(m_pain_lmm)

library(brms)

##brms Pain ----
pri <- c(
  prior(normal(0, 2), class = "b"),
  prior(normal(0, 5), class = "Intercept"),
  prior(exponential(1), class = "sigma"),
  prior(exponential(1), class = "sd")  
)

form_pain_brm <- bf(
  pain_T4 ~ age + gender + dass21_depression +
    time + pain + activity + stress1 + stress2 + sympt + cortisol + cortisone +
    (1 | study_id)
)

m_brms_pain_student <- brm(
  formula = form_pain_brm,
  data = dat_long_pain,
  family = student(),
  prior = pri,
  chains = 2, iter = 4000, warmup = 1000, cores = 2,
  control = list(adapt_delta = 0.98)
)

m_brms_pain_gaussian <- brm(
  formula = form_pain_brm,
  data = dat_long_pain,
  family = gaussian(),
  prior = pri,
  chains = 2, iter = 4000, warmup = 1000, cores = 2,
  control = list(adapt_delta = 0.98)
)

summary(m_brms_pain_student)
summary(m_brms_pain_gaussian)

#LONG: NDI
dat_long_ndi <- dat_imp_ndi %>%
  transmute(
    study_id,
    ndiscore_T4,
    age, gender, dass21_depression,
    
    pain_T1, pain_T2, pain_T3,
    activity_T1, activity_T2, activity_T3,
    stress1_T1, stress1_T2, stress1_T3,
    stress2_T1, stress2_T2, stress2_T3,
    sympt_T1,  sympt_T2,  sympt_T3,
    cortisol_T1, cortisol_T2, cortisol_T3,
    cortisone_T1, cortisone_T2, cortisone_T3
  ) %>%
  pivot_longer(
    cols = -c(study_id, ndiscore_T4, age, gender, dass21_depression),
    names_to = c(".value", "time"),
    names_pattern = "^(pain|activity|stress1|stress2|sympt|cortisol|cortisone)_T(\\d)$"
  ) %>%
  mutate(
    time = factor(time, levels = c("1","2","3"), labels = c("T1","T2","T3")),
    activity = factor(activity, levels = c("AR","EER","DER","FAR")),
    gender   = factor(gender)
  )

## LMM NDI----
m_ndi_lmm <- lmer(
  ndiscore_T4 ~ age + gender + dass21_depression +
    time + pain + activity + stress1 + stress2 + sympt + cortisol + cortisone +
    (1 | study_id),
  data = dat_long_ndi,
  REML = TRUE
)

summary(m_ndi_lmm)

## brms NDI----
form_ndi_brm <- bf(
  ndiscore_T4 ~ age + gender + dass21_depression +
    time + pain + activity + stress1 + stress2 + sympt + cortisol + cortisone +
    (1 | study_id)
)
