
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
  mutate(age = as.numeric(age))   # sicherstellen, dass age numerisch ist

#  deskriptive Statistiken
age_summary <- list(
  n = sum(!is.na(data_age$age)),
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

## 2) Helper-Funktion: zusammenfassen für ndiscore
summarize_disability <- function(df, tp_label) {
  v <- df$ndiscore
  # robuster Umgang mit komplett NA:
  qs <- if (all(is.na(v))) rep(NA_real_, 5) else
    as.numeric(quantile(v, probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
  rng <- if (all(is.na(v))) c(NA_real_, NA_real_) else range(v, na.rm = TRUE)
  
  tibble(
    timepoint = tp_label,
    n   = sum(!is.na(v)),
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v, na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = qs[1],
    q_25   = qs[2],
    q_50   = qs[3],
    q_75   = qs[4],
    q_97.5 = qs[5]
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE),  2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1],  2),
    q_25   = round(qs[2],  2),
    q_50   = round(qs[3],  2),
    q_75   = round(qs[4],  2),
    q_97.5 = round(qs[5],  2)
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
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
    mean = round(mean(v, na.rm = TRUE), 2),
    sd   = round(sd(v,   na.rm = TRUE), 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
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

# Deskriptive Statistiken je Zeitpunkt
summarize_num_tp <- function(vec, tp_lab) {
  if (length(vec) == 0 || all(is.na(vec))) {
    return(tibble::tibble(
      timepoint = tp_lab, n = 0, mean = NA_real_, sd = NA_real_,
      range = "NA - NA", q_2.5 = NA_real_, q_25 = NA_real_, q_50 = NA_real_,
      q_75 = NA_real_, q_97.5 = NA_real_
    ))
  }
  qs  <- stats::quantile(vec, probs = c(.025,.25,.5,.75,.975), na.rm = TRUE)
  rng <- range(vec, na.rm = TRUE)
  tibble::tibble(
    timepoint = tp_lab,
    n    = sum(!is.na(vec)),
    mean = round(mean(vec, na.rm = TRUE), 2),
    sd   = round(sd(vec,   na.rm = TRUE), 2),
    range = paste(round(rng[1],2), "-", round(rng[2],2)),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2)
  )
}

stress1_summary <- stress1_long %>%
  dplyr::group_by(timepoint) %>%
  dplyr::summarise(
    dplyr::across(stress_skala1_sum, list(
      n = ~sum(!is.na(.)),
      mean = ~round(mean(., na.rm = TRUE), 2),
      sd   = ~round(sd(.,   na.rm = TRUE), 2),
      min  = ~round(min(., na.rm = TRUE), 2),
      max  = ~round(max(., na.rm = TRUE), 2)
    ), .names = "{.fn}")
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

stress_excessive_demands_all <- purrr::imap_dfr(timepoints, function(ev_value, tp_name) {
  df_tp <- data %>% filter(.data[[ev]] == ev_value)
  summarize_num_tp(df_tp, tp_name, "stress_skala2_sum")
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
    mean = round(m, 2),
    sd   = round(s, 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2),
    ci_lower = round(ciL, 2),
    ci_upper = round(ciU, 2)
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
    mean = round(m, 2),
    sd   = round(s, 2),
    range = paste(rng, collapse = " - "),
    q_2.5  = round(qs[1], 2),
    q_25   = round(qs[2], 2),
    q_50   = round(qs[3], 2),
    q_75   = round(qs[4], 2),
    q_97.5 = round(qs[5], 2),
    ci_lower = if (n > 1) round(m - z * s / sqrt(n), 2) else NA_real_,
    ci_upper = if (n > 1) round(m + z * s / sqrt(n), 2) else NA_real_
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

###Boxplot ----
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

### ANOVA ----
anova_results <- cort_long_clean %>%
  group_by(sex) %>%
  anova_test(dv = cortisol, between = timepoint)  # (between) da IDs evtl. nicht identisch sind

anova_results

## Post-hoc
posthoc_results <- cort_long_clean %>%
  group_by(sex) %>%
  tukey_hsd(cortisol ~ timepoint)

posthoc_results



#LM/Bayes Vorbereitungen----

# Fragebogen
EV_T1_Q <- "fragebogen_t1_arm_1"
EV_T2_Q <- "fragebogen_t2_arm_1"
EV_T3_Q <- "fragebogen_t3_arm_1"
EV_T4_Q <- "finaler_fragebogen_arm_1"

# Untersuchung (HCC)
EV_HCC_T1 <- "untersuchung_t1_arm_1"
EV_HCC_T2 <- "untersuchung_t3_arm_1"
EV_HCC_T3 <- "untersuchung_t3_arm_1"


# Pain-Mittelwert je Zeile
data <- data %>%
  mutate(pain_avg = rowMeans(across(c(paindetect1, paindetect2, paindetect3)), na.rm = TRUE))

# Aktivitätsmuster (AR/EER/DER/FAR)
data <- data %>%
  mutate(activity_pattern = case_when(
    score_pps < 3 & dms_score == 2 ~ "FAR",
    score_pps >= 3 & dms_score == 2 ~ "DER",
    score_pps >= 3 & dms_score < 2 ~ "EER",
    score_pps < 3 & dms_score < 2 ~ "AR",
    TRUE ~ NA_character_
  )) %>%
  mutate(activity_pattern = factor(activity_pattern, levels = c("AR","EER","DER","FAR")))


# Outcomes & Kovariaten

# Outcome T4 (12M): Pain
pain_T4 <- data %>%
  filter(redcap_event_name == EV_T4_Q) %>%
  transmute(study_id, pain_T4 = clean_num(pain_avg))

# Outcome T4 (12M): NDI
ndi_T4 <- data %>%
  filter(redcap_event_name == EV_T4_Q) %>%
  transmute(study_id, ndiscore_T4 = clean_num(ndiscore))

# Kovariaten (T1, Fragebogen)
covars_T1 <- data %>%
  filter(redcap_event_name == EV_T1_Q) %>%
  transmute(
    study_id,
    age    = clean_num(age),
    gender = factor(gender),
    dass21_depression = clean_num(dass21_depression)
  )


# Prädiktoren T1–T3

# Pain T1–T3
pain_T1 <- data %>% filter(redcap_event_name == EV_T1_Q) %>%
  transmute(study_id, pain_T1 = clean_num(pain_avg))
pain_T2 <- data %>% filter(redcap_event_name == EV_T2_Q) %>%
  transmute(study_id, pain_T2 = clean_num(pain_avg))
pain_T3 <- data %>% filter(redcap_event_name == EV_T3_Q) %>%
  transmute(study_id, pain_T3 = clean_num(pain_avg))

# Aktivitätsmuster T1–T3
act_T1 <- data %>% filter(redcap_event_name == EV_T1_Q) %>%
  transmute(study_id, activity_T1 = activity_pattern)
act_T2 <- data %>% filter(redcap_event_name == EV_T2_Q) %>%
  transmute(study_id, activity_T2 = activity_pattern)
act_T3 <- data %>% filter(redcap_event_name == EV_T3_Q) %>%
  transmute(study_id, activity_T3 = activity_pattern)

# Stress (Skalen) T1–T3
stress_T1 <- data %>% filter(redcap_event_name == EV_T1_Q) %>%
  transmute(
    study_id,
    stress1_T1 = clean_num(score_stress_skala_1),
    stress2_T1 = clean_num(score_stress_skala_2),
    sympt_T1   = clean_num(gesamtscore_symptom)
  )
stress_T2 <- data %>% filter(redcap_event_name == EV_T2_Q) %>%
  transmute(
    study_id,
    stress1_T2 = clean_num(score_stress_skala_1),
    stress2_T2 = clean_num(score_stress_skala_2),
    sympt_T2   = clean_num(gesamtscore_symptom)
  )
stress_T3 <- data %>% filter(redcap_event_name == EV_T3_Q) %>%
  transmute(
    study_id,
    stress1_T3 = clean_num(score_stress_skala_1),
    stress2_T3 = clean_num(score_stress_skala_2),
    sympt_T3   = clean_num(gesamtscore_symptom)
  )

# HCC T1–T3
hcc_T1 <- data %>% filter(redcap_event_name == EV_HCC_T1) %>%
  transmute(study_id,
            cortisol_T1  = clean_num(cortisol1),
            cortisone_T1 = clean_num(cortisone1))
hcc_T2 <- data %>% filter(redcap_event_name == EV_HCC_T2) %>%
  transmute(study_id,
            cortisol_T2  = clean_num(cortisol2),
            cortisone_T2 = clean_num(cortisone2))
hcc_T3 <- data %>% filter(redcap_event_name == EV_HCC_T3) %>%
  transmute(study_id,
            cortisol_T3  = clean_num(cortisol1),
            cortisone_T3 = clean_num(cortisone1))

# Datensätze für Modelle (Pain & NDI)

# Pain – Prädiktionsdatensatz
dat_pred <- pain_T4 %>%
  left_join(covars_T1,  by = "study_id") %>%
  left_join(pain_T1,    by = "study_id") %>%
  left_join(pain_T2,    by = "study_id") %>%
  left_join(pain_T3,    by = "study_id") %>%
  left_join(act_T1,     by = "study_id") %>%
  left_join(act_T2,     by = "study_id") %>%
  left_join(act_T3,     by = "study_id") %>%
  left_join(stress_T1,  by = "study_id") %>%
  left_join(stress_T2,  by = "study_id") %>%
  left_join(stress_T3,  by = "study_id") %>%
  left_join(hcc_T1,     by = "study_id") %>%
  left_join(hcc_T2,     by = "study_id") %>%
  left_join(hcc_T3,     by = "study_id") %>%
  mutate(
    activity_T1 = relevel(activity_T1, ref = "AR"),
    activity_T2 = relevel(activity_T2, ref = "AR"),
    activity_T3 = relevel(activity_T3, ref = "AR")
  )

# NDI – Prädiktionsdatensatz (Outcome NDI, Prädiktoren: pain T1–T3)
dat_pred_ndi <- ndi_T4 %>%
  left_join(covars_T1,  by = "study_id") %>%
  left_join(pain_T1,    by = "study_id") %>%
  left_join(pain_T2,    by = "study_id") %>%
  left_join(pain_T3,    by = "study_id") %>%
  left_join(act_T1,     by = "study_id") %>%
  left_join(act_T2,     by = "study_id") %>%
  left_join(act_T3,     by = "study_id") %>%
  left_join(stress_T1,  by = "study_id") %>%
  left_join(stress_T2,  by = "study_id") %>%
  left_join(stress_T3,  by = "study_id") %>%
  left_join(hcc_T1,     by = "study_id") %>%
  left_join(hcc_T2,     by = "study_id") %>%
  left_join(hcc_T3,     by = "study_id") %>%
  mutate(
    activity_T1 = relevel(activity_T1, ref = "AR"),
    activity_T2 = relevel(activity_T2, ref = "AR"),
    activity_T3 = relevel(activity_T3, ref = "AR")
  )


# Imputation (kNN) – Outcomes NICHT imputieren
vars_for_imputation <- c(
  "age","gender","dass21_depression",
  "pain_T1","pain_T2","pain_T3",
  "activity_T1","activity_T2","activity_T3",
  "stress1_T1","stress1_T2","stress1_T3",
  "stress2_T1","stress2_T2","stress2_T3",
  "sympt_T1","sympt_T2","sympt_T3",
  "cortisol_T1","cortisol_T2","cortisol_T3",
  "cortisone_T1","cortisone_T2","cortisone_T3"
)

# Pain
vars_exist <- intersect(vars_for_imputation, names(dat_pred))
dat_imp <- VIM::kNN(dat_pred, variable = vars_exist, k = 5) |>
  dplyr::select(-dplyr::ends_with("_imp"))

# NDI
vars_exist_ndi <- intersect(vars_for_imputation, names(dat_pred_ndi))
dat_imp_ndi <- VIM::kNN(dat_pred_ndi, variable = vars_exist_ndi, k = 5) |>
  dplyr::select(-dplyr::ends_with("_imp"))


# Pain T2 – nur für die Anzeige der auffälligen Fälle (ohne Ausschluss)
form_T2 <- pain_T4 ~ age + gender + dass21_depression +
  pain_T2 + activity_T2 + stress1_T2 + stress2_T2 + sympt_T2 +
  cortisol_T2 + cortisone_T2

mf2_pain <- model.frame(form_T2, data = dat_imp)
m2_pain  <- lm(form_T2, data = mf2_pain)
hv_pain  <- hatvalues(m2_pain)

cut_pain <- 2 * mean(hv_pain, na.rm = TRUE)
idx_hi_pain <- which(hv_pain > cut_pain)
if (length(idx_hi_pain)) {
  cat("\n[Prüfung pain/T2] Fälle mit hohem Leverage (> 2*mean(h)): \n")
  print(cbind(row = idx_hi_pain, hat = round(hv_pain[idx_hi_pain], 3)))
  ids_pain <- dat_imp$study_id[as.numeric(rownames(model.frame(form_T2, data = dat_imp)))[idx_hi_pain]]
  print(cbind(row = idx_hi_pain, study_id = ids_pain))
}

# NDI T2 – nur Anzeige (ohne Ausschluss)
formNDI_T2 <- ndiscore_T4 ~ age + gender + dass21_depression +
  pain_T2 + activity_T2 + stress1_T2 + stress2_T2 + sympt_T2 +
  cortisol_T2 + cortisone_T2

mf2_ndi <- model.frame(formNDI_T2, data = dat_imp_ndi)
m2_ndi  <- lm(formNDI_T2, data = mf2_ndi)
hv_ndi  <- hatvalues(m2_ndi)

cut_ndi <- 2 * mean(hv_ndi, na.rm = TRUE)
idx_hi_ndi <- which(hv_ndi > cut_ndi)
if (length(idx_hi_ndi)) {
  cat("\n[Prüfung NDI/T2] Fälle mit hohem Leverage (> 2*mean(h)): \n")
  print(cbind(row = idx_hi_ndi, hat = round(hv_ndi[idx_hi_ndi], 3)))
  ids_ndi <- dat_imp_ndi$study_id[as.numeric(rownames(model.frame(formNDI_T2, data = dat_imp_ndi)))[idx_hi_ndi]]
  print(cbind(row = idx_hi_ndi, study_id = ids_ndi))
}


# FINALE ANALYSE: bekannte Influentials entfernen (456, 644)

drop_ids <- c(456, 644)

dat_imp     <- subset(dat_imp,     !(study_id %in% drop_ids))
dat_imp_ndi <- subset(dat_imp_ndi, !(study_id %in% drop_ids))


# Helper: LM vs. LMM 

fit_Tx_to_T4 <- function(form_fix, data){
  n_obs  <- nrow(data)
  n_grp  <- dplyr::n_distinct(data$study_id)
  if (n_grp < n_obs) {
    lme4::lmer(update(form_fix, . ~ . + (1|study_id)), data = data, REML = TRUE)
  } else {
    stats::lm(form_fix, data = data)
  }
}


# Formeln – Pain (Response = pain_T4)

form_T1 <- pain_T4 ~ age + gender + dass21_depression +
  pain_T1 + activity_T1 + stress1_T1 + stress2_T1 + sympt_T1 +
  cortisol_T1 + cortisone_T1

# form_T2 bereits oben definiert
form_T3 <- pain_T4 ~ age + gender + dass21_depression +
  pain_T3 + activity_T3 + stress1_T3 + stress2_T3 + sympt_T3 +
  cortisol_T3 + cortisone_T3


# Fits – Pain (nach Ausschluss)

m_T1 <- fit_Tx_to_T4(form_T1, dat_imp)
m_T2 <- fit_Tx_to_T4(form_T2, dat_imp)
m_T3 <- fit_Tx_to_T4(form_T3, dat_imp)

# Robuste SE (nur falls LM; HC2)
if (inherits(m_T1, "lm")) {
  cat("\n== Pain T1 → T4 (HC2) ==\n")
  print(coeftest(m_T1, vcov = sandwich::vcovHC(m_T1, type = "HC2")))
}
if (inherits(m_T2, "lm")) {
  cat("\n== Pain T2 → T4 (HC2) ==\n")
  print(coeftest(m_T2, vcov = sandwich::vcovHC(m_T2, type = "HC2")))
}
if (inherits(m_T3, "lm")) {
  cat("\n== Pain T3 → T4 (HC2) ==\n")
  print(coeftest(m_T3, vcov = sandwich::vcovHC(m_T3, type = "HC2")))
}


## Bayes – Pain ----

pri <- c(
  prior(normal(0, 2), class = "b"),
  prior(normal(0, 5), class = "Intercept"),
  prior(exponential(1), class = "sigma")
)


###brms: student----
m_brms_T1_student <- brm(formula = form_T1, data = dat_imp, family = student(),
                         prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                         control = list(adapt_delta = 0.98))

m_brms_T2_student <- brm(formula = form_T2, data = dat_imp, family = student(),
                         prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                         control = list(adapt_delta = 0.98))

m_brms_T3_student <- brm(formula = form_T3, data = dat_imp, family = student(),
                         prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                         control = list(adapt_delta = 0.98))

# Checks: student-Ergebnisse
summary(m_brms_T1_student); bayes_R2(m_brms_T1_student); pp_check(m_brms_T1_student); fixef(m_brms_T1_student)
summary(m_brms_T2_student); bayes_R2(m_brms_T2_student); pp_check(m_brms_T2_student); fixef(m_brms_T2_student)
summary(m_brms_T3_student); bayes_R2(m_brms_T3_student); pp_check(m_brms_T3_student); fixef(m_brms_T3_student)

###brms: gaussian----
m_brms_T1_gaussian <- brm(formula = form_T1, data = dat_imp, family = gaussian(),
                          prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                          control = list(adapt_delta = 0.98))

m_brms_T2_gaussian <- brm(formula = form_T2, data = dat_imp, family = gaussian(),
                          prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                          control = list(adapt_delta = 0.98))

m_brms_T3_gaussian <- brm(formula = form_T3, data = dat_imp, family = gaussian(),
                          prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                          control = list(adapt_delta = 0.98))

# Checks: gaussian
summary(m_brms_T1_gaussian); bayes_R2(m_brms_T1_gaussian); pp_check(m_brms_T1_gaussian); fixef(m_brms_T1_gaussian)
summary(m_brms_T2_gaussian); bayes_R2(m_brms_T2_gaussian); pp_check(m_brms_T2_gaussian); fixef(m_brms_T2_gaussian)
summary(m_brms_T3_gaussian); bayes_R2(m_brms_T3_gaussian); pp_check(m_brms_T3_gaussian); fixef(m_brms_T3_gaussian)


### LOO–Vergleich----
library(brms)
library(loo)
library(dplyr)
library(tibble)
library(purrr)

# "sicheres" loo() mit moment_match und ggf. reloo
safe_loo <- function(fit, mm = TRUE, reloo_if_bad = TRUE, k_thresh = 0.7) {
  x <- loo(fit, moment_match = mm)
  k <- x$diagnostics$pareto_k
  if (reloo_if_bad && any(k > k_thresh, na.rm = TRUE)) {
    message("Recomputing LOO with reloo=TRUE due to high Pareto-k ...")
    x <- loo(fit, reloo = TRUE)
  }
  x
}

# Vergleich eines Paares: Gaussian vs Student
compare_pair <- function(fit_gauss, fit_student, label = "T?") {
  lg <- safe_loo(fit_gauss)
  ls <- safe_loo(fit_student)
  
  cmp <- loo_compare(lg, ls)  # erste Zeile = besser
  
  # WICHTIG: hier die loo-Objekte (lg, ls) übergeben, nicht die Fits!
  w_stack <- loo_model_weights(list(lg, ls), weights = "stacking")
  w_pbma  <- loo_model_weights(list(lg, ls), weights = "pseudobma")
  
  tab_main <- as_tibble(as.data.frame(cmp), rownames = "model") |>
    mutate(label = label, .before = 1)
  
  tab_weights <- tibble(
    label = label,
    model = c("gaussian", "student"),
    w_stacking = as.numeric(w_stack),
    w_pseudoBMA = as.numeric(w_pbma)
  )
  
  list(
    loo_gaussian = lg,
    loo_student  = ls,
    compare      = tab_main,
    weights      = tab_weights,
    pareto_k_gaussian = pareto_k_table(lg),
    pareto_k_student  = pareto_k_table(ls)
  )
}

# --- Beispiele aufrufen (Pain) ---
res_T1 <- compare_pair(m_brms_T1_gaussian, m_brms_T1_student, label = "T1→T4")
res_T2 <- compare_pair(m_brms_T2_gaussian, m_brms_T2_student, label = "T2→T4")
res_T3 <- compare_pair(m_brms_T3_gaussian, m_brms_T3_student, label = "T3→T4")

cmp_all <- bind_rows(res_T1$compare, res_T2$compare, res_T3$compare)
w_all   <- bind_rows(res_T1$weights, res_T2$weights, res_T3$weights)

cat("\n=== LOO comparison (ΔELPD, SE) ===\n"); print(cmp_all)
cat("\n=== Model weights (stacking & pseudo-BMA) ===\n"); print(w_all)

cat("\n=== Pareto-k summary: Gaussian ===\n")
print(res_T1$pareto_k_gaussian); print(res_T2$pareto_k_gaussian); print(res_T3$pareto_k_gaussian)
cat("\n=== Pareto-k summary: Student ===\n")
print(res_T1$pareto_k_student);  print(res_T2$pareto_k_student);  print(res_T3$pareto_k_student)


# Bayes - NDI ----

formNDI_T1 <- ndiscore_T4 ~ age + gender + dass21_depression +
  pain_T1 + activity_T1 + stress1_T1 + stress2_T1 + sympt_T1 +
  cortisol_T1 + cortisone_T1

# formNDI_T2 bereits oben definiert
formNDI_T3 <- ndiscore_T4 ~ age + gender + dass21_depression +
  pain_T3 + activity_T3 + stress1_T3 + stress2_T3 + sympt_T3 +
  cortisol_T3 + cortisone_T3


# Fits – NDI (nach Ausschluss)

mNDI_T1 <- fit_Tx_to_T4(formNDI_T1, dat_imp_ndi)
mNDI_T2 <- fit_Tx_to_T4(formNDI_T2, dat_imp_ndi)
mNDI_T3 <- fit_Tx_to_T4(formNDI_T3, dat_imp_ndi)

# Robuste SE (nur falls LM; HC2)
if (inherits(mNDI_T1, "lm")) {
  cat("\n== NDI T1 → T4 (HC2) ==\n")
  print(coeftest(mNDI_T1, vcov = sandwich::vcovHC(mNDI_T1, type = "HC2")))
}
if (inherits(mNDI_T2, "lm")) {
  cat("\n== NDI T2 → T4 (HC2) ==\n")
  print(coeftest(mNDI_T2, vcov = sandwich::vcovHC(mNDI_T2, type = "HC2")))
}
if (inherits(mNDI_T3, "lm")) {
  cat("\n== NDI T3 → T4 (HC2) ==\n")
  print(coeftest(mNDI_T3, vcov = sandwich::vcovHC(mNDI_T3, type = "HC2")))
}


### brms: gaussian----

m_brms_NDI_T1_gaussian <- brm(formula = formNDI_T1, data = dat_imp_ndi, family = gaussian(),
                              prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                              control = list(adapt_delta = 0.98))

m_brms_NDI_T2_gaussian <- brm(formula = formNDI_T2, data = dat_imp_ndi, family = gaussian(),
                              prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                              control = list(adapt_delta = 0.98))

m_brms_NDI_T3_gaussian <- brm(formula = formNDI_T3, data = dat_imp_ndi, family = gaussian(),
                              prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                              control = list(adapt_delta = 0.98))

# Checks: gaussian
summary(m_brms_NDI_T1_gaussian); bayes_R2(m_brms_NDI_T1_gaussian); pp_check(m_brms_NDI_T1_gaussian); fixef(m_brms_NDI_T1_gaussian)
summary(m_brms_NDI_T2_gaussian); bayes_R2(m_brms_NDI_T2_gaussian); pp_check(m_brms_NDI_T2_gaussian); fixef(m_brms_NDI_T2_gaussian)
summary(m_brms_NDI_T3_gaussian); bayes_R2(m_brms_NDI_T3_gaussian); pp_check(m_brms_NDI_T3_gaussian); fixef(m_brms_NDI_T3_gaussian)


### brms: student ----- 

m_brms_NDI_T1_student <- brm(formula = formNDI_T1, data = dat_imp_ndi, family = student(),
                             prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                             control = list(adapt_delta = 0.98))

m_brms_NDI_T2_student <- brm(formula = formNDI_T2, data = dat_imp_ndi, family = student(),
                             prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                             control = list(adapt_delta = 0.98))

m_brms_NDI_T3_student <- brm(formula = formNDI_T3, data = dat_imp_ndi, family = student(),
                             prior = pri, chains = 2, iter = 4000, warmup = 1000, cores = 2,
                             control = list(adapt_delta = 0.98))

## Checks: student
summary(m_brms_NDI_T1_student); bayes_R2(m_brms_NDI_T1_student); pp_check(m_brms_NDI_T1_student); fixef(m_brms_NDI_T1_student)
summary(m_brms_NDI_T2_student); bayes_R2(m_brms_NDI_T2_student); pp_check(m_brms_NDI_T2_student); fixef(m_brms_NDI_T2_student)
summary(m_brms_NDI_T3_student); bayes_R2(m_brms_NDI_T3_student); pp_check(m_brms_NDI_T3_student); fixef(m_brms_NDI_T3_student)


### LOO–Vergleich----

safe_loo <- function(fit, mm = TRUE, reloo_if_bad = TRUE, k_thresh = 0.7) {
  x <- loo(fit, moment_match = mm)
  k <- x$diagnostics$pareto_k
  if (reloo_if_bad && any(k > k_thresh, na.rm = TRUE)) {
    message("Recomputing LOO with reloo=TRUE due to high Pareto-k ...")
    x <- loo(fit, reloo = TRUE)
  }
  x
}

compare_pair_named <- function(fit_gauss, fit_student, label = "NDI T?→T4") {
  lg <- safe_loo(fit_gauss)
  ls <- safe_loo(fit_student)
  
  # Benannte Liste -> saubere Rownames
  cmp <- loo_compare(loos = list(gaussian = lg, student = ls))
  tab_main <- as_tibble(as.data.frame(cmp), rownames = "model") |>
    mutate(label = label, .before = 1)
  
  # Gewichte auch mit loo-Objekten
  w_stack <- loo_model_weights(list(lg, ls), weights = "stacking")
  w_pbma  <- loo_model_weights(list(lg, ls), weights = "pseudobma")
  tab_w <- tibble(
    label = label,
    model = c("gaussian","student"),
    w_stacking  = as.numeric(w_stack),
    w_pseudoBMA = as.numeric(w_pbma)
  )
  
  # ΔELPD (student − gaussian) explizit
  delta <- (ls$estimates["elpd_loo","Estimate"] - lg$estimates["elpd_loo","Estimate"])
  se    <- sqrt(ls$estimates["elpd_loo","SE"]^2 + lg$estimates["elpd_loo","SE"]^2)
  tab_delta <- tibble(label = label,
                      elpd_diff_student_minus_gaussian = delta,
                      se_diff = se)
  
  list(
    loo_gaussian = lg,
    loo_student  = ls,
    compare      = tab_main,
    weights      = tab_w,
    delta        = tab_delta,
    pareto_k_gaussian = pareto_k_table(lg),
    pareto_k_student  = pareto_k_table(ls)
  )
}

res_NDI_T1 <- compare_pair_named(m_brms_NDI_T1_gaussian, m_brms_NDI_T1_student, "NDI T1→T4")
res_NDI_T2 <- compare_pair_named(m_brms_NDI_T2_gaussian, m_brms_NDI_T2_student, "NDI T2→T4")
res_NDI_T3 <- compare_pair_named(m_brms_NDI_T3_gaussian, m_brms_NDI_T3_student, "NDI T3→T4")

cmp_all_NDI   <- bind_rows(res_NDI_T1$compare, res_NDI_T2$compare, res_NDI_T3$compare)
w_all_NDI     <- bind_rows(res_NDI_T1$weights, res_NDI_T2$weights, res_NDI_T3$weights)
delta_all_NDI <- bind_rows(res_NDI_T1$delta,   res_NDI_T2$delta,   res_NDI_T3$delta)

cat("\n=== NDI: LOO comparison (ΔELPD, SE) — named ===\n"); print(cmp_all_NDI)
cat("\n=== NDI: Model weights (stacking & pseudo-BMA) ===\n"); print(w_all_NDI)
cat("\n=== NDI: ΔELPD (student − gaussian) ===\n"); print(delta_all_NDI)

cat("\n=== NDI: Pareto-k summary — Gaussian ===\n")
print(res_NDI_T1$pareto_k_gaussian); print(res_NDI_T2$pareto_k_gaussian); print(res_NDI_T3$pareto_k_gaussian)
cat("\n=== NDI: Pareto-k summary — Student ===\n")
print(res_NDI_T1$pareto_k_student);  print(res_NDI_T2$pareto_k_student);  print(res_NDI_T3$pareto_k_student)

