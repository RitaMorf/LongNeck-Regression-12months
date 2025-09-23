# Data_Cleaning.R
#install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, lme4, GGally, car, performance, Matrix, 
               generics, tidyselect, brms, sjPlot, Rcpp, rstudioapi,
               bayesplot, todor)

# TODOs in Codes auflisten lassen:
todor::todor() 


# Daten bereinigen, vorbereiten
source("Data_Cleaning.R")

# Manual assignment of column names----
colnames(data) <- c("study_id","redcap_event_name","redcap_repeat_instrument",
                    "redcap_repeat_instance","paindetect1","paindetect2",
                    "paindetect3","ndi1","ndi2","ndi3","ndi4","ndi5","ndi6",
                    "ndi7","ndi8","ndi9","ndi10","ndiscore","cortisol1",
                    "cortisone1","score_stress_skala_1",
                    "score_stress_skala_2","score_stress_skala_3",
                    "gesamtscore_symptom","sci_stress1_1","sci_stress1_2",
                    "sci_stress1_3","sci_stress1_4","sci_stress1_5",
                    "sci_stress1_6","sci_stress1_7","sci_stress2_1",
                    "sci_stress2_2","sci_stress2_3","sci_stress2_4",
                    "sci_stress2_5","sci_stress2_6","sci_stress3_1",
                    "sci_stress3_2","sci_stress3_3","sci_stress3_4",
                    "sci_stress3_5","sci_stress3_6","sci_stress3_7",
                    "sci_symtpom1","sci_symtpom2","sci_symtpom3",
                    "sci_symtpom4","sci_symtpom5","sci_symtpom6",
                    "sci_symtpom7","sci_symtpom8","sci_symtpom9",
                    "sci_symtpom10","sci_symtpom11","sci_symtpom12",
                    "sci_symtpom13","pps1_light","pps2_light","pps3_light",
                    "pps4_light","pps5_light","pps6_light",
                    "pps7_light","pps1_strong","pps2_strong","pps3_strong",
                    "pps4_strong","pps5_strong","pps6_strong",
                    "pps7_strong","aefs_dms1","aefs_dms2","score_pps",
                    "dms_score","age", "gender", "cortisol2", "cortisone2",
                    "dropout", "smoking", "education", "occupation", 
                    "medication___0", "medication___1", "medication___2", 
                    "medication___3", "medication___4", "medication___5", 
                    "medication___6", "medication___na", "care___7",
                    "care___6", "care___5", "care___4", "care___3", "care___2",
                    "care___1", "care___0", "care___na",
                    "sleepquality", "dass21_depression","dass21_anxiety", 
                    "dass21_stress", "met_vigorous","met_moderate",
                    "met_walk", "met_total", "total_sitting_mins","stais_score",
                    "pvaq_score_total")

# Delete the first line
data <- data [-1, ]

# Define columns in which empty positions are to be replaced with NA
columns_to_fill <- c("study_id","redcap_event_name","redcap_repeat_instrument",
                     "redcap_repeat_instance","paindetect1","paindetect2",
                     "paindetect3","ndi1","ndi2","ndi3","ndi4","ndi5","ndi6",
                     "ndi7","ndi8","ndi9","ndi10","ndiscore","cortisol1",
                     "cortisone1","score_stress_skala_1",
                     "score_stress_skala_2","score_stress_skala_3",
                     "gesamtscore_symptom","sci_stress1_1","sci_stress1_2",
                     "sci_stress1_3","sci_stress1_4","sci_stress1_5",
                     "sci_stress1_6","sci_stress1_7","sci_stress2_1",
                     "sci_stress2_2","sci_stress2_3","sci_stress2_4",
                     "sci_stress2_5","sci_stress2_6","sci_stress3_1",
                     "sci_stress3_2","sci_stress3_3","sci_stress3_4",
                     "sci_stress3_5","sci_stress3_6","sci_stress3_7",
                     "sci_symtpom1","sci_symtpom2","sci_symtpom3",
                     "sci_symtpom4","sci_symtpom5","sci_symtpom6",
                     "sci_symtpom7","sci_symtpom8","sci_symtpom9",
                     "sci_symtpom10","sci_symtpom11","sci_symtpom12",
                     "sci_symtpom13","pps1_light","pps2_light","pps3_light",
                     "pps4_light","pps5_light","pps6_light",
                     "pps7_light","pps1_strong","pps2_strong","pps3_strong",
                     "pps4_strong","pps5_strong","pps6_strong",
                     "pps7_strong","aefs_dms1","aefs_dms2","score_pps",
                     "dms_score","age", "gender", "cortisol2", "cortisone2",
                     "dropout", "smoking", "education", "occupation", 
                     "medication___0", "medication___1", "medication___2", 
                     "medication___3", "medication___4", "medication___5", 
                     "medication___6", "medication___na", "care___7",
                     "care___6", "care___5", "care___4", "care___3", 
                     "care___2", "care___1", "care___0", "care___na",
                     "sleepquality", "dass21_depression","dass21_anxiety", 
                     "dass21_stress", "met_vigorous","met_moderate",
                     "met_walk", "met_total", "total_sitting_mins",
                     "stais_score", "pvaq_score_total")



# Replace empty positions ----
data[] <- lapply(data, function(x) {
  if (is.character(x)) x[x %in% c("", ".", "NA")] <- NA
  return(x)
})



#Filtering of rows from different measurements (questionnaires or clinical measurements)----
###dropout
data_dropout <- data %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  dplyr::select(study_id, dropout)

###age----
data_age <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, age)

###gender----
data_gender <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, gender)

###smoking----
data_smoking <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, smoking)

###education----
data_education <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, education)

###occupation----
data_occupation <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, occupation)

###medication----
data_nomedication <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, medication___0)

data_painkillers <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, medication___1)

data_opioids <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, medication___2)

data_antidepressants <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, medication___3)

data_musclerelaxants <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, medication___4)

data_cannabis <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, medication___5)

###medcare----
data_medcare_none <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___7)

data_medcare_generalpracticioner <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___6)

data_medcare_specialist <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___5)

data_physiotherapy <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___4)

data_chiropractic <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___3)

data_psychiatrist <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___2)

data_massage <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, care___1)

###pain----
data_pain_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, paindetect1, paindetect2, paindetect3)

data_pain_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, paindetect1, paindetect2, paindetect3)

data_pain_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, paindetect1, paindetect2, paindetect3)

data_pain_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, paindetect1, paindetect2, paindetect3)



###disability----
data_disability_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, ndiscore)

data_disability_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, ndiscore)

data_disability_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, ndiscore)

data_disability_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, ndiscore)

###depression----
data_depression_T1 <- data %>% 
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%  
  dplyr::select(study_id, dass21_depression) 

data_depression_T2 <- data %>% 
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%  
  dplyr::select(study_id, dass21_depression) 

data_depression_T3 <- data %>% 
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%  
  dplyr::select(study_id, dass21_depression) 

data_depression_T4 <- data %>% 
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%  
  dplyr::select(study_id, dass21_depression) 

###DASS-stress----
data_DASS_stress_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, dass21_stress)

data_DASS_stress_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, dass21_stress)

data_DASS_stress_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, dass21_stress)

data_DASS_stress_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, dass21_stress)

###anxiety----
data_STAIanxiety_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, stais_score)

data_STAIanxiety_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, stais_score)

data_STAIanxiety_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, stais_score)

data_STAIanxiety_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, stais_score)

###pain vigiliance----
data_PVAQPainVigilance_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, pvaq_score_total)

data_PVAQPainVigilance_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, pvaq_score_total)

data_PVAQPainVigilance_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, pvaq_score_total)

data_PVAQPainVigilance_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, pvaq_score_total)


###physical activity----
data_physicalactivity <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, met_total)

###sedentary activity----
data_sedentaryactivity <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, total_sitting_mins)

###sleep----
data_sleep <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, sleepquality)



### stress - uncertainty  ----
data_stress_uncertainty_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, stress_skala1_sum)

data_stress_uncertainty_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, stress_skala1_sum)

data_stress_uncertainty_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, stress_skala1_sum)

data_stress_uncertainty_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, stress_skala1_sum)

### stress - excessive demands ----
data_stress_excessive_demands_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, stress_skala2_sum)

data_stress_excessive_demands_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, stress_skala2_sum)

data_stress_excessive_demands_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, stress_skala2_sum)

data_stress_excessive_demands_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, stress_skala2_sum)

### stress - symptoms ----
data_stress_symptoms_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, gesamtscore_symptom)

data_stress_symptoms_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, gesamtscore_symptom)

data_stress_symptoms_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, gesamtscore_symptom)

data_stress_symptoms_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, gesamtscore_symptom)



###activity patterns----
data_activity_patterns_T1 <- data %>%
  filter(redcap_event_name == "fragebogen_t1_arm_1") %>%
  dplyr::select(study_id, score_pps,dms_score)

data_activity_patterns_T2 <- data %>%
  filter(redcap_event_name == "fragebogen_t2_arm_1") %>%
  dplyr::select(study_id, score_pps,dms_score)

data_activity_patterns_T3 <- data %>%
  filter(redcap_event_name == "fragebogen_t3_arm_1") %>%
  dplyr::select(study_id, score_pps,dms_score)

data_activity_patterns_T4 <- data %>%
  filter(redcap_event_name == "finaler_fragebogen_arm_1") %>%
  dplyr::select(study_id, score_pps,dms_score)


###HCC----
data_cortisol_T1 <- data %>%
  filter(redcap_event_name == "untersuchung_t1_arm_1") %>%
  dplyr::select(study_id, cortisol1, cortisone1, gender)

data_cortisol_T2 <- data %>%
  filter(redcap_event_name == "untersuchung_t3_arm_1") %>%
  dplyr::select(study_id, cortisol2, cortisone2, gender)

data_cortisol_T3 <- data %>%
  filter(redcap_event_name == "untersuchung_t3_arm_1") %>%
  dplyr::select(study_id, cortisol1, cortisone1, gender)

