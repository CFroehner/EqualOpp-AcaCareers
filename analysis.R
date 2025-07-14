
library(dplyr)
library(tidyr)
library(stringr)

# Load & Preprocess -------------------------------------------------------

dat <- read.csv("surveydata2025.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
head(dat)

# Fix column names
dat <- dat %>%
  rename(
    stats_major = v_1,
    Know1_general = v_125,
    Know2_publication = v_147,
    Know2_funding = v_148,
    Know2_impact = v_149,
    Know2_career = v_150,
    Exp_literature = v_138,
    Exp_programming = v_139,
    Exp_papers = v_140,
    Exp_presenting = v_141,
    Exp_group = v_142,
    Exp_independent = v_143,
    Exp_sponsoring = v_145,
    Int_overall = v_136,
    Int_statistics = v_137,
    Barr1_competition = v_175,
    Barr1_guidance = v_176,
    Barr1_balance = v_178,
    Barr1_availability = v_179,
    Barr1_diversity = v_180,
    Barr1_mobility = v_181,
    Barr1_other = v_183,
    Barr1_none = v_182,
    Off_other = v_163,
    gender = v_53,
    degree = v_129,
    nativelanguage = v_158,
    education_parents = v_130,
    disabilities = v_159,
    opensugg = v_62
  ) 

colnames(dat)

# Fix response codes
dat <- dat %>%
  mutate(across(matches("^Know2|^Int|^Barr2"), ~ case_when(
    . == 6 ~ 1,
    . == 7 ~ 2,
    . == 8 ~ 3,
    . == 9 ~ 4,
    . == 10 ~ 5,
    TRUE ~ NA_real_  # in case other values exist
  ))) %>%
  mutate(across(matches("^Know1"), ~ case_when(
    . == 11 ~ 1,
    . == 12 ~ 2,
    . == 13 ~ 3,
    . == 14 ~ 4,
    . == 15 ~ 5,
    TRUE ~ NA_real_  # in case other values exist
  ))) %>%
  mutate(across(matches("^Exp"), ~ case_when(
    . == 19 ~ 1,
    . == 20 ~ 2,
    . == 21 ~ 3,
    . == 22 ~ 4,
    . == 23 ~ 5,
    TRUE ~ NA_real_
    ))) %>%
  mutate(across(all_of(starts_with("Prox3")), ~ case_when(
    . == 9 ~ 1,
    . == 10 ~ 2,
    . == 11 ~ 3,
    . == 12 ~ 4,
    . == 13 ~ 5,
    . == 14 ~ -1, # don't know
    TRUE ~ NA_real_
  ))) %>%
  mutate(across(all_of(matches("gender")), ~ case_when(
    . == 1 ~ "female",
    . == 2 ~ "male",
    . == 3 ~ "diverse",
    . == 4 ~ "not_identify",
    TRUE ~ NA_character_
  ))) %>%
  mutate(across(all_of(starts_with("degree")), ~ case_when(
    . == 6 ~ "undergraduate",
    . == 7 ~ "graduate",
    TRUE ~ NA_character_
  ))) %>%
  mutate(across(all_of(starts_with("education_parents")), ~ case_when(
    . == 6 ~ 1,
    . == 7 ~ 2,
    . == 8 ~ 3,
    . == 9 ~ 4,
    . == 10 ~ 5,
    . == 11 ~ 6,
    . == 13 ~ -1, # don't know
    . == 12 ~ -2, # other
    TRUE ~ NA_real_
  )))

summary(dat)
head(dat)
nrow(dat) # 115

# Fix NA declaration
dat <- dat %>%
  mutate(across(everything(), ~ ifelse(. == -77, NA, .)))

# Fix open text fields, e.g., match native languages
dat <- dat %>%
  mutate(
    native_clean = str_squish(str_to_title(nativelanguage)),
    
    # Handle combined native languages first
    native_clean = case_when(
      str_detect(native_clean, "Russian.*German|German.*Russian") ~ "Russian and German",
      str_detect(native_clean, "Ukrainian.*Greek|Greek.*Ukrainian") ~ "Ukrainian and Greek",
      str_detect(native_clean, "Arabic.*French|French.*Arabic") ~ "Arabic and French",
      str_detect(native_clean, "Persian.*Farsi|Farsi.*Persian") ~ "Farsi and Persian",
      str_detect(native_clean, "Deutsch.*Spanisch|Spanisch.*Deutsch") ~ "German and Spanish",
      
      # Then clean up single-language entries
      str_detect(native_clean, "Chi\\s*ne\\s*se|Chin|Chinesisch|Chinesich|Chinesiche") ~ "Chinese",
      str_detect(native_clean, "Deutsch|German") ~ "German",
      str_detect(native_clean, "English") ~ "English",
      str_detect(native_clean, "Farsi|Persian") ~ "Farsi/Persian",
      str_detect(native_clean, "Russian") ~ "Russian",
      str_detect(native_clean, "Bulgarian") ~ "Bulgarian",
      str_detect(native_clean, "Italian") ~ "Italian",
      str_detect(native_clean, "Turkish") ~ "Turkish",
      str_detect(native_clean, "Spanish") ~ "Spanish",
      str_detect(native_clean, "Tamil") ~ "Tamil",
      str_detect(native_clean, "Korean") ~ "Korean",
      str_detect(native_clean, "Ukrainian|Ukranian") ~ "Ukrainian",
      str_detect(native_clean, "Vietnamese") ~ "Vietnamese",
      
      # Handle unclear or anonymized answers
      str_detect(native_clean, "Anonym") ~ "Prefer not to say",
      str_detect(native_clean, "\\?{3,}") ~ NA_character_,
      
      TRUE ~ native_clean
    )
  )

# Cleaning ----------------------------------------------------------------

# remove non-stats majors
dat <- dat %>%
  filter(stats_major == 1)
nrow(dat) # 96

# Overview ----------------------------------------------------------------

summary_tables <- list(
  Gender = table(dat$gender, useNA = "ifany"),
  Education = table(dat$education, useNA = "ifany"),
  Gender_by_Education = table(dat$gender, dat$education, useNA = "ifany"),
  Semester = table(dat$Semester_nr, useNA = "ifany"),
  Native_Language = table(dat$native_clean, useNA = "ifany"),
  Education_Parents = table(dat$education_parents, useNA = "ifany"),
  Disabilities = table(dat$disabilities, useNA = "ifany")
)

# View all tables
summary_tables







