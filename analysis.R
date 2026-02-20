library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(psych)
library(tidyr)
library(tibble)
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
  dplyr::mutate(
    education_parents = dplyr::case_when(
      education_parents %in% c(6, 7, 8) ~ "Low/Medium school degree",
      education_parents %in% c(9, 10)  ~ "Bachelor's/Master's degree",
      education_parents == 11 ~ "PhD",
      education_parents == -77 ~ "Don't know",
      education_parents == 12  ~ "Other",
      TRUE ~ NA_character_
    ),
    education_parents = factor(
      education_parents,
      levels = c(
        "Low/Medium school degree",
        "Bachelor's/Master's degree",
        "PhD",
        "Don't know",
        "Other"
      ),
      ordered = TRUE
    )
  )

summary(dat)
head(dat)
nrow(dat) # 115

# Fix NA declaration
# dat <- dat %>%
#   mutate(across(everything(), ~ ifelse(. == -77, NA, .)))

# Fix open text fields, e.g., match native languages
dat <- dat %>%
  mutate(
    native_consistent = str_squish(str_to_title(nativelanguage)),
    
    # Handle combined native languages first
    native_consistent = case_when(
      str_detect(native_consistent, "Russian.*German|German.*Russian") ~ "Russian and German",
      str_detect(native_consistent, "Ukrainian.*Greek|Greek.*Ukrainian") ~ "Ukrainian and Greek",
      str_detect(native_consistent, "Arabic.*French|French.*Arabic") ~ "Arabic and French",
      str_detect(native_consistent, "Persian.*Farsi|Farsi.*Persian") ~ "Farsi and Persian",
      str_detect(native_consistent, "Deutsch.*Spanisch|Spanisch.*Deutsch") ~ "German and Spanish",
      
      # Then clean up single-language entries
      str_detect(native_consistent, "Chi\\s*ne\\s*se|Chin|Chinesisch|Chinesich|Chinesiche") ~ "Chinese",
      str_detect(native_consistent, "Deutsch|German") ~ "German",
      str_detect(native_consistent, "English") ~ "English",
      str_detect(native_consistent, "Farsi|Persian") ~ "Farsi/Persian",
      str_detect(native_consistent, "Russian") ~ "Russian",
      str_detect(native_consistent, "Bulgarian") ~ "Bulgarian",
      str_detect(native_consistent, "Italian") ~ "Italian",
      str_detect(native_consistent, "Turkish") ~ "Turkish",
      str_detect(native_consistent, "Spanish") ~ "Spanish",
      str_detect(native_consistent, "Tamil") ~ "Tamil",
      str_detect(native_consistent, "Korean") ~ "Korean",
      str_detect(native_consistent, "Ukrainian|Ukranian") ~ "Ukrainian",
      str_detect(native_consistent, "Vietnamese") ~ "Vietnamese",
      
      # Handle unclear or anonymized answers
      str_detect(native_consistent, "Anonym") ~ "Prefer not to say",
      str_detect(native_consistent, "\\?{3,}") ~ NA_character_,
      
      TRUE ~ native_consistent
    )
  ) %>%
  dplyr::mutate(
    german_background = dplyr::case_when(
      native_consistent == "German" ~ "German",
      is.na(native_consistent) ~ NA_character_,
      TRUE ~ "Non-German"
    ),
    german_background = factor(
      german_background,
      levels = c("German", "Non-German")
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
  Native_Language = table(dat$native_consistent, useNA = "ifany"),
  Education_Parents = table(dat$education_parents, useNA = "ifany"),
  Disabilities = table(dat$disabilities, useNA = "ifany")
)

# View all tables
summary_tables


# Analysis ----------------------------------------------------------------
# Equality of Opportunity: Knowledge, Experience, Interest

# ============================================================
# Equality of Academic Opportunity – Loop Version
# Descriptives + ANOVA + Conditioned Models
# ============================================================

# ------------------------------------------------------------
# 1) Construct core variables
# ------------------------------------------------------------

dat2 <- dat %>%
  dplyr::mutate(
    gender = factor(gender),
    degree = factor(degree)
    # ,
    # education_parents = dplyr::na_if(education_parents, -1),
    # education_parents = dplyr::na_if(education_parents, -2)
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    Knowledge  = mean(c(Know1_general, Know2_publication, Know2_funding,
                        Know2_impact, Know2_career), na.rm = TRUE),
    Experience = mean(c(Exp_literature, Exp_programming, Exp_papers,
                        Exp_presenting, Exp_group,
                        Exp_independent, Exp_sponsoring), na.rm = TRUE),
    Interest   = mean(c(Int_overall, Int_statistics), na.rm = TRUE),
    Barriers   = mean(dplyr::c_across(dplyr::starts_with("Barr1_")), na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

core_vars  <- c("Knowledge","Experience","Interest","Barriers")
group_vars <- c("gender","degree","education_parents", "german_background")

# ------------------------------------------------------------
# 2) DESCRIPTIVES + VISUALIZATION (Looped)
# ------------------------------------------------------------

plot_list <- purrr::map(group_vars, function(g){
  
  purrr::map(core_vars, function(v){
    
    # force grouping var to be discrete (works for numeric education_parents too)
    tmp <- dat2 %>%
      dplyr::mutate(group_plot = factor(.data[[g]]))
    
    # compute n per plotted group
    n_data <- tmp %>%
      dplyr::group_by(group_plot) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(group_chr = as.character(group_plot))
    
    # labels "level (n=..)"
    labels_vec <- stats::setNames(
      paste0(n_data$group_chr, "\n(n=", n_data$n, ")"),
      n_data$group_chr
    )
    
    ggplot2::ggplot(tmp, ggplot2::aes(x = group_plot, y = .data[[v]])) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = 3) +
      ggplot2::stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2) +
      ggplot2::scale_x_discrete(labels = labels_vec) +
      ggplot2::labs(title = paste(v, "by", g), x = g, y = "Mean (95% CI)")
  })
  
})

purrr::walk(plot_list, purrr::walk, print)

# ------------------------------------------------------------
# 3) GLOBAL GROUP DIFFERENCE TESTS (ANOVA)
# ------------------------------------------------------------

anova_results <- purrr::map_dfr(group_vars, function(g){
  
  purrr::map_dfr(core_vars, function(v){
    
    model <- stats::aov(
      stats::as.formula(paste0(v, " ~ ", g)),
      data = dat2
    )
    
    broom::tidy(model) %>%
      dplyr::mutate(
        group = g,
        outcome = v
      )
  })
  
})

print(anova_results)


# ------------------------------------------------------------
# 4) CONDITIONED MODELS (Equality-of-Opportunity Logic)
# ------------------------------------------------------------
# Experience | Knowledge
# Interest   | Knowledge + Experience
# Barriers   | Knowledge + Experience
# ------------------------------------------------------------

conditioned_results <- purrr::map_dfr(group_vars, function(g){
  
  m_exp <- stats::lm(
    stats::as.formula(paste0("Experience ~ ", g, " + Knowledge")),
    data = dat2)
  
  m_int <- stats::lm(
    stats::as.formula(paste0("Interest ~ ", g, " + Knowledge + Experience")),
    data = dat2)
  
  m_bar <- stats::lm(
    stats::as.formula(paste0("Barriers ~ ", g, " + Knowledge + Experience")),
    data = dat2)
  
  dplyr::bind_rows(
    broom::tidy(m_exp) %>%
      dplyr::mutate(group = g,
                    model = "Experience | Knowledge",
                    outcome = "Experience"),
    
    broom::tidy(m_int) %>%
      dplyr::mutate(group = g,
                    model = "Interest | Knowledge+Experience",
                    outcome = "Interest"),
    
    broom::tidy(m_bar) %>%
      dplyr::mutate(group = g,
                    model = "Barriers | Knowledge+Experience",
                    outcome = "Barriers")
  ) %>%
    dplyr::filter(!term %in% c("(Intercept)","Knowledge","Experience"))
  
})

print(conditioned_results)