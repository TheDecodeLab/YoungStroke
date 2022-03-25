# Libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(lsa)
library(tableone)

# Import data

GNSIS_DB <- read_excel("~/RProjects/FINAL_GNSIS_DATA/GNSIS_DATABASE_v7.5.1_7.15.2020.xlsx",
                       na = "NA",
                       col_names = TRUE)

GNSIS <- GNSIS_DB %>% 
  filter(AGE_AT_INDEX >= 18 & 
           PAST_ISCHEMIC_STROKE_AT_INDEX == 0 & 
           PAST_HEMORRHAGIC_STROKE_AT_INDEX == 0)

# The main ICD code file for patients
GNSIS_ALL_DX <- read_fwf("~/RProjects/GNSIS/GNSIS RAW DATA/STROKE_STRICT_ALL_DXS.txt", fwf_cols(PT_ID = c(1,26), INDEX_DT = c(27,37), ENC_DT = c(38,48), ENC_TYPE = c(49,303), ICD_CD = c(304,354), ICD_NM = c(355,611), CODE_SYSTEM = c(612,662), PRIMARY_YN = c(663,NA)), skip = 2, trim_ws = TRUE) %>% mutate_if(is.character, str_trim)

# Additional Admission ICD file
GNSIS_ADMISSION_ENC <- read_excel("~/RProjects/GNSIS/GNSIS RAW DATA/STROKE_STRICT_PROBLEM_LIST.xlsx", 
                                  sheet = "ADMISION_ENCS", na = "NULL") %>% mutate_if(is.character, str_trim)

GNSIS_ADMISSION_ENC <- GNSIS_ADMISSION_ENC %>% 
  rename(PRIMARY_YN = DX_PRIMARY_YN) %>% 
  mutate_if(is.POSIXct, as.Date)

# Combine all diagnoses, admission and problem list
GNSIS_ALL_DX <- rbind(GNSIS_ALL_DX, GNSIS_ADMISSION_ENC)

GNSIS_ALL_DX <- GNSIS_ALL_DX %>% 
  filter(CODE_SYSTEM == "ICD9CM" | CODE_SYSTEM == "ICD10CM")

GNSIS_ALL_DX <- GNSIS_ALL_DX %>% 
  mutate(vocabulary_id = CODE_SYSTEM,
         code = ICD_CD) %>% 
  filter(PT_ID %in% GNSIS$PT_ID)

# ICD to Phecodes
library(PheWAS)
GNSIS_PHECODE <- mapCodesToPhecodes(GNSIS_ALL_DX)

GNSIS_PHECODE <- addPhecodeInfo(GNSIS_PHECODE, groupnums = TRUE)

GNSIS_PHECODE_PREINDEX <- GNSIS_PHECODE %>% 
  filter(ENC_DT <= INDEX_DT)

GNSIS_PHECODE_PREINDEX_MAINGROUPS <- GNSIS_PHECODE_PREINDEX %>% 
  filter(str_detect(phecode, "\\.", negate = TRUE))


# Long to wide format
GNSIS_PHECODE_PREINDEX_MAINGROUPS2 <- GNSIS_PHECODE_PREINDEX_MAINGROUPS %>% 
  left_join(GNSIS[,c("PT_ID", "PT_SEX", "AGE_AT_INDEX")], by = "PT_ID") %>% 
  select(PT_ID, INDEX_DT, PT_SEX, AGE_AT_INDEX, phecode, description) %>% 
  group_by(PT_ID) %>% 
  distinct(phecode, .keep_all = TRUE) %>% 
  ungroup() %>% 
  arrange(phecode)

GNSIS_PHECODE_PREINDEX_MAINGROUPS3 <- GNSIS_PHECODE_PREINDEX_MAINGROUPS2 %>% 
  mutate(phecode_desc = paste("Phe", phecode, description, sep = " ")) %>% 
  select(-phecode, -description) %>% 
  mutate(value = 1) %>%
  pivot_wider(names_from = phecode_desc, values_from = value, values_fill = 0) 

GNSIS_PHECODE_PREINDEX_MAINGROUPS4 <- janitor::clean_names(GNSIS_PHECODE_PREINDEX_MAINGROUPS3)

# Summary table
summary_phecode <- as.data.frame(print(CreateTableOne(vars = names(GNSIS_PHECODE_PREINDEX_MAINGROUPS6),
                                                      factorVars = names(GNSIS_PHECODE_PREINDEX_MAINGROUPS6),
                                                      strata = "pt_sex",
                                                      data = GNSIS_PHECODE_PREINDEX_MAINGROUPS4,
                                                      addOverall = TRUE),
                                       nonnormal = var_list,
                                       noSpaces = TRUE))


# Phecode matrix of female patients
matrix_female <- GNSIS_PHECODE_PREINDEX_MAINGROUPS4 %>% 
  filter(pt_sex == "Female") %>% 
  select(-pt_sex) %>% 
  column_to_rownames(var = "pt_id") %>% 
  t()

# Female patients - cosine similarity
cosine_sim_female <- cosine(matrix_female)

diag(cosine_sim_female) <- NA_real_

# Median value 
cosine_sim_female_median <-
  as.data.frame(cosine_sim_female) %>%
  summarise_if(is.numeric, median, na.rm = TRUE) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "pt_id") %>% 
  rename(median_cosine_sim = "V1")

cosine_sim_female_median <- cosine_sim_female_median %>% 
  left_join(GNSIS_PHECODE_PREINDEX_MAINGROUPS4[,1:4], by = "pt_id")

# Phecode matrix male patients
matrix_male <- GNSIS_PHECODE_PREINDEX_MAINGROUPS4 %>% 
  filter(pt_sex == "Male") %>% 
  select(-pt_sex) %>% 
  column_to_rownames(var = "pt_id") %>% 
  t()

cosine_sim_male <- cosine(matrix_male)

diag(cosine_sim_male) <- NA_real_

# Median value
cosine_sim_male_median <-
  as.data.frame(cosine_sim_male) %>%
  summarise_if(is.numeric, median, na.rm = TRUE) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "pt_id") %>% 
  rename(median_cosine_sim = "V1")

cosine_sim_male_median <- cosine_sim_male_median %>% 
  left_join(GNSIS_PHECODE_PREINDEX_MAINGROUPS4[,1:4], by = "pt_id")

# Bind rows
overall_combined <- bind_rows(cosine_sim_female_median, cosine_sim_male_median)

# Cutpoint estimation
library(maxstat)

cutpoint_female <- maxstat.test(median_cosine_sim ~ age_at_index,
                                data = cosine_sim_female_median)

cutpoint_male <- maxstat.test(cs_value ~ age_at_index,
                              data = cosine_sim_male_median)

# Turning cutpoints from above to data-frame for plotting purpose
vline_dt <- data.frame(xint = c(53.7, 51),
                       pt_sex = c("Female", "Male"))

# Plot
cosine_sim_both_select_vars_all_points <- overall_combined %>% 
  ggplot(aes(x = age_at_index, y = mean_cosine_sim, group=pt_sex, color = pt_sex, fill = pt_sex)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", size = 1) +
  geom_vline(data = vline_dt, aes(xintercept = xint, color = pt_sex), linetype = "dashed") +
  geom_text(x=53.7, y=0, label="Female: 53.7 years", size=2.5, angle=90, vjust=-0.4, hjust=0, color = "black") +
  geom_text(x=51, y=0, label="Male: 51.0 years", size=2.5, angle=90, vjust=-0.4, hjust=0, color = "black") +
  scale_x_continuous(breaks=seq(20,90,5)) +
  labs(fill = "",
       color = "",
       x = "Age at Index Ischemic Stroke",
       y = "Median Cosine similarity") +
  theme_classic()+
  theme(legend.position="top") 
