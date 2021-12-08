GNSIS_DATABASE_v7.5.1_07152020 <- read_excel("~/Clare-Clustering/corrected_version_10112021/GNSIS_DATABASE_v7.5.1_7.15.2020.xlsx") %>%
  filter(PAST_HEMORRHAGIC_STROKE_AT_INDEX==0 & PAST_ISCHEMIC_STROKE_AT_INDEX==0 & AGE_AT_INDEX >= 18) %>%
  mutate(label = 'X1')
CONTROLS1_DATABASE_v1.5_05.22.2021 <- read_excel("~/Clare-Clustering/corrected_version_10112021/CONTROLS1_DATABASE_v1.5_5.22.2021.xlsx") %>%
  filter(PAST_HEMORRHAGIC_STROKE_PREINDEX==0 & PAST_ISCHEMIC_STROKE_PREINDEX==0 & AGE_AT_INDEX >= 18) %>%
  group_by(PT_ID)%>%
  mutate(rank = rank(INDEX_DT), label = 'X0')%>%
  arrange(PT_ID,INDEX_DT)%>%
  filter(rank == 1)
jiang.labs <- read_excel("jiangLabs.xlsx",na=c("NA"),col_names = T)
ICD_ALL_compiled <- read_excel("~/Clare-Clustering/corrected_version_10112021/combined_list_of_PTID_ICD.xlsx", sheet = 'combined_ICD_list')
# remove PT_IDs from CONTROLS-1 that are common between GNSIS & CONTROLS-1 
CONTROLS1_DATABASE_v1.5_05.22.2021 <- subset(
  CONTROLS1_DATABASE_v1.5_05.22.2021,
  !(PT_ID %in% intersect(GNSIS_DATABASE_v7.5.1_07152020$PT_ID,
                         CONTROLS1_DATABASE_v1.5_05.22.2021$PT_ID))
)
PT_IDs_compiled <- left_join(PT_IDs_compiled, 
                             rbind(GNSIS_DATABASE_v7.5.1_07152020 %>% select(c('PT_ID','INDEX_DT')),
                                   CONTROLS1_DATABASE_v1.5_05.22.2021 %>% select(c('PT_ID', 'INDEX_DT'))
                             )
                             , by = 'PT_ID') %>%
  select(c('PT_ID', 'INDEX_DT', 'ENC_DT', 'ENC_TYPE', 'ICD', 'SOURCE'))
PT_IDs_compiled$ENC_DT <- as.POSIXct(PT_IDs_compiled$ENC_DT, format = "%Y-%m-%d")
# PT_IDs whom from CONTROLS-1 seems to have Ischemic, TIA and HEMORRHAGIC_STROKE
excl.CONTROLS1 <- left_join(PT_IDs_compiled,
                            ICD_ALL_compiled,
                            by = c('ICD'='ICD_CODE'))
excl.CONTROLS1 <- left_join(
  CONTROLS1_DATABASE_v1.5_05.22.2021,
  excl.CONTROLS1,
  by = c('PT_ID')
)
excl.CONTROLS1 <- excl.CONTROLS1 %>%
  filter(CONDITION %in% c('HEMORRHAGIC_STROKE','ISCHEMIC_STROKE','TIA')) %>%
  select(PT_ID, INDEX_ICD, CONDITION, ENC_DT) %>%
  distinct()
features.1 <- c("PT_ID", "label",
                "AFIB_FLUTTER_AT_INDEX",
                "ALCOHOL_DEP_ABUSE_AT_INDEX",
                #"ANXIETY_DIS_AT_INDEX",
                "BRAIN_TUMOR_AT_INDEX",
                "CHF_AT_INDEX",
                "CHRONIC_KIDNEY_DIS_AT_INDEX",
                "CHRONIC_LIVER_DIS_AT_INDEX",
                "CHRONIC_LUNG_DIS_AT_INDEX",
                "CIRRHOSIS_AT_INDEX",
                #"CONVERSION_DIS_AT_INDEX",
                #"CONVULSIONS_AT_INDEX",
                #"DEPRESSION_AT_INDEX",
                "DIABETES_AT_INDEX",
                "DYSLIPIDEMIA_AT_INDEX",
                #"EPILEPSY_AT_INDEX",
                "ESRD_AT_INDEX",
                "FAM_HEART_HIST",
                "FAM_STROKE_HIST",
                "HEPATIC_ENCEPHALOPATHY_AT_INDEX",
                "HYPERCOAG_STATES_AT_INDEX",
                "HYPERTENSION_AT_INDEX",
                "INDEX_DT",
                #"MANIC_BIPOLAR_DIS_AT_INDEX",
                "MI_AT_INDEX",
                "MIGRAINE_AT_INDEX",
                "NEOPLASM_AT_INDEX",
                "PERI_VASC_DIS_AT_INDEX",
                #"PERIPHERAL_NEUROPATHY_AT_INDEX",
                "PFO_ALL_TIME",
                "PT_SEX",
                "INDEX_DT",
                "SMOKE_STTS",
                #"SYNCOPE_AT_INDEX",
                "OPIOID_DEP_ABUSE_AT_INDEX",
                "CANNABIS_DEP_ABUSE_AT_INDEX",
                "COCAINE_DEP_ABUSE_AT_INDEX",
                "OTHER_DRUG_DEP_ABUSE_AT_INDEX",
                "AGE_AT_INDEX"
)
features.2 <- c("PT_SEX","PT_ID",
                "BMI_CLOSEST_TO_INDEX",
                "LDL_CLOSEST_TO_INDEX",
                "HBA1C_CLOSEST_TO_INDEX",
                "HDL_CLOSEST_TO_INDEX",
                "HB_CLOSEST_TO_INDEX",
                "PLT_CLOSEST_TO_INDEX",
                "CREATININE_CLOSEST_TO_INDEX")
pre_index<-function(var,icd){
  PT_IDs_compiled%>%
    mutate(!!enquo(var) := if_else(ICD %in% icd & ENC_DT < INDEX_DT,1,0))%>%
    filter(!!enquo(var) == 1)%>%
    select("PT_ID",!!enquo(var))%>%
    distinct()
}
at_index<-function(var,icd){
  PT_IDs_compiled%>%
    mutate(!!enquo(var) := if_else(ICD %in% icd & ENC_DT <= INDEX_DT,1,0))%>%
    filter(!!enquo(var) == 1)%>%
    select("PT_ID",!!enquo(var))%>%
    distinct()
}
all_time<-function(var,icd){
  PT_IDs_compiled%>%
    mutate(!!enquo(var) := if_else(ICD %in% icd,1,0))%>%
    filter((!!enquo(var)) == 1)%>%
    select("PT_ID",!!enquo(var))%>%
    distinct()
}
tmp.1 <- rbind(
  GNSIS_DATABASE_v7.5.1_07152020 %>% select(features.1),
  anti_join(
    CONTROLS1_DATABASE_v1.5_05.22.2021,
    excl.CONTROLS1['PT_ID'] %>% distinct(),
    by = 'PT_ID'
  ) %>% 
    rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)}) %>%
    select(features.1)
) %>%
  mutate(DRUG_DEP_ABUSE_AT_INDEX = if_else((OPIOID_DEP_ABUSE_AT_INDEX == 1 | CANNABIS_DEP_ABUSE_AT_INDEX == 1 | 
                                              COCAINE_DEP_ABUSE_AT_INDEX == 1 | OTHER_DRUG_DEP_ABUSE_AT_INDEX==1),1,0),
         PT_SEX = if_else(PT_SEX %in% 'Male',1,0),
         SMOKE_STTS = if_else(SMOKE_STTS %in% 'CURRENT SMOKER',1,0))%>%
  left_join(., PT_IDs_compiled %>%
              mutate(HYPERTENSION_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'HYPERTENSION')$ICD_CODE)
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(HYPERTENSION_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","HYPERTENSION_AT_INDEX_3monthsPost")%>%
              distinct(),
            by = 'PT_ID'
  ) %>%
  left_join(., PT_IDs_compiled %>%
              mutate(TEMPORAL_ARTERITIS_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'TEMPORAL_ARTERITIS')$ICD_CODE)
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(TEMPORAL_ARTERITIS_3monthsPost==1)%>%
              select("PT_ID","TEMPORAL_ARTERITIS_3monthsPost")%>%
              distinct(),
            by = 'PT_ID'
  ) %>%
  left_join(., at_index(CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX, 
                        c(subset(ICD_ALL_compiled, CONDITION == 'CERVICOCEPHALIC_ARTERIAL_DISSECTION')$ICD_CODE)
  ), by = "PT_ID") %>%
  left_join(., at_index(NON_CNS_NEOPLASM_AT_INDEX, 
                        c(subset(ICD_ALL_compiled, CONDITION == 'NON_CNS_NEOPLASM')$ICD_CODE)
  ), by = "PT_ID") %>%
  left_join(., PT_IDs_compiled%>%
              mutate(CIRRHOSIS_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'Cirrhosis')$ICD_CODE)
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(CIRRHOSIS_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","CIRRHOSIS_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID") %>%
  left_join(., PT_IDs_compiled%>%
              mutate(HYPERCOAG_STATES_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'HYPERCOAG_STATES')$ICD_CODE)
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(HYPERCOAG_STATES_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","HYPERCOAG_STATES_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID") %>%
  left_join(., PT_IDs_compiled%>%
              mutate(RHEUM_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'RHEUM_DISEASE')$ICD_CODE)
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(RHEUM_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","RHEUM_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID") %>%
  left_join(., all_time(FIBROMUSCULAR_DYSPLASIA_ALL_TIME, 
                        c(subset(ICD_ALL_compiled, CONDITION == 'FIBROMUSCULAR')$ICD_CODE)
  ), by = "PT_ID") %>%
  left_join(., all_time(MOYAMOYA_ALL_TIME, 
                        c(subset(ICD_ALL_compiled, CONDITION == 'MOYAMOYA')$ICD_CODE)
  ), by = "PT_ID") %>%
  left_join(., all_time(SICKLE_CELL_ALL_TIME, 
                        c(subset(ICD_ALL_compiled, CONDITION == 'SICKLE_CELL')$ICD_CODE)
  ), by = "PT_ID") %>%
  left_join(., PT_IDs_compiled%>%
              mutate(AFIB_FLUTTER_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION %in% c('ATRIAL_FIB','ATRIAL_FLUTTER'))$ICD_CODE) 
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(AFIB_FLUTTER_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","AFIB_FLUTTER_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID") %>%
  left_join(., PT_IDs_compiled%>%
              mutate(VASCULITIS_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'VASCULITIS')$ICD_CODE)
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(VASCULITIS_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","VASCULITIS_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
  left_join(., PT_IDs_compiled%>%
              mutate(REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost = if_else(
                ICD %in% c(subset(ICD_ALL_compiled, CONDITION %in% c('CEREBRAL_VASOCONSTRICTION'))$ICD_CODE) 
                & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID") %>%
  replace(is.na(.), 0) %>%
  mutate(
    OTHER_ARTERIOPATHIES = if_else(
      VASCULITIS_AT_INDEX_3monthsPost == 1 | 
        FIBROMUSCULAR_DYSPLASIA_ALL_TIME == 1 |
        MOYAMOYA_ALL_TIME == 1 |
        REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost == 1 | 
        SICKLE_CELL_ALL_TIME == 1, 1, 0),
    OTHER_CHRONIC_SYSTEMIC_DISORDERS = if_else(
      CHRONIC_LIVER_DIS_AT_INDEX == 1 | CHRONIC_LUNG_DIS_AT_INDEX == 1 |
        CHRONIC_KIDNEY_DIS_AT_INDEX == 1 | CIRRHOSIS_AT_INDEX_3monthsPost == 1 |
        ESRD_AT_INDEX == 1 | HEPATIC_ENCEPHALOPATHY_AT_INDEX == 1, 
      1, 0),
    FAM_HIST = if_else(
      FAM_HEART_HIST == 1 | FAM_STROKE_HIST ==1, 1, 0)
  )

tmp.2 <- left_join(rbind(
  GNSIS_DATABASE_v7.5.1_07152020 %>% select(features.2),
  anti_join(
    CONTROLS1_DATABASE_v1.5_05.22.2021,
    excl.CONTROLS1['PT_ID'] %>% distinct(),
    by = 'PT_ID'
  ) %>% 
    rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)}) %>%
    select(features.2)
),
jiang.labs,
by = "PT_ID")%>%
  mutate_at(c(3:16), as.numeric) %>%
  mutate(                            
    PT_SEX = if_else(PT_SEX %in% 'Male',1,0),
    HB_CLOSEST_TO_INDEX=coalesce(HB_CLOSEST_TO_INDEX.x,HB_CLOSEST_TO_INDEX.y),
    HBA1C_CLOSEST_TO_INDEX=coalesce(HBA1C_CLOSEST_TO_INDEX.x,HBA1C_CLOSEST_TO_INDEX.y),
    HDL_CLOSEST_TO_INDEX=coalesce(HDL_CLOSEST_TO_INDEX.x,HDL_CLOSEST_TO_INDEX.y),
    LDL_CLOSEST_TO_INDEX=coalesce(LDL_CLOSEST_TO_INDEX.x,LDL_CLOSEST_TO_INDEX.y),
    PLT_CLOSEST_TO_INDEX=coalesce(PLT_CLOSEST_TO_INDEX.x,PLT_CLOSEST_TO_INDEX.y),
    CREATININE_CLOSEST_TO_INDEX=coalesce(CREATININE_CLOSEST_TO_INDEX.x,CREATININE_CLOSEST_TO_INDEX.y)
  )%>%
  select(c( PT_ID, PT_SEX,
            BMI_CLOSEST_TO_INDEX,
            PLT_CLOSEST_TO_INDEX,
            HB_CLOSEST_TO_INDEX,
            CREATININE_CLOSEST_TO_INDEX)) %>%
  mutate(   BMI_CLOSEST_TO_INDEX_BINARY=if_else(BMI_CLOSEST_TO_INDEX>25,1,0),
            PLT_CLOSEST_TO_INDEX_BINARY=if_else(PLT_CLOSEST_TO_INDEX>450,1,0),
            HB_CLOSEST_TO_INDEX_BINARY=if_else(
              ((PT_SEX==1 & HB_CLOSEST_TO_INDEX>=13.5 & HB_CLOSEST_TO_INDEX<=17.5)|
                 (PT_SEX==0 & HB_CLOSEST_TO_INDEX>=12 & HB_CLOSEST_TO_INDEX<=15.5)),0,1),
            CREATININE_CLOSEST_TO_INDEX_BINARY=if_else(
              ((PT_SEX==1 & CREATININE_CLOSEST_TO_INDEX<=1.4)|
                 (PT_SEX==0 & CREATININE_CLOSEST_TO_INDEX<=1.2)),0,1)
  )%>%
  select(c(BMI_CLOSEST_TO_INDEX_BINARY))
features <- c(
  'PT_SEX', 'AGE_AT_INDEX',
  'OTHER_ARTERIOPATHIES',
  'OTHER_CHRONIC_SYSTEMIC_DISORDERS',
  'HYPERCOAG_STATES_AT_INDEX_3monthsPost',
  'AFIB_FLUTTER_AT_INDEX_3monthsPost',
  'ALCOHOL_DEP_ABUSE_AT_INDEX',
  'CHF_AT_INDEX',
  'DIABETES_AT_INDEX',
  'DRUG_DEP_ABUSE_AT_INDEX',
  'FAM_HIST',
  'MI_AT_INDEX',
  'PERI_VASC_DIS_AT_INDEX',
  'PFO_ALL_TIME',
  'SMOKE_STTS',
  'BMI_CLOSEST_TO_INDEX_BINARY',
  'CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX',
  'NON_CNS_NEOPLASM_AT_INDEX',
  'BRAIN_TUMOR_AT_INDEX',
  'RHEUM_AT_INDEX_3monthsPost',
  'MIGRAINE_AT_INDEX',
  'TEMPORAL_ARTERITIS_3monthsPost',
  'HYPERTENSION_AT_INDEX_3monthsPost'
)
woImputation <- cbind(tmp.1, tmp.2) %>%
  select(c(
    'label', 'PT_SEX', 'AGE_AT_INDEX',
    'OTHER_ARTERIOPATHIES',
    'OTHER_CHRONIC_SYSTEMIC_DISORDERS',
    'HYPERCOAG_STATES_AT_INDEX_3monthsPost',
    'AFIB_FLUTTER_AT_INDEX_3monthsPost',
    'ALCOHOL_DEP_ABUSE_AT_INDEX',
    'CHF_AT_INDEX',
    'DIABETES_AT_INDEX',
    'DRUG_DEP_ABUSE_AT_INDEX',
    'FAM_HIST',
    'MI_AT_INDEX',
    'PERI_VASC_DIS_AT_INDEX',
    'PFO_ALL_TIME',
    'SMOKE_STTS',
    'BMI_CLOSEST_TO_INDEX_BINARY',
    'CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX',
    'NON_CNS_NEOPLASM_AT_INDEX',
    'BRAIN_TUMOR_AT_INDEX',
    'RHEUM_AT_INDEX_3monthsPost',
    'MIGRAINE_AT_INDEX',
    'TEMPORAL_ARTERITIS_3monthsPost',
    'HYPERTENSION_AT_INDEX_3monthsPost'
  ))
wImputation <- mice(woImputation[,-1],
                    m = 25, maxit = 25, method = 'pmm', seed = 99) 
wImputation <- cbind(complete(wImputation), woImputation['label'])
wImputation$label <- factor(wImputation$label)
#
age.buckets <- function(AGE, GENDER){
  subset(wImputation, AGE_AT_INDEX > AGE & PT_SEX == GENDER)                    # Tweak the AGE for > and <
}
df <- list()
i<- 0
for (age in seq(40, 75, by = 5)) {
  for (gender in seq(0, 1, by = 1)) {
    i <- i+1
    ifelse( gender == 0, 
            df[[i]] <- age.buckets(age, gender)%>%
              select(label, features[!features %in% c('PT_SEX')]),
            df[[i]] <- age.buckets(age, gender)%>%
              select(label, features[!features %in% c('PT_SEX')])
    )
  }
}
df[[33]] <- subset(wImputation, PT_SEX == 0) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[34]] <- subset(wImputation, PT_SEX == 1) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[35]] <- subset(wImputation, (AGE_AT_INDEX >= 55 & AGE_AT_INDEX <= 70) & PT_SEX == 0) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[36]] <- subset(wImputation, (AGE_AT_INDEX >= 55 & AGE_AT_INDEX <= 70) & PT_SEX == 1) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[37]] <- subset(wImputation, AGE_AT_INDEX <= 80 & PT_SEX == 0) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[38]] <- subset(wImputation, AGE_AT_INDEX <= 80 & PT_SEX == 1) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[39]] <- subset(wImputation, AGE_AT_INDEX > 80 & PT_SEX == 0) %>%
  select(label, features[!features %in% c('PT_SEX')])
df[[40]] <- subset(wImputation, AGE_AT_INDEX > 80 & PT_SEX == 1) %>%
  select(label, features[!features %in% c('PT_SEX')])
names(df) <- c(
  'female.age.lt.40', 'male.age.lt.40', 'female.age.lt.45', 'male.age.lt.45', 'female.age.lt.50', 'male.age.lt.50',
  'female.age.lt.55', 'male.age.lt.55', 'female.age.lt.60', 'male.age.lt.60', 'female.age.lt.65', 'male.age.lt.65',
  'female.age.lt.70', 'male.age.lt.70', 'female.age.lt.75', 'male.age.lt.75',
  'female.age.gt.40', 'male.age.gt.40', 'female.age.gt.45', 'male.age.gt.45', 'female.age.gt.50', 'male.age.gt.50',
  'female.age.gt.55', 'male.age.gt.55', 'female.age.gt.60', 'male.age.gt.60', 'female.age.gt.65', 'male.age.gt.65',
  'female.age.gt.70', 'male.age.gt.70', 'female.age.gt.75', 'male.age.gt.75',
  'female.ALL', 'male.ALL',
  'female.age.bt.55.70', 'male.age.bt.55.70', 'female.age.lt.80', 'male.age.lt.80', 'female.age.gt.80', 'male.age.gt.80'
)
trainSets <- list()
testSets <- list()
for (dat in 1:length(df)) {
  index <- caTools::sample.split(df[[dat]]$label, SplitRatio = .8)
  trainSets[[dat]] <- subset(df[[dat]], index == TRUE)
  testSets[[dat]] <- subset(df[[dat]], index == FALSE)
  rm(index,dat)
}
names(trainSets) <- names(df)
names(testSets) <- names(df)
ml.train <- list()
classifiers <- c("glm","rf","svmRadial","xgbDART")
paramGrid <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          summaryFunction = twoClassSummary,                      # Evaluate performance
                          classProbs = T,                                         # Estimate class probabilities
                          allowParallel = T,
                          search = "random")
for (dat in 1:length(trainSets)){
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets[dat]),"at",Sys.time()))
    ml.train[[paste(names(trainSets[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
                                                                                    data = trainSets[[dat]],
                                                                                    method = classifiers[[c]],
                                                                                    preProcess = c("center","scale"),
                                                                                    metric = "ROC",
                                                                                    trControl = paramGrid,
                                                                                    tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets[dat]),"at",Sys.time()))
  }
}
