#-----
# Load & Read data
#-----
library(readxl)
library(dplyr)
library(lubridate)
library(mice)
library(openxlsx)
library(stringr)
library(ggplot2)
library(cowplot)
library(caret)
library(ROCR)
library(pROC)
load("~/Clare-Clustering/corrected_version_10112021/dowloaded_RData")
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
ICD_ALL_compiled <- read_excel("~/Clare-Clustering/corrected_version_10112021/combined_ICD_list.xlsx")
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
                "ANXIETY_DIS_AT_INDEX",
                "BRAIN_TUMOR_AT_INDEX",
                "CHF_AT_INDEX",
                "CHRONIC_KIDNEY_DIS_AT_INDEX",
                "CHRONIC_LIVER_DIS_AT_INDEX",
                "CHRONIC_LUNG_DIS_AT_INDEX",
                "CIRRHOSIS_AT_INDEX",
                "CONVERSION_DIS_AT_INDEX",
                "CONVULSIONS_AT_INDEX",
                "DEPRESSION_AT_INDEX",
                "DIABETES_AT_INDEX",
                "DYSLIPIDEMIA_AT_INDEX",
                "EPILEPSY_AT_INDEX",
                "ESRD_AT_INDEX",
                "FAM_HEART_HIST",
                "FAM_STROKE_HIST",
                "HEPATIC_ENCEPHALOPATHY_AT_INDEX",
                "HYPERCOAG_STATES_AT_INDEX",
                "HYPERTENSION_AT_INDEX",
                "INDEX_DT",
                "MANIC_BIPOLAR_DIS_AT_INDEX",
                "MI_AT_INDEX",
                "MIGRAINE_AT_INDEX",
                "NEOPLASM_AT_INDEX",
                "PERI_VASC_DIS_AT_INDEX",
                "PERIPHERAL_NEUROPATHY_AT_INDEX",
                "PFO_ALL_TIME",
                "PT_SEX",
                "INDEX_DT",
                "SMOKE_STTS",
                "SYNCOPE_AT_INDEX",
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
              mutate(NEOPLASM_AT_INDEX_3monthsPost = if_else(
                                                            ICD %in% c(subset(ICD_ALL_compiled, CONDITION == 'NEOPLASM')$ICD_CODE)
                                                          & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(NEOPLASM_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","NEOPLASM_AT_INDEX_3monthsPost")%>%
              distinct(),
            by = 'PT_ID'
            ) %>%
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
      VASCULITIS_AT_INDEX_3monthsPost == 1 | FIBROMUSCULAR_DYSPLASIA_ALL_TIME == 1 |
        REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost == 1 | SICKLE_CELL_ALL_TIME == 1, 1, 0),
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
woImputation <- cbind(tmp.1, tmp.2)
rm(tmp.1, tmp.2)

#---------
# Summary Stats
#---------
tmp <- print(tableone::CreateTableOne(factorVars = colnames(woImputation %>% 
                                                              select(colnames(wImputation)) %>%
                                                              select(-c('AGE_AT_INDEX'))),
                                      data = woImputation %>%
                                                select(colnames(wImputation)) %>%
                                                select(-c('AGE_AT_INDEX')), 
                                      strata = "label"),
             noSpaces = TRUE,
             contDigits = 1,
             missing = TRUE, 
             exact = c('variable'))
write.xlsx(data.frame(tmp), 'summary_stats.xlsx', row.names = T)
#---------
# Features selection, imputation and train-test splits
#---------
features <- c(
  'AGE_AT_INDEX',
  'PT_SEX',
  'SMOKE_STTS',
  'BMI_CLOSEST_TO_INDEX_BINARY',
  'OTHER_ARTERIOPATHIES',
  'OTHER_CHRONIC_SYSTEMIC_DISORDERS',
  'FAM_HIST',
  'DRUG_DEP_ABUSE_AT_INDEX',
  'ANXIETY_DIS_AT_INDEX',
  'ALCOHOL_DEP_ABUSE_AT_INDEX',
  'BRAIN_TUMOR_AT_INDEX',
  'CHF_AT_INDEX',
  'CONVERSION_DIS_AT_INDEX',
  'CONVULSIONS_AT_INDEX',
  'DIABETES_AT_INDEX',
  'DEPRESSION_AT_INDEX',
  'DYSLIPIDEMIA_AT_INDEX',
  'EPILEPSY_AT_INDEX',
  'HYPERCOAG_STATES_AT_INDEX',
  'HYPERTENSION_AT_INDEX',
  'MANIC_BIPOLAR_DIS_AT_INDEX',
  'MI_AT_INDEX',
  'MIGRAINE_AT_INDEX',
  'PERI_VASC_DIS_AT_INDEX',
  'PERIPHERAL_NEUROPATHY_AT_INDEX',
  'PFO_ALL_TIME',
  'SYNCOPE_AT_INDEX',
  'NEOPLASM_AT_INDEX_3monthsPost',
  'HYPERCOAG_STATES_AT_INDEX_3monthsPost',
  'RHEUM_AT_INDEX_3monthsPost',
  'AFIB_FLUTTER_AT_INDEX_3monthsPost'
)
wImputation <- mice(woImputation[features],
                    m = 25, maxit = 25, method = 'pmm', seed = 99) 
wImputation <- cbind(complete(wImputation), woImputation['label'])
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
names(df) <- c('female.age.lt.40', 'male.age.lt.40', 'female.age.lt.45', 'male.age.lt.45', 'female.age.lt.50', 'male.age.lt.50',
               'female.age.lt.55', 'male.age.lt.55', 'female.age.lt.60', 'male.age.lt.60', 'female.age.lt.65', 'male.age.lt.65',
               'female.age.lt.70', 'male.age.lt.70', 'female.age.lt.75', 'male.age.lt.75',
               'female.age.gt.40', 'male.age.gt.40', 'female.age.gt.45', 'male.age.gt.45', 'female.age.gt.50', 'male.age.gt.50',
               'female.age.gt.55', 'male.age.gt.55', 'female.age.gt.60', 'male.age.gt.60', 'female.age.gt.65', 'male.age.gt.65',
               'female.age.gt.70', 'male.age.gt.70', 'female.age.gt.75', 'male.age.gt.75',
               'female.ALL', 'male.ALL'
)
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
# adding the new age breakouts to the train and test sets
for (dat in 35:40) {
  index <- caTools::sample.split(df[[dat]]$label, SplitRatio = .8)
  trainSets[[dat]] <- subset(df[[dat]], index == TRUE)
  testSets[[dat]] <- subset(df[[dat]], index == FALSE)
  rm(index,dat)
}
names(trainSets) <- names(df)
names(testSets) <- names(df)
#----------
# Model training
#----------
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

for (dat in 35:40){
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
#----------
# Model validation
#----------
fetchResults <- function(x,y){
  z <- as.data.frame(x)
  colnames(z) <- names(y)
  return(z)
}
incrementStart <- function(x){4*x-3}
incrementEnd <- function(x){4*x}
results <- as.data.frame(list())
for (dat in 1:length(testSets)){
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions <- setNames(
      data.frame(
        testSets[[dat]]$label,
        predict(object = ml.train[[c]], testSets[[dat]], type = "prob"),
        predict(object = ml.train[[c]], testSets[[dat]], type = "raw")
      ),
      c("obs","X0","X1","pred")
    )
    predictions$obs <- factor(predictions$obs)
    cm <- confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp <- as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,
          levels = rev(levels(predictions$obs)))$auc,                                           # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                             # Calculate AUPR
    )))
    results <- rbind(results,
                        tmp%>%
                          mutate(
                            "Classifier" = names(ml.train[c]),
                            "95%CI"= paste0("(",round(AccuracyLower,3),",",round(AccuracyUpper,3),")")
                          )%>%
                          select(
                            c("Classifier",AUROC="23",AUPR="24","Accuracy","95%CI",NIR="AccuracyNull",
                              "Kappa","Sensitivity","Specificity",
                              "Precision","F1","Prevalence",TP="4",FP="2",FN="3",TN="1")
                          )
    )
    #rm(tmp,cm,predictions)
  }
}
#
#----------
# Extract feature importance
#----------
feaImp.wide <- data.frame(features = character(0))                                   # create empty data frame with a column to append data iterative
for (i in 1:length(ml.train)) {
  ifelse(
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           # Check for 'pattern'(glm|rf|xgb) 
          names(ml.train[i])),                                                  # in the 'string'(female.age.lt.40.glm)
    # if TRUE then do
    feaImp.wide <- full_join(feaImp.wide, 
                        varImp(ml.train[[i]])$importance%>%              
                          mutate(features = rownames(.),
                                 model = names(ml.train[i]))%>%
                          select(features, model, Overall)%>%
                          arrange(model, features)%>%
                          rename(!!quo_name(names(ml.train[i])) := Overall),    # dynamically rename the default 'Overall' column
                        by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  )
}
feaImp.long <- data.frame(t(feaImp.wide))
colnames(feaImp.long) <- feaImp.long[1,]
feaImp.long <- feaImp.long[-1,]
feaImp.long <- data.frame(sapply(feaImp.long, function(x) as.numeric(as.character(x))),
                          row.names = rownames(feaImp.long))
# could break here for wide format
feaImp.long$conditions <- rownames(feaImp.long)
rownames(feaImp.long) <- NULL
feaImp.long <- cbind(
  stringr::str_split_fixed(feaImp.long$conditions, fixed('.'), 5),
  feaImp.long%>%
    select(-c(conditions))
)
feaImp.long <- feaImp.long[,-2]
names(feaImp.long)[1:4] <- c('gender', 'condition', 'age', 'model')
openxlsx::write.xlsx(feaImp.long, "feaImp.xlsx")
feaImp.long <- readxl::read_excel("feaImp.xlsx",na=c("NA"),col_names = T)
feaImp.long <- feaImp.long %>%
  select(-c(model))%>%
  group_by(gender, condition, age)%>%
  summarize_all(., funs(mean=mean))
row_names <- paste(feaImp.long$gender, 
                   feaImp.long$condition, 
                   feaImp.long$age, 
                   sep = ".")
feaImp.long <- feaImp.long[-c(1:3)]
rownames(feaImp.long) <- row_names
colnames(feaImp.long) <- sub("_mean", "", colnames(feaImp.long))
openxlsx::write.xlsx(feaImp.long, "feaImp.long.xlsx", row.names=T)
feaimp.mean <- readxl::read_excel('latest_figuresData.xlsx', 
                                  sheet = "feaimp", col_names = T)
feaimp.mean[,6:35] <- lapply(feaimp.mean[,6:35],
                             function(x) as.numeric(as.character(x)))
feaimp.mean <- feaimp.mean%>%
  select(-c(MODEL))%>%
  group_by(SEX, CONDITION, AGE, CRITERIA)%>%
  summarise(across(everything(), list(mean = mean)))
colnames(feaimp.mean) <- sub("_mean", "", colnames(feaimp.mean))
openxlsx::write.xlsx(melt(feaimp.mean), 'feaimp.mean.xlsx')
#------
# cosine similarity
#--------
cosine.similarity <- function(x, y){
  (x %*% y) / sqrt(x%*%x * y%*%y)
}
tmp <- data.frame(t(feaImp.long))
cosine.lt.vs.ALL <- data.frame(
  f.40 = c(cosine.similarity(tmp$female.lt.40, tmp$female.ALL.ALL)),
  f.45 = c(cosine.similarity(tmp$female.lt.45, tmp$female.ALL.ALL)),
  f.50 = c(cosine.similarity(tmp$female.lt.50, tmp$female.ALL.ALL)),
  f.55 = c(cosine.similarity(tmp$female.lt.55, tmp$female.ALL.ALL)),
  f.60 = c(cosine.similarity(tmp$female.lt.60, tmp$female.ALL.ALL)),
  f.65 = c(cosine.similarity(tmp$female.lt.65, tmp$female.ALL.ALL)),
  f.70 = c(cosine.similarity(tmp$female.lt.70, tmp$female.ALL.ALL)),
  f.75 = c(cosine.similarity(tmp$female.lt.75, tmp$female.ALL.ALL)),
  f.80 = c(cosine.similarity(tmp$female.lt.80, tmp$female.ALL.ALL)),
  f.bt.55.70 = c(cosine.similarity(tmp$female.bt.55, tmp$female.ALL.ALL)),
  m.40 = c(cosine.similarity(tmp$male.lt.40, tmp$male.ALL.ALL)),
  m.45 = c(cosine.similarity(tmp$male.lt.45, tmp$male.ALL.ALL)),
  m.50 = c(cosine.similarity(tmp$male.lt.50, tmp$male.ALL.ALL)),
  m.55 = c(cosine.similarity(tmp$male.lt.55, tmp$male.ALL.ALL)),
  m.60 = c(cosine.similarity(tmp$male.lt.60, tmp$male.ALL.ALL)),
  m.65 = c(cosine.similarity(tmp$male.lt.65, tmp$male.ALL.ALL)),
  m.70 = c(cosine.similarity(tmp$male.lt.70, tmp$male.ALL.ALL)),
  m.75 = c(cosine.similarity(tmp$male.lt.75, tmp$male.ALL.ALL)),
  m.80 = c(cosine.similarity(tmp$male.lt.80, tmp$male.ALL.ALL)),
  m.bt.55.70 = c(cosine.similarity(tmp$male.bt.55, tmp$male.ALL.ALL))
)
row.names(cosine.lt.vs.ALL) <- "cosine.lt.vs.ALL"
cosine.lt.vs.gt <- data.frame(
  f.40 = c(cosine.similarity(tmp$female.lt.40, tmp$female.gt.40)),
  f.45 = c(cosine.similarity(tmp$female.lt.45, tmp$female.gt.45)),
  f.50 = c(cosine.similarity(tmp$female.lt.50, tmp$female.gt.50)),
  f.55 = c(cosine.similarity(tmp$female.lt.55, tmp$female.gt.55)),
  f.60 = c(cosine.similarity(tmp$female.lt.60, tmp$female.gt.60)),
  f.65 = c(cosine.similarity(tmp$female.lt.65, tmp$female.gt.65)),
  f.70 = c(cosine.similarity(tmp$female.lt.70, tmp$female.gt.70)),
  f.75 = c(cosine.similarity(tmp$female.lt.75, tmp$female.gt.75)),
  f.80 = c(cosine.similarity(tmp$female.lt.80, tmp$female.gt.80)),
  f.bt.55.70 = c(cosine.similarity(tmp$female.bt.55, tmp$female.ALL.ALL)),
  m.40 = c(cosine.similarity(tmp$male.lt.40, tmp$male.gt.40)),
  m.45 = c(cosine.similarity(tmp$male.lt.45, tmp$male.gt.45)),
  m.50 = c(cosine.similarity(tmp$male.lt.50, tmp$male.gt.50)),
  m.55 = c(cosine.similarity(tmp$male.lt.55, tmp$male.gt.55)),
  m.60 = c(cosine.similarity(tmp$male.lt.60, tmp$male.gt.60)),
  m.65 = c(cosine.similarity(tmp$male.lt.65, tmp$male.gt.65)),
  m.70 = c(cosine.similarity(tmp$male.lt.70, tmp$male.gt.70)),
  m.75 = c(cosine.similarity(tmp$male.lt.75, tmp$male.gt.75)),
  m.80 = c(cosine.similarity(tmp$male.lt.80, tmp$male.gt.80)),
  m.bt.55.70 = c(cosine.similarity(tmp$male.bt.55, tmp$male.ALL.ALL))
)
row.names(cosine.lt.vs.gt) <- "cosine.lt.vs.gt"
cosine.gt.vs.ALL <- data.frame(
  f.40 = c(cosine.similarity(tmp$female.gt.40, tmp$female.ALL.ALL)),
  f.45 = c(cosine.similarity(tmp$female.gt.45, tmp$female.ALL.ALL)),
  f.50 = c(cosine.similarity(tmp$female.gt.50, tmp$female.ALL.ALL)),
  f.55 = c(cosine.similarity(tmp$female.gt.55, tmp$female.ALL.ALL)),
  f.60 = c(cosine.similarity(tmp$female.gt.60, tmp$female.ALL.ALL)),
  f.65 = c(cosine.similarity(tmp$female.gt.65, tmp$female.ALL.ALL)),
  f.70 = c(cosine.similarity(tmp$female.gt.70, tmp$female.ALL.ALL)),
  f.75 = c(cosine.similarity(tmp$female.gt.75, tmp$female.ALL.ALL)),
  f.80 = c(cosine.similarity(tmp$female.gt.80, tmp$female.ALL.ALL)),
  f.bt.55.70 = c(cosine.similarity(tmp$female.bt.55, tmp$female.ALL.ALL)),
  m.40 = c(cosine.similarity(tmp$male.gt.40, tmp$male.ALL.ALL)),
  m.45 = c(cosine.similarity(tmp$male.gt.45, tmp$male.ALL.ALL)),
  m.50 = c(cosine.similarity(tmp$male.gt.50, tmp$male.ALL.ALL)),
  m.55 = c(cosine.similarity(tmp$male.gt.55, tmp$male.ALL.ALL)),
  m.60 = c(cosine.similarity(tmp$male.gt.60, tmp$male.ALL.ALL)),
  m.65 = c(cosine.similarity(tmp$male.gt.65, tmp$male.ALL.ALL)),
  m.70 = c(cosine.similarity(tmp$male.gt.70, tmp$male.ALL.ALL)),
  m.75 = c(cosine.similarity(tmp$male.gt.75, tmp$male.ALL.ALL)),
  m.80 = c(cosine.similarity(tmp$male.gt.80, tmp$male.ALL.ALL)),
  m.bt.55.70 = c(cosine.similarity(tmp$male.bt.55, tmp$male.ALL.ALL))
)
row.names(cosine.gt.vs.ALL) <- "cosine.gt.vs.ALL"
cosine <- rbind(cosine.lt.vs.ALL, cosine.lt.vs.gt, cosine.gt.vs.ALL)
openxlsx::write.xlsx(cosine, "cosine.similarity.xlsx", row.names = TRUE)
#------
#--get model parameters
#-------
results.params <- as.data.frame(list())
for (model in 1:length(ml.train)) {
  tmp <- ml.train[[model]]
  best.Model <- which(tmp$results$ROC == max(tmp$results$ROC))
  tmp <- data.frame(
    model = names(ml.train[model]),
    best.Model = which(tmp$results$ROC == max(tmp$results$ROC)),
    AUROC = data.frame(tmp$results)[best.Model,]$ROC,
    Sensitivity = data.frame(tmp$results)[best.Model,]$Sens,
    Specificity = data.frame(tmp$results)[best.Model,]$Spec,
    data.frame(tmp$results)[best.Model,]%>%
      select(-c('ROC', 'Sens', 'Spec', 'ROCSD', 'SensSD', 'SpecSD'))
  )
  results.params <- bind_rows(tmp,
                                 results.params)
  rm(tmp, best.Model)
}
results.params <- results.params %>% select(-c('parameter', 'best.Model'))
results.params[is.na(results.params)] <- ""
rownames(results.params) <- NULL
openxlsx::write.xlsx(results.params, "results.params.xlsx", 
                     row.names=FALSE)
#----
# Table 1A and 1B
#----
library(tableone)
ptests <- list()
arr <- array(c(seq(16),33:38))
i <- 0
for (dat in 1:length(trainSets)) {
  #i <- i+1
  tmp <- rbind(trainSets[[dat]] %>% select(-c('AGE_AT_INDEX')),
               testSets[[dat]] %>% select(-c('AGE_AT_INDEX'))
               )
  ptests[[dat]] <- print(CreateTableOne(factorVars = colnames(tmp),
                                        data = tmp, 
                                        strata = "label"),
                         noSpaces = TRUE,
                         contDigits = 1,
                         missing = TRUE, 
                         exact = c('variable')) 
  rm(tmp)
}
names(ptests) <- names(trainSets)
tmp <- do.call(rbind.data.frame, ptests) # convert list to dataframe object
openxlsx::write.xlsx(tmp, 'ptests.xlsx', row.names = T)
