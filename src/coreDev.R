#############
# Read data
############
library(readxl)
library(dplyr)
library(lubridate)
library(reshape2)
library(caret)
library(ROCR)
library(pROC)
save(list=ls(),file="backup_12102020.rda")
CASES<-read_excel("GNSIS_DATABASE_v7.5.1_7.15.2020.xlsx",na=c("NA"),col_names = T)%>%
  filter(PAST_HEMORRHAGIC_STROKE_AT_INDEX==0 & PAST_ISCHEMIC_STROKE_AT_INDEX==0 & AGE_AT_INDEX>=18)
# ex for validation: PT1056376 has 8 indexes
CONTROLS<-list()
for (dat in 1:3) {
  CONTROLS[[dat]]<-readxl::read_excel(paste("CONTROLS",dat,"_DATABASE_v1.4_8.01.2020.xlsx",sep = ""),
                       na=c("NA"),col_names = T)%>%
    filter(PAST_HEMORRHAGIC_STROKE_PREINDEX==0 & PAST_ISCHEMIC_STROKE_PREINDEX==0 & AGE_AT_INDEX>=18)%>%
    group_by(PT_ID)%>%
    mutate(rank = rank(INDEX_DT))%>%
    arrange(PT_ID,INDEX_DT)%>%
    filter(rank == 1)
}
names(CONTROLS)<-paste0("CONTROLS.",seq_along(CONTROLS))
table(CASES$PT_SEX,CASES$AGE_AT_INDEX<=40)
#################################
# Split train and test data sets
################################
CONTROLS<-CONTROLS[[1]]
features.1 <- c("PT_ID",
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
tmp1 <- rbind(CASES%>%
          select(features.1),
        CONTROLS%>%
          rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)})%>%
          select(features.1))%>%
  mutate(DRUG_DEP_ABUSE_AT_INDEX = if_else((OPIOID_DEP_ABUSE_AT_INDEX == 1 | CANNABIS_DEP_ABUSE_AT_INDEX == 1 | 
                                              COCAINE_DEP_ABUSE_AT_INDEX == 1 | OTHER_DRUG_DEP_ABUSE_AT_INDEX==1),1,0),
         PT_SEX = if_else(PT_SEX %in% 'Male',1,0),
         SMOKE_STTS = if_else(SMOKE_STTS %in% 'CURRENT SMOKER',1,0))%>%
         left_join(., all_time(FIBROMUSCULAR_DYSPLASIA_ALL_TIME, icd_FIBROMUSCULAR), by = "PT_ID")%>%
         left_join(., at_index(CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX, icd_CERVICOCEPHALIC), by = "PT_ID")%>%
         left_join(., ICD_Patients_ENCS%>%
                       mutate(HIV_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_HIV & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                       filter(HIV_AT_INDEX_3monthsPost==1)%>%
                       select("PT_ID","HIV_AT_INDEX_3monthsPost")%>%
                       distinct(), 
                     by = "PT_ID")%>%
          left_join(., all_time(MOYAMOYA_ALL_TIME, icd_MOYAMOYA), by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost = if_else(
                ICD_CD %in% icd_CEREBRAL_VASOCONSTRICTION & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
                       mutate(RHEUM_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_RHEUM & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                       filter(RHEUM_AT_INDEX_3monthsPost==1)%>%
                       select("PT_ID","RHEUM_AT_INDEX_3monthsPost")%>%
                       distinct(), 
                     by = "PT_ID")%>%
          left_join(., all_time(SICKLE_CELL_ALL_TIME, icd_SICKLE_CELL), by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost = if_else(
                ICD_CD %in% icd_TEMPORAL_ARTERITIS & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., at_index(VARICELLA_ZOSTER_VIRUS_AT_INDEX, icd_VARICELLA_ZOSTER_VIRUS), by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(AFIB_FLUTTER_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_AFIB_FLUTTER & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(AFIB_FLUTTER_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","AFIB_FLUTTER_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(CIRRHOSIS_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_CIRRHOSIS & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(CIRRHOSIS_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","CIRRHOSIS_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(HYPERCOAG_STATES_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_HYPERCOAG & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(HYPERCOAG_STATES_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","HYPERCOAG_STATES_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(NEOPLASM_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_NEOPLASM & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(NEOPLASM_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","NEOPLASM_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
              mutate(PERI_VASC_DIS_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_PERIVASC & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
              filter(PERI_VASC_DIS_AT_INDEX_3monthsPost==1)%>%
              select("PT_ID","PERI_VASC_DIS_AT_INDEX_3monthsPost")%>%
              distinct(), 
            by = "PT_ID")%>%
          left_join(., ICD_Patients_ENCS%>%
                       mutate(VASCULITIS_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_VASCULITIS & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                       filter(VASCULITIS_AT_INDEX_3monthsPost==1)%>%
                       select("PT_ID","VASCULITIS_AT_INDEX_3monthsPost")%>%
                       distinct(), 
                     by = "PT_ID")%>%
  replace(is.na(.),0)

tmp4 <- rbind(CASES%>%
                select(features.2),
              CONTROLS%>%
                rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)})%>%
                select(features.2))%>%
  mutate(   BMI_CLOSEST_TO_INDEX_BINARY=if_else(BMI_CLOSEST_TO_INDEX>25,1,0),
            LDL_CLOSEST_TO_INDEX_BINARY=if_else(LDL_CLOSEST_TO_INDEX>100,1,0),
            HBA1C_CLOSEST_TO_INDEX_BINARY=if_else(HBA1C_CLOSEST_TO_INDEX>6.5,1,0),
            HDL_CLOSEST_TO_INDEX_BINARY=if_else(HDL_CLOSEST_TO_INDEX>40,1,0),
            PLT_CLOSEST_TO_INDEX_BINARY=if_else(PLT_CLOSEST_TO_INDEX>450,1,0),
            HB_CLOSEST_TO_INDEX_BINARY=if_else(
              ((PT_SEX==1 & HB_CLOSEST_TO_INDEX>=13.5 & HB_CLOSEST_TO_INDEX<=17.5)|
                 (PT_SEX==0 & HB_CLOSEST_TO_INDEX>=12 & HB_CLOSEST_TO_INDEX<=15.5)),0,1),
            CREATININE_CLOSEST_TO_INDEX_BINARY=if_else(
              ((PT_SEX==1 & CREATININE_CLOSEST_TO_INDEX<=1.4)|
                 (PT_SEX==0 & CREATININE_CLOSEST_TO_INDEX<=1.2)),0,1)
  )%>%
  select(c(BMI_CLOSEST_TO_INDEX_BINARY,
           LDL_CLOSEST_TO_INDEX_BINARY,
           HBA1C_CLOSEST_TO_INDEX_BINARY,
           HDL_CLOSEST_TO_INDEX_BINARY,
           PLT_CLOSEST_TO_INDEX_BINARY,
           HB_CLOSEST_TO_INDEX_BINARY,
           CREATININE_CLOSEST_TO_INDEX_BINARY))
woImp <- na.omit(cbind(tmp1, tmp4))
###################################################
# replacing missing values with Jiang's imputation
###################################################
tmp2 <- left_join(rbind(CASES%>%
                         select(features.2),
                       CONTROLS%>%
                         rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)})%>%
                         select(features.2)),
          jiang.labs,
          by = "PT_ID")%>%
  mutate(                                                                       
    HB_CLOSEST_TO_INDEX=coalesce(HB_CLOSEST_TO_INDEX.x,HB_CLOSEST_TO_INDEX.y),
    HBA1C_CLOSEST_TO_INDEX=coalesce(HBA1C_CLOSEST_TO_INDEX.x,HBA1C_CLOSEST_TO_INDEX.y),
    HDL_CLOSEST_TO_INDEX=coalesce(HDL_CLOSEST_TO_INDEX.x,HDL_CLOSEST_TO_INDEX.y),
    LDL_CLOSEST_TO_INDEX=coalesce(LDL_CLOSEST_TO_INDEX.x,LDL_CLOSEST_TO_INDEX.y),
    PLT_CLOSEST_TO_INDEX=coalesce(PLT_CLOSEST_TO_INDEX.x,PLT_CLOSEST_TO_INDEX.y),
    CREATININE_CLOSEST_TO_INDEX=coalesce(CREATININE_CLOSEST_TO_INDEX.x,CREATININE_CLOSEST_TO_INDEX.y)
  )%>%
  select(c(BMI_CLOSEST_TO_INDEX,
           LDL_CLOSEST_TO_INDEX,
           HBA1C_CLOSEST_TO_INDEX,
           HDL_CLOSEST_TO_INDEX,
           PLT_CLOSEST_TO_INDEX,
           HB_CLOSEST_TO_INDEX,
           CREATININE_CLOSEST_TO_INDEX))

  mutate(   BMI_CLOSEST_TO_INDEX_BINARY=if_else(BMI_CLOSEST_TO_INDEX>25,1,0),
            LDL_CLOSEST_TO_INDEX_BINARY=if_else(LDL_CLOSEST_TO_INDEX>100,1,0),
            HBA1C_CLOSEST_TO_INDEX_BINARY=if_else(HBA1C_CLOSEST_TO_INDEX>6.5,1,0),
            HDL_CLOSEST_TO_INDEX_BINARY=if_else(HDL_CLOSEST_TO_INDEX>40,1,0),
            PLT_CLOSEST_TO_INDEX_BINARY=if_else(PLT_CLOSEST_TO_INDEX>450,1,0),
            HB_CLOSEST_TO_INDEX_BINARY=if_else(
              ((PT_SEX==1 & HB_CLOSEST_TO_INDEX>=13.5 & HB_CLOSEST_TO_INDEX<=17.5)|
                 (PT_SEX==0 & HB_CLOSEST_TO_INDEX>=12 & HB_CLOSEST_TO_INDEX<=15.5)),0,1),
            CREATININE_CLOSEST_TO_INDEX_BINARY=if_else(
              ((PT_SEX==1 & CREATININE_CLOSEST_TO_INDEX<=1.4)|
                 (PT_SEX==0 & CREATININE_CLOSEST_TO_INDEX<=1.2)),0,1)
  )%>%
  select(c(BMI_CLOSEST_TO_INDEX_BINARY,
           LDL_CLOSEST_TO_INDEX_BINARY,
           HBA1C_CLOSEST_TO_INDEX_BINARY,
           HDL_CLOSEST_TO_INDEX_BINARY,
           PLT_CLOSEST_TO_INDEX_BINARY,
           HB_CLOSEST_TO_INDEX_BINARY,
           CREATININE_CLOSEST_TO_INDEX_BINARY))
  
######
# Train-test split
######
features <- c(
  "AFIB_FLUTTER_AT_INDEX_3monthsPost",
  "ALCOHOL_DEP_ABUSE_AT_INDEX",
  "ANXIETY_DIS_AT_INDEX",
  "BRAIN_TUMOR_AT_INDEX",
  "CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX",
  "CHF_AT_INDEX",
  "CHRONIC_KIDNEY_DIS_AT_INDEX",
  "CHRONIC_LIVER_DIS_AT_INDEX",
  "CHRONIC_LUNG_DIS_AT_INDEX",
  "CIRRHOSIS_AT_INDEX_3monthsPost",
  "CONVERSION_DIS_AT_INDEX",
  "CONVULSIONS_AT_INDEX",
  "DEPRESSION_AT_INDEX",
  "DIABETES_AT_INDEX",
  "DRUG_DEP_ABUSE_AT_INDEX",
  "DYSLIPIDEMIA_AT_INDEX",
  "EPILEPSY_AT_INDEX",
  "ESRD_AT_INDEX",
  "FAM_HEART_HIST",
  "FAM_STROKE_HIST",
  "FIBROMUSCULAR_DYSPLASIA_ALL_TIME",
  "HEPATIC_ENCEPHALOPATHY_AT_INDEX",
  "HIV_AT_INDEX_3monthsPost",
  "HYPERCOAG_STATES_AT_INDEX_3monthsPost",
  "HYPERTENSION_AT_INDEX",
  "MANIC_BIPOLAR_DIS_AT_INDEX",
  "MI_AT_INDEX",
  "MIGRAINE_AT_INDEX",
  "MOYAMOYA_ALL_TIME",
  "NEOPLASM_AT_INDEX_3monthsPost",
  "PERI_VASC_DIS_AT_INDEX_3monthsPost",
  "PERIPHERAL_NEUROPATHY_AT_INDEX",
  "PFO_ALL_TIME",
  #"PT_SEX",
  "REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost",
  "RHEUM_AT_INDEX_3monthsPost",
  "SICKLE_CELL_ALL_TIME",
  "SMOKE_STTS",
  "SYNCOPE_AT_INDEX",
  "TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost",
  "VARICELLA_ZOSTER_VIRUS_AT_INDEX",
  "VASCULITIS_AT_INDEX_3monthsPost",
  "BMI_CLOSEST_TO_INDEX_BINARY",
  "LDL_CLOSEST_TO_INDEX_BINARY",
  "HBA1C_CLOSEST_TO_INDEX_BINARY",
  "HDL_CLOSEST_TO_INDEX_BINARY",
  "PLT_CLOSEST_TO_INDEX_BINARY",
  "HB_CLOSEST_TO_INDEX_BINARY",
  "CREATININE_CLOSEST_TO_INDEX_BINARY"
)
age.buckets<-function(AGE){
  rbind(
    subset(woImp, AGE_AT_INDEX<=AGE)%>%
      mutate(label=1),
    subset(woImp, AGE_AT_INDEX>AGE)%>%
      mutate(label=0)
  )%>%
    mutate(label=factor(label,                                                  # Rename the label classes to valid R variable names
                        labels = make.names(levels(as.factor(as.character(label))))))%>%
    select(features,label)}
df <- list()
i<- 0
for (age in seq(40, 65, by = 5)) {
  i <- i+1
  df[[i]]<-age.buckets(age)
}
names(df) <- paste0("age.",seq(40, 65, by = 5))
trainSets <- list()
testSets <- list()
for (dat in 1:length(df)) {
  index <- caTools::sample.split(df[[dat]]$label, SplitRatio = .8)
  trainSets[[dat]] <- subset(df[[dat]], index==TRUE)
  testSets[[dat]] <- subset(df[[dat]], index==FALSE)
  rm(index,dat)
}
names(trainSets) <- paste0("age.",seq(40, 65, by = 5))
names(testSets) <- paste0("age.",seq(40, 65, by = 5))
####################
# Model development
###################
ml.train <- list()
classifiers <- c("glm","rf")
paramGrid<-trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        summaryFunction = twoClassSummary,                      # Evaluate performance
                        classProbs = T,                                         # Estimate class probabilities
                        allowParallel = T,
                        search = "random")
for (dat in 1:length(trainSets)){
  for (c in 1:length(classifiers)) {
    ml.train[[paste(names(trainSets[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
                                                                                data = trainSets[[dat]],
                                                                                method = classifiers[[c]],
                                                                                preProcess = c("center","scale"),
                                                                                metric = "ROC",
                                                                                trControl = paramGrid,
                                                                                tuneLength = 5
                                                                                )
  }
  
}
###################
# Model validation
##################
library(pROC)
library(e1071)
fetchResults<-function(x,y){
  z<-as.data.frame(x)
  colnames(z)<-names(y)
  return(z)
}
predictions<-setNames(
  data.frame(
    testSets[[1]]$label,
    predict(object = ml.train[[1]], testSets[[1]], type = "prob"),
    predict(object = ml.train[[1]], testSets[[1]], type = "raw")
  ),
  c("obs","X0","X1","pred")
)
confusionMatrix(
  reference = predictions$obs,
  data = predictions$pred,
  mode = "everything",
  positive = "X1"
)
prSummary(predictions, lev = rev(levels(predictions$obs)))
pROC::roc(
  predictor = predictions$X1,
  response = predictions$obs,
  levels = rev(levels(predictions$obs))
)
incrementStart<-function(x){2*x-1}
incrementEnd<-function(x){2*x}
results.5fold<-as.data.frame(list())
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
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.5fold<-rbind(results.5fold,
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
    rm(tmp,cm,predictions)
  }
}
feaImp <- read_excel("ModelResults.xlsx",na=c("NA"),col_names = T,sheet = "FeaImp")
tiff("RandomForest.tiff", units="in", width=20, height=8, res=300)
melt(feaImp)%>%
  filter(!grepl('GLM',variable))%>%
  ggplot(aes(x=Features, y=value))+
  geom_point(aes(shape = factor(variable)), size=2)+
  coord_flip()+
  scale_y_reverse()+
  facet_grid(.~variable)+
  labs(shape="Model.Age", y="Feature Importance(scaled) score")+
  theme(
    axis.text = element_text(color="black", size=8),
    axis.title = element_text(color="black"),
    strip.text = element_text(color="black", face="bold", size=12)
  )
dev.off()

tiff("RandomForest2.tiff", units="in", width=20, height=12, res=300)
melt(feaImp)%>%
  filter(!grepl('GLM',variable))%>%
  ggplot(aes(x=variable, y=value))+
  geom_point(aes(shape=factor(variable)), size=2)+
  coord_flip()+
  facet_wrap(Features~.)+
  labs(shape="Model.Age")+
  theme(
    axis.text = element_text(color="black", size=8),
    axis.title = element_blank(),
    strip.text = element_text(color="black", face="bold", size=8)
  )
dev.off()


################################################################################
# Revisit: definition and selection criteria changed 12/10/2020
################################################################################
# Read data
library(readxl)
library(dplyr)
library(lubridate)
library(reshape2)
library(caret)
CASES <- read_excel("GNSIS_DATABASE_v7.5.1_7.15.2020.xlsx",na=c("NA"),col_names = T)%>%
  filter(PAST_HEMORRHAGIC_STROKE_AT_INDEX==0 & PAST_ISCHEMIC_STROKE_AT_INDEX==0 & AGE_AT_INDEX>=18)%>%
  mutate(label = 'X1')
CONTROLS <- readxl::read_excel('CONTROLS1_DATABASE_v1.5_5.22.2021.xlsx',
                                    na=c("NA"),col_names = T)%>%
  filter(PAST_HEMORRHAGIC_STROKE_PREINDEX==0 & PAST_ISCHEMIC_STROKE_PREINDEX==0 & AGE_AT_INDEX>=18)%>%
  mutate(label = 'X0')
  group_by(PT_ID)%>%
  mutate(rank = rank(INDEX_DT))%>%
  arrange(PT_ID,INDEX_DT)%>%
  filter(rank == 1)
df.features.1 <- rbind(CASES%>%
                  select(label, features.1),
                CONTROLS%>%
                  rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)})%>%
                  select(label, features.1))%>%
    mutate(DRUG_DEP_ABUSE_AT_INDEX = if_else((OPIOID_DEP_ABUSE_AT_INDEX == 1 | CANNABIS_DEP_ABUSE_AT_INDEX == 1 | 
                                                COCAINE_DEP_ABUSE_AT_INDEX == 1 | OTHER_DRUG_DEP_ABUSE_AT_INDEX==1),1,0),
           PT_SEX = if_else(PT_SEX %in% 'Male',1,0),
           SMOKE_STTS = if_else(SMOKE_STTS %in% 'CURRENT SMOKER',1,0))%>%
    left_join(., all_time(FIBROMUSCULAR_DYSPLASIA_ALL_TIME, icd_FIBROMUSCULAR), by = "PT_ID")%>%
    left_join(., at_index(CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX, icd_CERVICOCEPHALIC), by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(HIV_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_HIV & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(HIV_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","HIV_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., all_time(MOYAMOYA_ALL_TIME, icd_MOYAMOYA), by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost = if_else(
                  ICD_CD %in% icd_CEREBRAL_VASOCONSTRICTION & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(RHEUM_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_RHEUM & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(RHEUM_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","RHEUM_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., all_time(SICKLE_CELL_ALL_TIME, icd_SICKLE_CELL), by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost = if_else(
                  ICD_CD %in% icd_TEMPORAL_ARTERITIS & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., at_index(VARICELLA_ZOSTER_VIRUS_AT_INDEX, icd_VARICELLA_ZOSTER_VIRUS), by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(AFIB_FLUTTER_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_AFIB_FLUTTER & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(AFIB_FLUTTER_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","AFIB_FLUTTER_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(CIRRHOSIS_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_CIRRHOSIS & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(CIRRHOSIS_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","CIRRHOSIS_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(HYPERCOAG_STATES_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_HYPERCOAG & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(HYPERCOAG_STATES_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","HYPERCOAG_STATES_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(NEOPLASM_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_NEOPLASM & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(NEOPLASM_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","NEOPLASM_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(PERI_VASC_DIS_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_PERIVASC & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(PERI_VASC_DIS_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","PERI_VASC_DIS_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    left_join(., ICD_Patients_ENCS%>%
                mutate(VASCULITIS_AT_INDEX_3monthsPost = if_else(ICD_CD %in% icd_VASCULITIS & ENC_DT < (INDEX_DT+months(3)),1,0))%>%
                filter(VASCULITIS_AT_INDEX_3monthsPost==1)%>%
                select("PT_ID","VASCULITIS_AT_INDEX_3monthsPost")%>%
                distinct(), 
              by = "PT_ID")%>%
    replace(is.na(.),0)
  
df.features.2 <- rbind(CASES%>%
                  select(label, features.2),
                CONTROLS%>%
                  rename_with(function(x){sub("_PREINDEX","_AT_INDEX",x)})%>%
                  select(label, features.2))%>%
    mutate(   BMI_CLOSEST_TO_INDEX_BINARY=if_else(BMI_CLOSEST_TO_INDEX>25,1,0),
              LDL_CLOSEST_TO_INDEX_BINARY=if_else(LDL_CLOSEST_TO_INDEX>100,1,0),
              HBA1C_CLOSEST_TO_INDEX_BINARY=if_else(HBA1C_CLOSEST_TO_INDEX>6.5,1,0),
              HDL_CLOSEST_TO_INDEX_BINARY=if_else(HDL_CLOSEST_TO_INDEX>40,1,0),
              PLT_CLOSEST_TO_INDEX_BINARY=if_else(PLT_CLOSEST_TO_INDEX>450,1,0),
              HB_CLOSEST_TO_INDEX_BINARY=if_else(
                ((PT_SEX==1 & HB_CLOSEST_TO_INDEX>=13.5 & HB_CLOSEST_TO_INDEX<=17.5)|
                   (PT_SEX==0 & HB_CLOSEST_TO_INDEX>=12 & HB_CLOSEST_TO_INDEX<=15.5)),0,1),
              CREATININE_CLOSEST_TO_INDEX_BINARY=if_else(
                ((PT_SEX==1 & CREATININE_CLOSEST_TO_INDEX<=1.4)|
                   (PT_SEX==0 & CREATININE_CLOSEST_TO_INDEX<=1.2)),0,1)
    )%>%
    select(c(BMI_CLOSEST_TO_INDEX_BINARY,
             LDL_CLOSEST_TO_INDEX_BINARY,
             HBA1C_CLOSEST_TO_INDEX_BINARY,
             HDL_CLOSEST_TO_INDEX_BINARY,
             PLT_CLOSEST_TO_INDEX_BINARY,
             HB_CLOSEST_TO_INDEX_BINARY,
             CREATININE_CLOSEST_TO_INDEX_BINARY))
df.woImp <- na.omit(cbind(df.features.1, df.features.2)) %>%
  select('PT_ID','label','AGE_AT_INDEX', 'INDEX_DT','PT_SEX', all_of(features))
rm(df.features.1, df.features.2)  
age.buckets<-function(AGE, GENDER){
    subset(df.woImp, AGE_AT_INDEX > AGE & PT_SEX == GENDER) %>%
    mutate(label = factor(label, levels = c('X0', 'X1')))%>%                             # Rename the label classes to valid R variable names
    select(label,all_of(features))
    }
df <- list()
i<- 0
for (age in seq(40, 75, by = 5)) {
  for (gender in seq(0, 1, by = 1)) {
    i <- i+1
    df[[i]] <- age.buckets(age, gender)
  }
}
df[[33]] <- subset(df.woImp, PT_SEX == 0)%>%
  mutate(label = factor(label, levels = c('X0', 'X1')))%>%                             # Rename the label classes to valid R variable names
  select(label,all_of(features))
df[[34]] <- subset(df.woImp, PT_SEX == 1)%>%
  mutate(label = factor(label, levels = c('X0', 'X1')))%>%                             # Rename the label classes to valid R variable names
  select(label,all_of(features))
names(df) <- c('female.age.lt.40', 'male.age.lt.40', 'female.age.lt.45', 'male.age.lt.45', 'female.age.lt.50', 'male.age.lt.50',
               'female.age.lt.55', 'male.age.lt.55', 'female.age.lt.60', 'male.age.lt.60', 'female.age.lt.65', 'male.age.lt.65',
               'female.age.lt.70', 'male.age.lt.70','female.age.lt.75', 'male.age.lt.75',
               'female.age.gt.40', 'male.age.gt.40', 'female.age.gt.45', 'male.age.gt.45', 'female.age.gt.50', 'male.age.gt.50',
               'female.age.gt.55', 'male.age.gt.55', 'female.age.gt.60', 'male.age.gt.60', 'female.age.gt.65', 'male.age.gt.65',
               'female.age.gt.70', 'male.age.gt.70','female.age.gt.75', 'male.age.gt.75',
               'female.ALL', 'male.ALL'
               )
trainSets <- list()
testSets <- list()
for (dat in 1:length(df)) {
  index <- caTools::sample.split(df[[dat]]$label, SplitRatio = .8)
  trainSets[[dat]] <- subset(df[[dat]], index==TRUE)
  testSets[[dat]] <- subset(df[[dat]], index==FALSE)
  rm(index,dat)
}
names(trainSets) <- names(df)
names(testSets) <- names(df)
# Model development
ml.train.v2 <- list()
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
    ml.train.v2[[paste(names(trainSets[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
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
# Model validation
incrementStart<-function(x){4*x-3}
incrementEnd<-function(x){4*x}
results.v2 <- as.data.frame(list())
for (dat in 1:length(testSets)){
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions <- setNames(
      data.frame(
        testSets[[dat]]$label,
        predict(object = ml.train.v2[[c]], testSets[[dat]], type = "prob"),
        predict(object = ml.train.v2[[c]], testSets[[dat]], type = "raw")
      ),
      c("obs","X0","X1","pred")
    )
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
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.v2 <- rbind(results.v2,
                         tmp%>%
                           mutate(
                             "Classifier" = names(ml.train.v2[c]),
                             "95%CI"= paste0("(",round(AccuracyLower,3),",",round(AccuracyUpper,3),")")
                           )%>%
                           select(
                             c("Classifier",AUROC="23",AUPR="24","Accuracy","95%CI",NIR="AccuracyNull",
                               "Kappa","Sensitivity","Specificity",
                               "Precision","F1","Prevalence",TP="4",FP="2",FN="3",TN="1")
                           )
    )
    rm(tmp,cm,predictions)
  }
}
# Summary stats
row_names_summary_stats <- rownames(tmp)
summary.stats <- data.frame(row.names = row_names_summary_stats)
for (i in 1:length(df)) {
  tmp <- t(df[[i]] %>%
             group_by(label)%>%
             group_by(N=n(), .add = TRUE)%>%
             summarise_all(funs(sum)))
  colnames(tmp) <- c(
    paste(names(df[i]),'X0', sep = "."),
    paste(names(df[i]),'X1', sep = ".")
  )
  summary.stats <- cbind(summary.stats, tmp)
  rm(tmp)
}
openxlsx::write.xlsx(summary.stats, "summary_stats.xlsx", row.names = TRUE)
# Extract feature importance
feaImp <- data.frame(features = character(0))                                   # create empty data frame with a column to append data iterative
for (i in 1:length(ml.train.v2)) {
  ifelse(
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           # Check for 'pattern'(glm|rf|xgb) 
          names(ml.train.v2[i])),                                               # in the 'string'(female.age.lt.40.glm)
    # if TRUE then do
    feaImp <- full_join(feaImp, 
                        varImp(ml.train.v2[[i]])$importance%>%              
                          mutate(features = rownames(.),
                                 model = names(ml.train.v2[i]))%>%
                          select(features, model, Overall)%>%
                          arrange(model, features)%>%
                          rename(!!quo_name(names(ml.train.v2[i])) := Overall), # dynamically rename the default 'Overall' column
                        by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  )
}
openxlsx::write.xlsx(feaImp, "feaImp.xlsx")
feaImp.long <- data.frame(t(feaImp))
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
feaImp.long <- feaImp.long%>%
  select(-c(model))%>%
  group_by(gender, condition, age)%>%
  summarize_all(., funs(mean=mean))
row_names <- paste(feaImp.long$gender, feaImp.long$condition, feaImp.long$age, sep = ".")
feaImp.long <- feaImp.long[-c(1:3)]
rownames(feaImp.long) <- row_names
colnames(feaImp.long) <- sub("_mean", "", colnames(feaImp.long))
# hclust
library(gplots)
hclust.matrix.female <- as.matrix(
  t(
    data.frame(
  feaImp.long[grepl('female', row.names(feaImp.long)),],
  row.names = rownames(feaImp.long)[grep('female', row.names(feaImp.long))]
              )
  )
  )
indx.males <- setdiff(grep('male', row.names(feaImp.long)), grep('female', row.names(feaImp.long)))
hclust.matrix.male <- as.matrix(
  t(
    data.frame(
      feaImp.long[indx.males,],
      row.names = rownames(feaImp.long)[indx.males]
    )
  )
)

dev.off()
heatmap.2(hclust.matrix.female,
          trace = "none", 
          main = "FEMALE",
          #density.info = "none",
          dendrogram = c('row'),
          key.title = "", 
          keysize = .75,
          key.par = list(mar=c(5,.5,1,.5)),
          margins = c(7,15),
          xlab = "",                          # Names the x-axis
          #labCol = "",                        # to show X-axis labels
          cexRow = .75,
          cexCol = 1,
          Colv = FALSE,
          col = rev(heat.colors(999))
)
dev.off()

##############################################
# Revisit: changed input variables 01/04/2021
#############################################
vars.v3 <- function(dat){
  dat%>%
    mutate(
    OTHER_ARTERIOPATHIES = if_else(
    VASCULITIS_AT_INDEX_3monthsPost == 1 | FIBROMUSCULAR_DYSPLASIA_ALL_TIME == 1 |
    REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost == 1 |
    MOYAMOYA_ALL_TIME == 1 | SICKLE_CELL_ALL_TIME == 1, 1, 0),
    OTHER_CHRONIC_SYSTEMIC_DISORDERS = if_else(
      CHRONIC_LIVER_DIS_AT_INDEX == 1 | CHRONIC_LUNG_DIS_AT_INDEX == 1 |
      CHRONIC_KIDNEY_DIS_AT_INDEX == 1 | CIRRHOSIS_AT_INDEX_3monthsPost == 1 |
      ESRD_AT_INDEX == 1, 1, 0),
    FAM_HIST = if_else(
      FAM_HEART_HIST == 1 | FAM_STROKE_HIST ==1, 1, 0),
    NEOPLASM_BRAIN_TUMOR = if_else(
      NEOPLASM_AT_INDEX_3monthsPost == 1 | BRAIN_TUMOR_AT_INDEX == 1, 1, 0)
  )%>%
  select(label,
         OTHER_ARTERIOPATHIES,
         OTHER_CHRONIC_SYSTEMIC_DISORDERS,
         HYPERCOAG_STATES_AT_INDEX_3monthsPost,
         AFIB_FLUTTER_AT_INDEX_3monthsPost,
         ALCOHOL_DEP_ABUSE_AT_INDEX,
         CHF_AT_INDEX,
         DIABETES_AT_INDEX,
         DRUG_DEP_ABUSE_AT_INDEX,
         DYSLIPIDEMIA_AT_INDEX,
         FAM_HIST,
         HYPERTENSION_AT_INDEX,
         MI_AT_INDEX,
         PERI_VASC_DIS_AT_INDEX_3monthsPost,
         PFO_ALL_TIME,
         SMOKE_STTS,
         BMI_CLOSEST_TO_INDEX_BINARY,
         CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX,
         NEOPLASM_BRAIN_TUMOR,
         RHEUM_AT_INDEX_3monthsPost,
         MIGRAINE_AT_INDEX,
         TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost
         )
}
trainSets.v3 <- list()
for (dat in 1:length(trainSets)) {
  trainSets.v3[[dat]] <- vars.v3(trainSets[[dat]])
}
names(trainSets.v3) <- names(trainSets)
testSets.v3 <- list()
for (dat in 1:length(testSets)) {
  testSets.v3[[dat]] <- vars.v3(testSets[[dat]])
}
names(testSets.v3) <- names(testSets)
# Model development
ml.train.v3 <- list()
classifiers <- c("glm","rf","svmRadial","xgbDART")
paramGrid <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          summaryFunction = twoClassSummary,                      # Evaluate performance
                          classProbs = T,                                         # Estimate class probabilities
                          allowParallel = T,
                          search = "random")
for (dat in 1:length(trainSets.v3)){
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.v3[dat]),"at",Sys.time()))
    ml.train.v3[[paste(names(trainSets.v3[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
                                                                                       data = trainSets.v3[[dat]],
                                                                                       method = classifiers[[c]],
                                                                                       preProcess = c("center","scale"),
                                                                                       metric = "ROC",
                                                                                       trControl = paramGrid,
                                                                                       tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets.v3[dat]),"at",Sys.time()))
  }
}
# Model validation
incrementStart<-function(x){4*x-3}
incrementEnd<-function(x){4*x}
results.v3 <- as.data.frame(list())
for (dat in 1:length(testSets.v3)){
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions <- setNames(
      data.frame(
        testSets.v3[[dat]]$label,
        predict(object = ml.train.v3[[c]], testSets.v3[[dat]], type = "prob"),
        predict(object = ml.train.v3[[c]], testSets.v3[[dat]], type = "raw")
      ),
      c("obs","X0","X1","pred")
    )
    cm <- confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp <- as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.v3[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.v3[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.v3[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.v3 <- rbind(results.v3,
                        tmp%>%
                          mutate(
                            "Classifier" = names(ml.train.v3[c]),
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
# Feature Importance
feaImp.v3 <- data.frame(features = character(0))                                   # create empty data frame with a column to append data iterative
for (i in 1:length(ml.train.v3)) {
  ifelse(
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           # Check for 'pattern'(glm|rf|xgb) 
          names(ml.train.v3[i])),                                               # in the 'string'(female.age.lt.40.glm)
    # if TRUE then do
    feaImp.v3 <- full_join(feaImp.v3, 
                        varImp(ml.train.v3[[i]])$importance%>%              
                          mutate(features = rownames(.),
                                 model = names(ml.train.v3[i]))%>%
                          select(features, model, Overall)%>%
                          arrange(model, features)%>%
                          rename(!!quo_name(names(ml.train.v3[i])) := Overall), # dynamically rename the default 'Overall' column
                        by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  )
}
openxlsx::write.xlsx(feaImp.v3, "feaImp.v3.xlsx")
feaImp.v3.long <- data.frame(t(feaImp.v3))
colnames(feaImp.v3.long) <- feaImp.v3.long[1,]
feaImp.v3.long <- feaImp.v3.long[-1,]
feaImp.v3.long <- data.frame(sapply(feaImp.v3.long, function(x) as.numeric(as.character(x))),
                          row.names = rownames(feaImp.v3.long))
feaImp.v3.long$conditions <- rownames(feaImp.v3.long)
rownames(feaImp.v3.long) <- NULL
feaImp.v3.long <- cbind(
  stringr::str_split_fixed(feaImp.v3.long$conditions, fixed('.'), 5),
  feaImp.v3.long%>%
    select(-c(conditions))
)
feaImp.v3.long <- feaImp.v3.long[,-2]
names(feaImp.v3.long)[1:4] <- c('gender', 'condition', 'age', 'model')
feaImp.v3.long <- feaImp.v3.long%>%
  select(-c(model))%>%
  group_by(gender, condition, age)%>%
  summarize_all(., funs(mean=mean))
row_names <- paste(feaImp.v3.long$gender, feaImp.v3.long$condition, feaImp.v3.long$age, sep = ".")
feaImp.v3.long <- feaImp.v3.long[-c(1:3)]
rownames(feaImp.v3.long) <- row_names
colnames(feaImp.v3.long) <- sub("_mean", "", colnames(feaImp.v3.long))
# hclust
library(gplots)
hclust.matrix.v3.female <- as.matrix(
  t(
    data.frame(
      feaImp.v3.long[grepl('female', row.names(feaImp.v3.long)),],
      row.names = rownames(feaImp.v3.long)[grep('female', row.names(feaImp.v3.long))]
    )
  )
)
indx.males <- setdiff(grep('male', row.names(feaImp.v3.long)), grep('female', row.names(feaImp.v3.long)))
hclust.matrix.male <- as.matrix(
  t(
    data.frame(
      feaImp.v3.long[indx.males,],
      row.names = rownames(feaImp.v3.long)[indx.males]
    )
  )
)

dev.off()
heatmap.2(
  #hclust.matrix.v3.female,
  as.matrix(t(feaImp.v3.long)),
          trace = "none", 
          #main = "FEMALE",
          #density.info = "none",
          dendrogram = c('row'),
          key.title = "", 
          key = "True",
          keysize = .75,
          key.par = list(mar=c(5,3,1,1)),
          key.xlab = 'Avg. Feature Importance Score',
          key.ylab = "Count",
          margins = c(7,20),
          xlab = "",                          # Names the x-axis
          #labCol = "",                        # to show X-axis labels
          cexRow = .75,
          cexCol = 1,
          Colv = FALSE,
          col = rev(heat.colors(999))
)
dev.off()
# version-1: excluding all 'greater than' groups from the heat map
indexes.of.lt.rownames <- grep('lt', row.names(feaImp.v3.long))
v1.heatmap.matrix <- data.frame(feaImp.v3.long[indexes.of.lt.rownames,],
                                row.names = rownames(feaImp.v3.long)[indexes.of.lt.rownames])
dev.off()
heatmap.2(
  #hclust.matrix.v3.female,
  #as.matrix(t(v1.heatmap.matrix)),
  as.matrix(t(log(v1.heatmap.matrix+1))),
  trace = "none", 
  main = "version-1",
  #density.info = "none",
  dendrogram = c('row'),
  key.title = "", 
  key = "True",
  keysize = .75,
  key.par = list(mar=c(5,3,1,1)),
  key.xlab = 'Avg. Feature Importance Score\n(log-transformed)',
  key.ylab = "Count",
  margins = c(7,20),
  xlab = "",                          # Names the x-axis
  #labCol = "",                        # to show X-axis labels
  cexRow = .75,
  cexCol = 1,
  Colv = FALSE,
  #col = rev(heat.colors(999))
  symbreaks = FALSE,
  col = colorRampPalette(c("red",'yellow','darkgreen'))
)
# version-2: exclude <40 age groups from the above heat map
indexes.wo.lt.40.rownames <- grep(paste(c('lt.40','gt'),collapse = "|"), row.names(feaImp.v3.long))
v2.heatmap.matrix <- data.frame(
  feaImp.v3.long[-indexes.wo.lt.40.rownames,],
  row.names = rownames(feaImp.v3.long)[-indexes.wo.lt.40.rownames]
)
heatmap.2(
  #hclust.matrix.v3.female,
  as.matrix(t(log(v2.heatmap.matrix+1))),
  trace = "none", 
  main = "version-2",
  #density.info = "none",
  dendrogram = c('row'),
  key.title = "", 
  key = "True",
  keysize = .75,
  key.par = list(mar=c(5,3,1,1)),
  key.xlab = 'Avg. Feature Importance Score\n(log-transformed)',
  key.ylab = "Count",
  margins = c(7,20),
  xlab = "",                          # Names the x-axis
  #labCol = "",                        # to show X-axis labels
  cexRow = .75,
  cexCol = 1,
  Colv = FALSE,
  #col = rev(heat.colors(999))
  symbreaks = FALSE,
  col = colorRampPalette(c("red",'yellow','darkgreen'))
)

#######################################################################################
# Revisit: changed input variables 01/21/2021: use separate vars for Males and Females
######################################################################################
df.woImp <- df.woImp%>%
  mutate(
    OTHER_ARTERIOPATHIES = if_else(
      VASCULITIS_AT_INDEX_3monthsPost == 1 | FIBROMUSCULAR_DYSPLASIA_ALL_TIME == 1 |
        REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost == 1 |
        MOYAMOYA_ALL_TIME == 1 | SICKLE_CELL_ALL_TIME == 1, 1, 0),
    OTHER_CHRONIC_SYSTEMIC_DISORDERS = if_else(
      CHRONIC_LIVER_DIS_AT_INDEX == 1 | CHRONIC_LUNG_DIS_AT_INDEX == 1 |
        CHRONIC_KIDNEY_DIS_AT_INDEX == 1 | CIRRHOSIS_AT_INDEX_3monthsPost == 1 |
        ESRD_AT_INDEX == 1, 1, 0),
    FAM_HIST = if_else(
      FAM_HEART_HIST == 1 | FAM_STROKE_HIST ==1, 1, 0),
    NEOPLASM_BRAIN_TUMOR = if_else(
      NEOPLASM_AT_INDEX_3monthsPost == 1 | BRAIN_TUMOR_AT_INDEX == 1, 1, 0)
  )
df <- list()
i<- 0
age.buckets<-function(AGE, GENDER){
  subset(df.woImp, AGE_AT_INDEX > AGE & PT_SEX == GENDER) %>%                   # Tweak the AGE for > and <
    mutate(label = factor(label, levels = c('X0', 'X1')))                        # Rename the label classes to valid R variable names
}
for (age in seq(40, 75, by = 5)) {
  for (gender in seq(0, 1, by = 1)) {
    i <- i+1
    ifelse( gender == 0, 
    df[[i]] <- age.buckets(age, gender)%>%
      select(label, features.v3[!features.v3 %in% c('OTHER_ARTERIOPATHIES',
                                                    'HYPERTENSION_AT_INDEX',
                                                    'MIGRAINE_AT_INDEX',
                                                    'NEOPLASM_BRAIN_TUMOR',
                                                    'DYSLIPIDEMIA_AT_INDEX',
                                                    'BMI_CLOSEST_TO_INDEX_BINARY',
                                                    'DRUG_DEP_ABUSE_AT_INDEX')]),
    df[[i]] <- age.buckets(age, gender)%>%
      select(label, features.v3[!features.v3 %in% c('RHEUM_AT_INDEX_3monthsPost',
                                                    'CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX',
                                                    'TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost',
                                                    'HYPERTENSION_AT_INDEX',
                                                    'DRUG_DEP_ABUSE_AT_INDEX',
                                                    'ALCOHOL_DEP_ABUSE_AT_INDEX',
                                                    'FAM_HIST')])
    )
  }
}
df[[33]] <- subset(df.woImp, PT_SEX == 0)%>%
  mutate(label = factor(label, levels = c('X0', 'X1')))%>%
  select(label, features.v3[!features.v3 %in% c('OTHER_ARTERIOPATHIES',
                                                'HYPERTENSION_AT_INDEX',
                                                'MIGRAINE_AT_INDEX',
                                                'NEOPLASM_BRAIN_TUMOR',
                                                'DYSLIPIDEMIA_AT_INDEX',
                                                'BMI_CLOSEST_TO_INDEX_BINARY',
                                                'DRUG_DEP_ABUSE_AT_INDEX')])
df[[34]] <- subset(df.woImp, PT_SEX == 1)%>%
  mutate(label = factor(label, levels = c('X0', 'X1')))%>%
  select(label, features.v3[!features.v3 %in% c('RHEUM_AT_INDEX_3monthsPost',
                                                'CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX',
                                                'TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost',
                                                'HYPERTENSION_AT_INDEX',
                                                'DRUG_DEP_ABUSE_AT_INDEX',
                                                'ALCOHOL_DEP_ABUSE_AT_INDEX',
                                                'FAM_HIST')])
names(df) <- c('female.age.lt.40', 'male.age.lt.40', 'female.age.lt.45', 'male.age.lt.45', 'female.age.lt.50', 'male.age.lt.50',
               'female.age.lt.55', 'male.age.lt.55', 'female.age.lt.60', 'male.age.lt.60', 'female.age.lt.65', 'male.age.lt.65',
               'female.age.lt.70', 'male.age.lt.70', 'female.age.lt.75', 'male.age.lt.75',
               'female.age.gt.40', 'male.age.gt.40', 'female.age.gt.45', 'male.age.gt.45', 'female.age.gt.50', 'male.age.gt.50',
               'female.age.gt.55', 'male.age.gt.55', 'female.age.gt.60', 'male.age.gt.60', 'female.age.gt.65', 'male.age.gt.65',
               'female.age.gt.70', 'male.age.gt.70', 'female.age.gt.75', 'male.age.gt.75',
               'female.ALL', 'male.ALL'
)
trainSets.v4 <- list()
testSets.v4 <- list()
for (dat in 1:length(df)) {
  index <- caTools::sample.split(df[[dat]]$label, SplitRatio = .8)
  trainSets.v4[[dat]] <- subset(df[[dat]], index == TRUE)
  testSets.v4[[dat]] <- subset(df[[dat]], index == FALSE)
  rm(index,dat)
}
names(trainSets.v4) <- names(df)
names(testSets.v4) <- names(df)
# Model development
ml.train.v4 <- list()
classifiers <- c("glm","rf","svmRadial","xgbDART")
paramGrid <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          summaryFunction = twoClassSummary,                      # Evaluate performance
                          classProbs = T,                                         # Estimate class probabilities
                          allowParallel = T,
                          search = "random")
for (dat in 1:length(trainSets.v4)){
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.v4[dat]),"at",Sys.time()))
    ml.train.v4[[paste(names(trainSets.v4[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
                                                                                          data = trainSets.v4[[dat]],
                                                                                          method = classifiers[[c]],
                                                                                          preProcess = c("center","scale"),
                                                                                          metric = "ROC",
                                                                                          trControl = paramGrid,
                                                                                          tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets.v4[dat]),"at",Sys.time()))
  }
}
# Model validation
incrementStart <- function(x){4*x-3}
incrementEnd <- function(x){4*x}
results.v4 <- as.data.frame(list())
for (dat in 1:length(testSets.v4)){
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions <- setNames(
      data.frame(
        testSets.v4[[dat]]$label,
        predict(object = ml.train.v4[[c]], testSets.v4[[dat]], type = "prob"),
        predict(object = ml.train.v4[[c]], testSets.v4[[dat]], type = "raw")
      ),
      c("obs","X0","X1","pred")
    )
    cm <- confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp <- as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.v4[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.v4[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.v4[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.v4 <- rbind(results.v4,
                        tmp%>%
                          mutate(
                            "Classifier" = names(ml.train.v4[c]),
                            "95%CI"= paste0("(",round(AccuracyLower,3),",",round(AccuracyUpper,3),")")
                          )%>%
                          select(
                            c("Classifier",AUROC="23",AUPR="24","Accuracy","95%CI",NIR="AccuracyNull",
                              "Kappa","Sensitivity","Specificity",
                              "Precision","F1","Prevalence",TP="4",FP="2",FN="3",TN="1")
                          )
    )
    rm(tmp,cm,predictions)
  }
}
# Feature Importance
feaImp.v4 <- data.frame(features = character(0))                                   # create empty data frame with a column to append data iterative
for (i in 1:length(ml.train.v4)) {
  ifelse(
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           # Check for 'pattern'(glm|rf|xgb) 
          names(ml.train.v4[i])),                                               # in the 'string'(female.age.lt.40.glm)
    # if TRUE then do
    feaImp.v4 <- full_join(feaImp.v4, 
                           varImp(ml.train.v4[[i]])$importance%>%              
                             mutate(features = rownames(.),
                                    model = names(ml.train.v4[i]))%>%
                             select(features, model, Overall)%>%
                             arrange(model, features)%>%
                             rename(!!quo_name(names(ml.train.v4[i])) := Overall), # dynamically rename the default 'Overall' column
                           by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  )
}
openxlsx::write.xlsx(feaImp.v4, "feaImp.v4.xlsx")
feaImp.v4.long <- data.frame(t(feaImp.v4))
colnames(feaImp.v4.long) <- feaImp.v4.long[1,]
feaImp.v4.long <- feaImp.v4.long[-1,]
feaImp.v4.long <- data.frame(sapply(feaImp.v4.long, function(x) as.numeric(as.character(x))),
                             row.names = rownames(feaImp.v4.long))
feaImp.v4.long$conditions <- rownames(feaImp.v4.long)
rownames(feaImp.v4.long) <- NULL
feaImp.v4.long <- cbind(
  stringr::str_split_fixed(feaImp.v4.long$conditions, fixed('.'), 5),
  feaImp.v4.long%>%
    select(-c(conditions))
)
feaImp.v4.long <- feaImp.v4.long[,-2]
names(feaImp.v4.long)[1:4] <- c('gender', 'condition', 'age', 'model')
openxlsx::write.xlsx(feaImp.v4, "feaImp.v4.xlsx")
feaImp.v4.long <-readxl::read_excel("feaImp.v4.xlsx",na=c("NA"),col_names = T)
feaImp.v4.long <- feaImp.v4.long%>%
  select(-c(model))%>%
  group_by(gender, condition, age)%>%
  summarize_all(., funs(mean=mean))
row_names <- paste(feaImp.v4.long$gender, feaImp.v4.long$condition, feaImp.v4.long$age, sep = ".")
feaImp.v4.long <- feaImp.v4.long[-c(1:3)]
rownames(feaImp.v4.long) <- row_names
colnames(feaImp.v4.long) <- sub("_mean", "", colnames(feaImp.v4.long))
# version-1: excluding all 'greater than' groups from the heat map
indexes.of.lt.rownames <- c(grep('lt', row.names(feaImp.v4.long)),1,18)
v4.heatmap.matrix <- data.frame(feaImp.v4.long[indexes.of.lt.rownames,],
                                row.names = rownames(feaImp.v4.long)[indexes.of.lt.rownames])
dev.off()
library(gplots)
#females
heatmap.2(
  #hclust.matrix.v3.female,
  #as.matrix(t(v1.heatmap.matrix)),
  as.matrix(t(log(v4.heatmap.matrix[grepl('female', row.names(v4.heatmap.matrix)),]%>%
                    select(-c('OTHER_ARTERIOPATHIES',
                              #'HYPERTENSION_AT_INDEX',
                              'MIGRAINE_AT_INDEX',
                              'NEOPLASM_BRAIN_TUMOR',
                              'DYSLIPIDEMIA_AT_INDEX',
                              'BMI_CLOSEST_TO_INDEX_BINARY'
                              #'DRUG_DEP_ABUSE_AT_INDEX'
                    ))+1))),
  trace = "none", 
  main = "FEMALES",
  #density.info = "none",
  dendrogram = c('row'),
  key.title = "", 
  key = "True",
  keysize = .75,
  key.par = list(mar=c(5,3,1,1)),
  key.xlab = 'Avg. Feature Importance Score\n(log-transformed)',
  key.ylab = "Count",
  margins = c(7,20),
  xlab = "",                          # Names the x-axis
  #labCol = "",                        # to show X-axis labels
  cexRow = .75,
  cexCol = 1,
  Colv = FALSE,
  #col = rev(heat.colors(999))
  symbreaks = FALSE,
  col = colorRampPalette(c("red",'yellow','darkgreen'))
)
#males
heatmap.2(
  #hclust.matrix.v3.female,
  #as.matrix(t(v1.heatmap.matrix)),
  as.matrix(t(log(v4.heatmap.matrix[setdiff(grep('male', row.names(v4.heatmap.matrix)), grep('female', row.names(v4.heatmap.matrix))),]%>%
                    select(-c('RHEUM_AT_INDEX_3monthsPost',
                               'CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX',
                               'TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost',
                               #'HYPERTENSION_AT_INDEX',
                               #'DRUG_DEP_ABUSE_AT_INDEX',
                               'ALCOHOL_DEP_ABUSE_AT_INDEX',
                               'FAM_HIST'))+1))),
  trace = "none", 
  main = "MALES",
  #density.info = "none",
  dendrogram = c('row'),
  key.title = "", 
  key = "True",
  keysize = .75,
  key.par = list(mar=c(5,3,1,1)),
  key.xlab = 'Avg. Feature Importance Score\n(log-transformed)',
  key.ylab = "Count",
  margins = c(7,20),
  xlab = "",                          # Names the x-axis
  #labCol = "",                        # to show X-axis labels
  cexRow = .75,
  cexCol = 1,
  Colv = FALSE,
  #col = rev(heat.colors(999))
  symbreaks = FALSE,
  col = colorRampPalette(c("red",'yellow','darkgreen'))
)

##############################################
# Revisit: added new age cutoffs for v3 input variables 01/27/2021
##############################################
# Model development
ml.train.v3.1 <- list()
classifiers <- c("glm","rf","svmRadial","xgbDART")
paramGrid <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          summaryFunction = twoClassSummary,                      # Evaluate performance
                          classProbs = T,                                         # Estimate class probabilities
                          allowParallel = T,
                          search = "random")
for (dat in 1:length(trainSets.v3)){
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.v3[dat]),"at",Sys.time()))
    ml.train.v3.1[[paste(names(trainSets.v3[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
                                                                                          data = trainSets.v3[[dat]],
                                                                                          method = classifiers[[c]],
                                                                                          preProcess = c("center","scale"),
                                                                                          metric = "ROC",
                                                                                          trControl = paramGrid,
                                                                                          tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets.v3[dat]),"at",Sys.time()))
  }
}
# Model validation
incrementStart<-function(x){4*x-3}
incrementEnd<-function(x){4*x}
results.v3.1 <- as.data.frame(list())
for (dat in 1:length(testSets.v3)){
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions <- setNames(
      data.frame(
        testSets.v3[[dat]]$label,
        predict(object = ml.train.v3.1[[c]], testSets.v3[[dat]], type = "prob"),
        predict(object = ml.train.v3.1[[c]], testSets.v3[[dat]], type = "raw")
      ),
      c("obs","X0","X1","pred")
    )
    cm <- confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp <- as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.v3.1[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.v3.1[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.v3.1[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.v3.1 <- rbind(results.v3.1,
                        tmp%>%
                          mutate(
                            "Classifier" = names(ml.train.v3.1[c]),
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
feaImp.v3.1 <- data.frame(features = character(0))                                   # create empty data frame with a column to append data iterative
for (i in 1:length(ml.train.v3.1)) {
  ifelse(
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           # Check for 'pattern'(glm|rf|xgb) 
          names(ml.train.v3.1[i])),                                               # in the 'string'(female.age.lt.40.glm)
    # if TRUE then do
    feaImp.v3.1 <- full_join(feaImp.v3.1, 
                           varImp(ml.train.v3.1[[i]])$importance%>%              
                             mutate(features = rownames(.),
                                    model = names(ml.train.v3.1[i]))%>%
                             select(features, model, Overall)%>%
                             arrange(model, features)%>%
                             rename(!!quo_name(names(ml.train.v3.1[i])) := Overall), # dynamically rename the default 'Overall' column
                           by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  )
}
openxlsx::write.xlsx(feaImp.v3.1, "feaImp.v3.1.xlsx")
feaImp.v3.1.long <- data.frame(t(feaImp.v3.1))
colnames(feaImp.v3.1.long) <- feaImp.v3.1.long[1,]
feaImp.v3.1.long <- feaImp.v3.1.long[-1,]
feaImp.v3.1.long <- data.frame(sapply(feaImp.v3.1.long, function(x) as.numeric(as.character(x))),
                             row.names = rownames(feaImp.v3.1.long))
feaImp.v3.1.long$conditions <- rownames(feaImp.v3.1.long)
rownames(feaImp.v3.1.long) <- NULL
feaImp.v3.1.long <- cbind(
  stringr::str_split_fixed(feaImp.v3.1.long$conditions, fixed('.'), 5),
  feaImp.v3.1.long%>%
    select(-c(conditions))
)
feaImp.v3.1.long <- feaImp.v3.1.long[,-2]
names(feaImp.v3.1.long)[1:4] <- c('gender', 'condition', 'age', 'model')
openxlsx::write.xlsx(feaImp.v3.1.long, "feaImp.v3.1.xlsx")
feaImp.v3.1.long <-readxl::read_excel("feaImp.v3.1.xlsx",na=c("NA"),col_names = T)
feaImp.v3.1.long <- feaImp.v3.1.long%>%
  select(-c(model))%>%
  group_by(gender, condition, age)%>%
  summarize_all(., funs(mean=mean))
row_names <- paste(feaImp.v3.1.long$gender, feaImp.v3.1.long$condition, feaImp.v3.1.long$age, sep = ".")
feaImp.v3.1.long <- feaImp.v3.1.long[-c(1:3)]
rownames(feaImp.v3.1.long) <- row_names
colnames(feaImp.v3.1.long) <- sub("_mean", "", colnames(feaImp.v3.1.long))
# version-1: excluding all 'greater than' groups from the heat map
indexes.of.lt.rownames <- c(grep('lt', row.names(feaImp.v3.1.long)),1,18)
v3.1.heatmap.matrix <- data.frame(feaImp.v3.1.long[indexes.of.lt.rownames,],
                                row.names = rownames(feaImp.v3.1.long)[indexes.of.lt.rownames])

####################
# Cosine similarity
##################
cosine.similarity <- function(x, y){
  (x %*% y) / sqrt(x%*%x * y%*%y)
}
tmp <- data.frame(t(feaImp.v4.long))
cosine.v4 <- data.frame(
  f.40.45 = c(cosine.similarity(tmp$female.lt.40, tmp$female.lt.45)),
  f.45.50 = c(cosine.similarity(tmp$female.lt.45, tmp$female.lt.50)),
  f.50.55 = c(cosine.similarity(tmp$female.lt.50, tmp$female.lt.55)),
  f.55.60 = c(cosine.similarity(tmp$female.lt.55, tmp$female.lt.60)),
  f.60.65 = c(cosine.similarity(tmp$female.lt.60, tmp$female.lt.65)),
  f.65.70 = c(cosine.similarity(tmp$female.lt.65, tmp$female.lt.70)),
  f.70.75 = c(cosine.similarity(tmp$female.lt.70, tmp$female.lt.75)),
  f.75.ALL = c(cosine.similarity(tmp$female.lt.75, tmp$female.ALL.ALL)),
  m.40.45 = c(cosine.similarity(tmp$male.lt.40, tmp$male.lt.45)),
  m.45.50 = c(cosine.similarity(tmp$male.lt.45, tmp$male.lt.50)),
  m.50.55 = c(cosine.similarity(tmp$male.lt.50, tmp$male.lt.55)),
  m.55.60 = c(cosine.similarity(tmp$male.lt.55, tmp$male.lt.60)),
  m.60.65 = c(cosine.similarity(tmp$male.lt.60, tmp$male.lt.65)),
  m.65.70 = c(cosine.similarity(tmp$male.lt.65, tmp$male.lt.70)),
  m.70.75 = c(cosine.similarity(tmp$male.lt.70, tmp$male.lt.75)),
  m.75.ALL = c(cosine.similarity(tmp$male.lt.75, tmp$male.ALL.ALL))
)
tmp <- data.frame(t(feaImp.v3.1.long))
cosine.v3.1 <- data.frame(
  f.40.45 = c(cosine.similarity(tmp$female.lt.40, tmp$female.lt.45)),
  f.45.50 = c(cosine.similarity(tmp$female.lt.45, tmp$female.lt.50)),
  f.50.55 = c(cosine.similarity(tmp$female.lt.50, tmp$female.lt.55)),
  f.55.60 = c(cosine.similarity(tmp$female.lt.55, tmp$female.lt.60)),
  f.60.65 = c(cosine.similarity(tmp$female.lt.60, tmp$female.lt.65)),
  f.65.70 = c(cosine.similarity(tmp$female.lt.65, tmp$female.lt.70)),
  f.70.75 = c(cosine.similarity(tmp$female.lt.70, tmp$female.lt.75)),
  f.75.ALL = c(cosine.similarity(tmp$female.lt.75, tmp$female.ALL.ALL)),
  m.40.45 = c(cosine.similarity(tmp$male.lt.40, tmp$male.lt.45)),
  m.45.50 = c(cosine.similarity(tmp$male.lt.45, tmp$male.lt.50)),
  m.50.55 = c(cosine.similarity(tmp$male.lt.50, tmp$male.lt.55)),
  m.55.60 = c(cosine.similarity(tmp$male.lt.55, tmp$male.lt.60)),
  m.60.65 = c(cosine.similarity(tmp$male.lt.60, tmp$male.lt.65)),
  m.65.70 = c(cosine.similarity(tmp$male.lt.65, tmp$male.lt.70)),
  m.70.75 = c(cosine.similarity(tmp$male.lt.70, tmp$male.lt.75)),
  m.75.ALL = c(cosine.similarity(tmp$male.lt.75, tmp$male.ALL.ALL))
)
openxlsx::write.xlsx(rbind(cosine.v3.1, cosine.v4), "cosine.similarity.xlsx")
# For each age cut-off, create two vectors: average feature importance less and more than the age for all the variables and then calculate the similarity score.
cosine.v3.1.1 <- data.frame(
  f.40 = c(cosine.similarity(tmp$female.lt.40, tmp$female.gt.40)),
  f.45 = c(cosine.similarity(tmp$female.lt.45, tmp$female.gt.45)),
  f.50 = c(cosine.similarity(tmp$female.lt.50, tmp$female.gt.50)),
  f.55 = c(cosine.similarity(tmp$female.lt.55, tmp$female.gt.55)),
  f.60 = c(cosine.similarity(tmp$female.lt.60, tmp$female.gt.60)),
  f.65 = c(cosine.similarity(tmp$female.lt.65, tmp$female.gt.65)),
  f.70 = c(cosine.similarity(tmp$female.lt.70, tmp$female.gt.70)),
  f.75 = c(cosine.similarity(tmp$female.lt.75, tmp$female.gt.75)),
  m.40 = c(cosine.similarity(tmp$male.lt.40, tmp$male.gt.40)),
  m.45 = c(cosine.similarity(tmp$male.lt.45, tmp$male.gt.45)),
  m.50 = c(cosine.similarity(tmp$male.lt.50, tmp$male.gt.50)),
  m.55 = c(cosine.similarity(tmp$male.lt.55, tmp$male.gt.55)),
  m.60 = c(cosine.similarity(tmp$male.lt.60, tmp$male.gt.60)),
  m.65 = c(cosine.similarity(tmp$male.lt.65, tmp$male.gt.65)),
  m.70 = c(cosine.similarity(tmp$male.lt.70, tmp$male.gt.70)),
  m.75 = c(cosine.similarity(tmp$male.lt.75, tmp$male.gt.75))
)
openxlsx::write.xlsx(cosine.v3.1.1, "cosine.similarity.xlsx")


#########################
# Summary stats for v3.1
#########################
tmp <- rbind(trainSets.v3[[1]],testSets.v3[[1]])%>%
  select(-c(label))%>%
  group_by(n= n())%>%
  summarise_all(., funs(sum))
summary.Stats.v3.1 <- tmp[0, ]
rm(tmp, dat)
summaryStats <- function(dat){
  tmp <- #rbind(trainSets.v3[[dat]],testSets.v3[[dat]])%>%
    subset(rbind(trainSets.v3[[dat]],testSets.v3[[dat]]), label == 'X1')%>%
  select(-c(label))%>%
  group_by(n= n())%>%
  summarise_all(., funs(sum))
  rownames(tmp) <- names(trainSets.v3[dat])
  return(tmp)
}
rm(tmp, dat)
for (dat in 1:length(trainSets.v3)){
  summary.Stats.v3.1 <- rbind(summary.Stats.v3.1,
                        summaryStats(dat))
}
openxlsx::write.xlsx(summary.Stats.v3.1, "summarystats_v3.1_CASES.xlsx", 
                     row.names=TRUE)
#--get model parameters
results.v3.1.params <- as.data.frame(list())
for (model in 1:length(ml.train.v3.1)) {
  tmp <- ml.train.v3.1[[model]]
  tmp <- data.frame(
    model = names(ml.train.v3.1[model]),
    AUROC = data.frame(tmp$results)[best.Model,]$ROC,
    Sensitivity = data.frame(tmp$results)[best.Model,]$Sens,
    Specificity = data.frame(tmp$results)[best.Model,]$Spec,
    data.frame(tmp$results)[best.Model,]%>%
      select(-c('ROC', 'Sens', 'Spec', 'ROCSD', 'SensSD', 'SpecSD'))
  )
  results.v3.1.params <- bind_rows(tmp,
                                   results.v3.1.params)
  rm(tmp)
}
results.v3.1.params <- results.v3.1.params %>% select(-c('parameter'))
results.v3.1.params[is.na(results.v3.1.params)] <- ""
openxlsx::write.xlsx(results.v3.1.params, "results_v3.1.params.xlsx", 
                     row.names=TRUE)

#-----------------------------------------------------------------------------------
# Revisit: updated PFO_ALL_TIME,  split NEOPLASM, BRAIN TUMOR variables 05/22/2021
#-----------------------------------------------------------------------------------
vars.v5 <- function(dat){
  dat%>%
    mutate(
      OTHER_ARTERIOPATHIES = if_else(
        VASCULITIS_AT_INDEX_3monthsPost == 1 | FIBROMUSCULAR_DYSPLASIA_ALL_TIME == 1 |
          REVERSIBLE_CEREBRAL_VASOCONSTRICTION_SYNDROME_AT_INDEX_3monthsPost == 1 |
          MOYAMOYA_ALL_TIME == 1 | SICKLE_CELL_ALL_TIME == 1, 1, 0),
      OTHER_CHRONIC_SYSTEMIC_DISORDERS = if_else(
        CHRONIC_LIVER_DIS_AT_INDEX == 1 | CHRONIC_LUNG_DIS_AT_INDEX == 1 |
          CHRONIC_KIDNEY_DIS_AT_INDEX == 1 | CIRRHOSIS_AT_INDEX_3monthsPost == 1 |
          ESRD_AT_INDEX == 1, 1, 0),
      FAM_HIST = if_else(
        FAM_HEART_HIST == 1 | FAM_STROKE_HIST ==1, 1, 0)
      #, NEOPLASM_BRAIN_TUMOR = if_else( NEOPLASM_AT_INDEX_3monthsPost == 1 | BRAIN_TUMOR_AT_INDEX == 1, 1, 0)
    )%>%
    select(label,
           OTHER_ARTERIOPATHIES,
           OTHER_CHRONIC_SYSTEMIC_DISORDERS,
           HYPERCOAG_STATES_AT_INDEX_3monthsPost,
           AFIB_FLUTTER_AT_INDEX_3monthsPost,
           ALCOHOL_DEP_ABUSE_AT_INDEX,
           CHF_AT_INDEX,
           DIABETES_AT_INDEX,
           DRUG_DEP_ABUSE_AT_INDEX,
           DYSLIPIDEMIA_AT_INDEX,
           FAM_HIST,
           HYPERTENSION_AT_INDEX,
           MI_AT_INDEX,
           PERI_VASC_DIS_AT_INDEX_3monthsPost,
           PFO_ALL_TIME,
           SMOKE_STTS,
           BMI_CLOSEST_TO_INDEX_BINARY,
           CERVICOCEPHALIC_ARTERIAL_DISSECTION_AT_INDEX,
           NEOPLASM_AT_INDEX_3monthsPost,
           BRAIN_TUMOR_AT_INDEX,
           RHEUM_AT_INDEX_3monthsPost,
           MIGRAINE_AT_INDEX,
           TEMPORAL_ARTERITIS_AT_INDEX_3monthsPost
    )
}
trainSets.v5 <- list()
for (dat in 1:length(trainSets)) {
  trainSets.v5[[dat]] <- vars.v5(trainSets[[dat]])
}
names(trainSets.v5) <- names(trainSets)
testSets.v5 <- list()
for (dat in 1:length(testSets)) {
  testSets.v5[[dat]] <- vars.v5(testSets[[dat]])
}
names(testSets.v5) <- names(testSets)
# Model development
ml.train.v5 <- list()
classifiers <- c("glm","rf","svmRadial","xgbDART")
paramGrid <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          summaryFunction = twoClassSummary,                      # Evaluate performance
                          classProbs = T,                                         # Estimate class probabilities
                          allowParallel = T,
                          search = "random")
for (dat in 1:length(trainSets.v5)){
  for (c in 1:length(classifiers)) {
    print(paste("started:",classifiers[[c]],names(trainSets.v5[dat]),"at",Sys.time()))
    ml.train.v5[[paste(names(trainSets.v5[dat]),".",classifiers[[c]],sep = "")]] <- train(label~.,
                                                                                       data = trainSets.v5[[dat]],
                                                                                       method = classifiers[[c]],
                                                                                       preProcess = c("center","scale"),
                                                                                       metric = "ROC",
                                                                                       trControl = paramGrid,
                                                                                       tuneLength = 5
    )
    print(paste("finished:",classifiers[[c]],names(trainSets.v5[dat]),"at",Sys.time()))
  }
}
save(list=ls(),file="backup_05242021.rda")
# Model validation
incrementStart <- function(x){4*x-3}
incrementEnd <- function(x){4*x}
results.v5 <- as.data.frame(list())
for (dat in 1:length(testSets.v5)){
  for (c in incrementStart(dat):incrementEnd(dat)) {
    predictions <- setNames(
      data.frame(
        testSets.v5[[dat]]$label,
        predict(object = ml.train.v5[[c]], testSets.v5[[dat]], type = "prob"),
        predict(object = ml.train.v5[[c]], testSets.v5[[dat]], type = "raw")
      ),
      c("obs","X0","X1","pred")
    )
    cm <- confusionMatrix(
      reference = predictions$obs,
      data = predictions$pred,
      mode = "everything",
      positive = "X1"
    )
    tmp <- as.data.frame(t(rbind(
      fetchResults(cm$byClass,ml.train.v5[c]),                                                             # Fetch Recall,Specificity,Precision
      fetchResults(cm$overall,ml.train.v5[c]),                                                             # Fetch Accuracy,95%CI
      fetchResults(as.data.frame(cm$table)$Freq,ml.train.v5[c]),                                           # Fetch TP,FP,FN,TN
      roc(predictor = predictions$X1,response = predictions$obs,levels = rev(levels(predictions$obs)))$auc,      # Calculate AUROC
      prSummary(predictions, lev = rev(levels(predictions$obs)))[1]                                              # Calculate AUPR
    )))
    results.v5 <- rbind(results.v5,
                        tmp%>%
                          mutate(
                            "Classifier" = names(ml.train.v5[c]),
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
# Feature Importance
feaImp.v5 <- data.frame(features = character(0))                                   # create empty data frame with a column to append data iterative
for (i in 1:length(ml.train.v5)) {
  ifelse( i != 4, 
  ifelse(
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           # Check for 'pattern'(glm|rf|xgb) 
          names(ml.train.v5[i])),                                               # in the 'string'(female.age.lt.40.glm)
    # if TRUE then do
    feaImp.v5 <- full_join(feaImp.v5, 
                           varImp(ml.train.v5[[i]])$importance%>%              
                             mutate(features = rownames(.),
                                    model = names(ml.train.v5[i]))%>%
                             select(features, model, Overall)%>%
                             arrange(model, features)%>%
                             rename(!!quo_name(names(ml.train.v5[i])) := Overall), # dynamically rename the default 'Overall' column
                           by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  ),
  print('error XGB'))
}
openxlsx::write.xlsx(feaImp.v5, "feaImp.v5.xlsx")
# Summary stats for v5.0
tmp <- rbind(trainSets.v5[[1]],testSets.v5[[1]])%>%
  select(-c(label))%>%
  group_by(n= n())%>%
  summarise_all(., funs(sum))
summary.Stats.v5 <- tmp[0, ]
rm(tmp, dat)
summaryStats <- function(dat){
  tmp <- #rbind(trainSets.v5[[dat]],testSets.v5[[dat]])%>%
    subset(rbind(trainSets.v5[[dat]],testSets.v5[[dat]]), label == 'X0')%>%
    select(-c(label))%>%
    group_by(n= n())%>%
    summarise_all(., funs(sum))
  rownames(tmp) <- names(trainSets.v5[dat])
  return(tmp)
}
rm(tmp, dat)
for (dat in 1:length(trainSets.v5)){
  summary.Stats.v5 <- rbind(summary.Stats.v5,
                              summaryStats(dat))
}
openxlsx::write.xlsx(summary.Stats.v5, "summarystats_v5_CONTROLS.xlsx", 
                     row.names=TRUE)
#--get model parameters
results.v5.params <- as.data.frame(list())
for (model in 1:length(ml.train.v5)) {
  tmp <- ml.train.v5[[model]]
  tmp <- data.frame(
    model = names(ml.train.v5[model]),
    AUROC = data.frame(tmp$results)[best.Model,]$ROC,
    Sensitivity = data.frame(tmp$results)[best.Model,]$Sens,
    Specificity = data.frame(tmp$results)[best.Model,]$Spec,
    data.frame(tmp$results)[best.Model,]%>%
      select(-c('ROC', 'Sens', 'Spec', 'ROCSD', 'SensSD', 'SpecSD'))
  )
  results.v5.params <- bind_rows(tmp,
                                   results.v5.params)
  rm(tmp)
}
results.v5.params <- results.v5.params %>% select(-c('parameter'))
results.v5.params[is.na(results.v5.params)] <- ""
openxlsx::write.xlsx(results.v5.params, "results_v5.params.xlsx", 
                     row.names=TRUE)
# COSINE similarity
feaImp.v5.long <- data.frame(t(feaImp.v5))
colnames(feaImp.v5.long) <- feaImp.v5.long[1,]
feaImp.v5.long <- feaImp.v5.long[-1,]
feaImp.v5.long <- data.frame(sapply(feaImp.v5.long, function(x) as.numeric(as.character(x))),
                             row.names = rownames(feaImp.v5.long))
feaImp.v5.long$conditions <- rownames(feaImp.v5.long)
rownames(feaImp.v5.long) <- NULL
feaImp.v5.long <- cbind(
  stringr::str_split_fixed(feaImp.v5.long$conditions, fixed('.'), 5),
  feaImp.v5.long%>%
    select(-c(conditions))
)
feaImp.v5.long <- feaImp.v5.long[,-2]
names(feaImp.v5.long)[1:4] <- c('gender', 'condition', 'age', 'model')
openxlsx::write.xlsx(feaImp.v5, "feaImp.v5.xlsx")
feaImp.v5.long <-readxl::read_excel("feaImp.v5.xlsx",na=c("NA"),col_names = T)
feaImp.v5.long <- feaImp.v5.long%>%
  select(-c(model))%>%
  group_by(gender, condition, age)%>%
  summarize_all(., funs(mean=sd))
row_names <- paste(feaImp.v5.long$gender, feaImp.v5.long$condition, feaImp.v5.long$age, sep = ".")
feaImp.v5.long <- feaImp.v5.long[-c(1:3)]
rownames(feaImp.v5.long) <- row_names
colnames(feaImp.v5.long) <- sub("_mean", "", colnames(feaImp.v5.long))
  tmp <- data.frame(t(feaImp.v5.long))
  cosine.v5 <- data.frame(
    f.40 = c(cosine.similarity(tmp$female.lt.40, tmp$female.ALL.ALL)),
    f.45 = c(cosine.similarity(tmp$female.lt.45, tmp$female.ALL.ALL)),
    f.50 = c(cosine.similarity(tmp$female.lt.50, tmp$female.ALL.ALL)),
    f.55 = c(cosine.similarity(tmp$female.lt.55, tmp$female.ALL.ALL)),
    f.60 = c(cosine.similarity(tmp$female.lt.60, tmp$female.ALL.ALL)),
    f.65 = c(cosine.similarity(tmp$female.lt.65, tmp$female.ALL.ALL)),
    f.70 = c(cosine.similarity(tmp$female.lt.70, tmp$female.ALL.ALL)),
    f.75 = c(cosine.similarity(tmp$female.lt.75, tmp$female.ALL.ALL)),
    m.40 = c(cosine.similarity(tmp$male.lt.40, tmp$male.ALL.ALL)),
    m.45 = c(cosine.similarity(tmp$male.lt.45, tmp$male.ALL.ALL)),
    m.50 = c(cosine.similarity(tmp$male.lt.50, tmp$male.ALL.ALL)),
    m.55 = c(cosine.similarity(tmp$male.lt.55, tmp$male.ALL.ALL)),
    m.60 = c(cosine.similarity(tmp$male.lt.60, tmp$male.ALL.ALL)),
    m.65 = c(cosine.similarity(tmp$male.lt.65, tmp$male.ALL.ALL)),
    m.70 = c(cosine.similarity(tmp$male.lt.70, tmp$male.ALL.ALL)),
    m.75 = c(cosine.similarity(tmp$male.lt.75, tmp$male.ALL.ALL))
  )
  openxlsx::write.xlsx(cosine.v5, "cosine.similarity.SD.xlsx")
