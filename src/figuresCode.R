library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gplots)
library(ggpubr)
library(tidyverse)
library(maxstat)
options(scipen = 999)
options(readr.default_locale=readr::locale(tz="US/Eastern"))


#-----------
# Figure 1: Cosine mean
#----------
# notes: try attaching the data table in the figure
# check error bars with diff models and SD
cosine.long <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                          , sheet = 'cosine'
                          , na=c("NA")
                          , col_names = TRUE) 
cosine.long$age <- factor(cosine.long$age)
cosine.long$sex <- factor(cosine.long$sex)
tmp.cosine <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                         , sheet = 'tmp_cosine'
                         , na=c("NA")
                         , col_names = TRUE) 
tmp.cosine$age <- factor(tmp.cosine$age, 
                         levels = c('< 55', '55 - 70','> 70'))
tmp.cosine$sex <- factor(tmp.cosine$sex)
# match colors with fig.2 for SEX
unique(ggplot_build(fig.2)$data[[1]]["fill"])
tiff('fig.1A.lt_vs_gt.tiff', units = "in", width = 8, height = 8, res = 300, compression = 'jpeg')
fig.1A.lt_vs_gt <- ggplot(data = na.omit(cosine.long), 
                aes(x = age, y = cosine.lt.vs.gt, group = sex, color = sex)) +
  geom_line(size = 1.75) + 
  geom_point(size = 5, alpha = .75)+
  geom_text(aes(label = round(cosine.lt.vs.gt,2)), hjust = 1, vjust = 2, check_overlap = TRUE,
            fontface = 'bold') +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  theme_bw() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
  )
fig.1A.lt_vs_gt
dev.off()
tiff('fig.1B.lt_vs_ALL.tiff', units = "in", width = 8, height = 8, res = 300, compression = 'jpeg')
fig.1B.lt_vs_ALL <- ggplot(data = na.omit(cosine.long), 
                aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = 1.75) + 
  geom_point(size = 5, alpha = .75)+
  geom_text(aes(label = round(cosine.lt.vs.ALL,2)), hjust = 1, vjust = 2, check_overlap = TRUE,
            fontface = 'bold') +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  theme_bw() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
  )
fig.1B.lt_vs_ALL
dev.off()
tiff('fig.1C.newbreakouts_vs_ALL.tiff', units = "in", width = 8, height = 8, res = 300, compression = 'jpeg')
fig.1C.newbreakouts_vs_ALL <- ggplot(data = tmp.cosine, 
       aes(x = age, y = value, group = sex, color = sex)) +
  geom_line(size = 1.75) + 
  geom_point(size = 5, alpha = .75)+
  geom_text(aes(label = round(value,2)), hjust = 1, vjust = 2, check_overlap = TRUE,
            fontface = 'bold') +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  theme_bw() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
  )
fig.1C.newbreakouts_vs_ALL
dev.off()
#-----------
# Figure 1: Cosine model based
#----------
cosine.long <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                          , sheet = 'cosine.model.feaimp'
                          , na=c("NA")
                          , col_names = TRUE) 
cosine.long$age <- factor(cosine.long$age)
cosine.long$sex <- factor(cosine.long$sex)
cosine.long$model <- factor(cosine.long$model)
tiff('fig.1A.lt_vs_gt_modelbased.tiff', units = "in", width = 12, height = 6, res = 300, compression = 'jpeg')
cosine.long.gt <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                          , sheet = 'cosine.copy'
                          , na=c("NA")
                          , col_names = TRUE) 
cosine.long.gt$age <- factor(cosine.long.gt$age)
cosine.long.gt$sex <- factor(cosine.long.gt$sex)
cosine.long.gt$model <- factor(cosine.long.gt$model)
ggplot(data = subset(na.omit(cosine.long.gt), !(age == '55-70')), 
                          aes(x = age, y = cosine.lt.vs.gt, group = sex, color = sex)) +
  geom_line(size = 1.5) + 
  geom_point(size = 2.5, alpha = .75)+
  geom_text(aes(label = round(cosine.lt.vs.gt,2))
            #, hjust = 1
            , vjust = 2
            , check_overlap = TRUE, size = 3.5
            #, fontface = 'bold'
  ) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  facet_wrap(model~.)+
  theme_bw() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
  )
dev.off()
tiff('fig.1B.lt_vs_ALL_modelbased.tiff', units = "in", width = 12, height = 6, res = 300, compression = 'jpeg')
fig.1B.lt_vs_ALL_modelbased <- ggplot(data = subset(na.omit(cosine.long), !(age == '55-70')), 
                           aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = 1.5) + 
  geom_point(size = 2.5, alpha = .75)+
  geom_text(aes(label = round(cosine.lt.vs.ALL,2))
            #, hjust = 1
            , vjust = 2
            , check_overlap = TRUE, size = 3.5
            #, fontface = 'bold'
            ) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  facet_wrap(model~.)+
  theme_bw() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
  )
dev.off()
tmp.cosine <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                         , sheet = 'cosine_new_breakouts'
                         , na=c("NA")
                         , col_names = TRUE) %>%
  filter(model != 'mean')
tmp.cosine$age <- factor(tmp.cosine$age, 
                         levels = c('< 55', '55 - 70','> 70'))
tmp.cosine$sex <- factor(tmp.cosine$sex)
tmp.cosine$model <- factor(tmp.cosine$model)
tiff('fig.1C.newbreakouts_vs_ALL_modelbased.tiff', units = "in", width = 12, height = 6, res = 300, compression = 'jpeg')
ggplot(data = tmp.cosine, 
       aes(x = age, y = value, group = sex, color = sex)) +
  geom_line(size = 1.5) + 
  geom_point(size = 2.5, alpha = .75)+
  geom_text(aes(label = round(value,2))
            #, hjust = 1
            , vjust = 2
            , check_overlap = TRUE, size = 3.5
            , fontface = 'bold'
  ) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  facet_wrap(model~.)+
  theme_bw() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
  )
dev.off()
#-----------
# Figure 2: Results
#----------
# try Box plots instead of line plots; 4 models into one box 
# include only less than & ALL criteria
# try colors for FEAMLE and MALE
results.wide <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                                      , sheet = 'results'
                                      , na=c("NA")
                                      , col_names = TRUE) %>%
  #select(c('SEX', 'CONDITION', 'AGE', 'MODEL', 'AUROC', 'Accuracy', 'PPV', 'NPV')) %>%
  #mutate(across(c('SEX', 'CONDITION', 'AGE', 'MODEL'), factor))
  select(c('SEX', 'CRITERIA', 'MODEL',  'AUROC', 'ACCURACY', 'PPV', 'NPV')) %>%
  mutate(across(c('SEX', 'CRITERIA', 'MODEL'), factor)) %>%
  filter(., !grepl('>', CRITERIA))
results.long <- melt(results.wide)
#ggplot(results.long, aes(x = AGE, y = value, group = interaction(MODEL, CONDITION),
#                         color = MODEL)) +
#  geom_line() +
#  facet_grid(SEX~variable)
#ggplot(results.long, 
#       aes(x = CRITERIA, y = value, group = MODEL, color = MODEL)) +
#  geom_line(aes(linetype = MODEL), size = 1) +
#  facet_wrap(variable~SEX, scales = 'free_y') +
#  theme(
#    legend.position = 'top'
#  ) +
#  scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'dotdash'))
# finalized
tmp <- subset(results.long, variable == c('AUROC', 'ACCURACY'))
tmp <- unique(tmp[,c('variable','SEX')])
tmp$CRITERIA <- tmp$value <- 1
tiff('fig.2.tiff', units = "in", width = 14, height = 8, res = 300, compression = 'jpeg')
fig.2.data <- subset(results.long, variable == c('AUROC', 'ACCURACY') & 
                       !(CRITERIA %in% '55 - 70')
                     )
fig.2 <- ggplot(fig.2.data, 
       aes(x = CRITERIA, y = value)) +
  geom_boxplot(aes(fill = SEX), show.legend = FALSE) +
  geom_rect(data = tmp, aes(fill = SEX),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = .3, show.legend = FALSE) +
  facet_grid( variable~SEX, scales = 'free_y') +
  theme_gray() +
  labs(x = '\n Age at Index Ischemic Stroke', y = 'Performance Measure\n') +
  scale_y_continuous(limits = c(.5, 1.0)) +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 12, color = 'black', family = 'serif')
    , legend.text = element_text(size = 12, color = 'black', family = 'serif')
    , strip.text = element_text(size = 12, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_text(size = 12, color = 'black', family = 'serif')
  )
fig.2
dev.off()
#-----------
# Figure 3: Feature Importance
#----------
#  include only less than & ALL criteria
feaimp.wide <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                           , sheet = 'feaimp'
                           , na=c("NA")
                           , col_names = TRUE) %>%
  select(-c('CONDITION', 'AGE')) %>%
  mutate(across(c('SEX', 'CRITERIA', 'MODEL'), factor)) %>%
  filter(., !grepl('>', CRITERIA)) %>%
  select(-c(MODEL)) %>%
  group_by(SEX, CRITERIA) %>%
  summarise_all(.funs = c(mean = 'mean'))
  
feaimp.long <- melt(feaimp.wide)
openxlsx::write.xlsx(feaimp.long, 'feaimp.long.xlsx')
feaimp.long <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                          , sheet = 'feaimp.mean'
                          , na=c("NA")
                          , col_names = TRUE) %>%
  select(c('SEX','CRITERIA','CONDITION','variable','abbr','Feature Importance Score'))
colnames(feaimp.long) <- c("SEX","CRITERIA",'CONDITION',"variable",'abbr',"Feature Importance\nScore")
feaimp.long$abbr <- trimws(feaimp.long$abbr, which = c('both'))
feaimp.long$abbr <- factor(feaimp.long$abbr, levels = c(
  'Age',
  'Alcohol Dependance or Abuse',
  'Atrial Fibrillation / Flutter',
  'Body Mass Index',
  'Brain Tumor',
  'Cervicocephalic Arterial Dissection',
  'Chronic Systemic Disorders',
  'Congestive Heart Failure',
  'Diabetes Mellitus',
  'Drug Dependance or Abuse',
  'Family History of Stroke',
  'Hypercoagulable States',
  'Hypertension',
  'Migraine',
  'Myocardial Infarction',
  'Non CNS Neoplasm',
  'Other Arteriopathies',
  'Patent Foramen Ovale',
  'Peripheral Vascular Disease',
  'Rheumatic Diseases',
  'Smoking Status',
  'Temporal Arteritis'
))

# model-based figures
feaimp.wide <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                          , sheet = 'feaimp'
                          , na=c("NA")
                          , col_names = TRUE) %>%
  select(-c('AGE')) %>%
  mutate(across(c('SEX', 'CRITERIA', 'MODEL'), factor)) 
abbr <- read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx"
                   , sheet = 'abbr'
                   , na=c("NA")
                   , col_names = TRUE)
feaimp.long.modelbased <- left_join(
  melt(feaimp.wide), 
  abbr[,c('variable','abbr')],
by = 'variable') %>% 
  filter(variable != 'HYPERCOAG_STATES_AT_INDEX') %>%
  distinct() %>%
  rename('Feature Importance\nScore' = value)
# create a data frame with faceting variables
tmp <- unique(feaimp.long[,c('SEX')])
# add some dummy data with vars - X and Y used for the plotting
tmp$abbr <- tmp$CRITERIA <- 1
fig.3 <- ggplot(feaimp.long, aes(y = abbr, x = CRITERIA, alpha = `Feature Importance\nScore`)) +
  geom_tile(colour = 'white', size = .5)  +
  geom_rect(data = tmp, aes(fill = SEX),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = .3, show.legend = FALSE) +
  theme_bw() +
  facet_grid(. ~ SEX) +
  labs(x = '\n Age at Index Ischemic Stroke', y = 'Features') +
  theme(  legend.position = 'top'
        , axis.title = element_text(size = 12, color = 'black', family = 'serif')
        , axis.text.y = element_text(size = 12, color = 'black', family = 'serif')
        , axis.text.x = element_text(size = 9, color = 'black', family = 'serif')
        , legend.text = element_text(size = 12, color = 'black', family = 'serif')
        , strip.text = element_text(size = 12, color = 'black', family = 'serif')
        , legend.title = element_text(size = 12, color = 'black', family = 'serif')
        ) +
  guides(color = guide_legend(title.vjust = .5))
tiff('fig.3.tiff', units = "in", width = 18, height = 8, res = 300, compression = 'jpeg')
fig.3
dev.off()
# separate > and < age groups
fig.3.lt <- ggplot(subset(feaimp.long, CONDITION %in% c('lt')), 
                   aes(y = abbr, x = CRITERIA, alpha = `Feature Importance\nScore`)) +
  geom_tile(colour = 'white', size = .5)  +
  geom_rect(data = tmp, aes(fill = SEX),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = .3, show.legend = FALSE) +
  theme_bw() +
  facet_grid(. ~ SEX) +
  labs(x = '\n Age at Index Ischemic Stroke', y = 'Features') +
  theme(  legend.position = 'top'
          , axis.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.text.x = element_text(size = 12, color = 'black', family = 'serif')
          , legend.text = element_text(size = 12, color = 'black', family = 'serif')
          , strip.text = element_text(size = 12, color = 'black', family = 'serif')
          , legend.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.title.y = element_blank()
          , axis.text.y = element_blank()
  ) + 
  scale_y_discrete(position = 'right') +
  guides(color = guide_legend(title.vjust = .5))
fig.3.gt <- ggplot(subset(feaimp.long, CONDITION %in% c('gt')), 
                   aes(y = abbr, x = CRITERIA, alpha = `Feature Importance\nScore`)) +
  geom_tile(colour = 'white', size = .5)  +
  geom_rect(data = tmp, aes(fill = SEX),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = .3, show.legend = FALSE) +
  theme_bw() +
  facet_grid(. ~ SEX) +
  labs(x = '\n Age at Index Ischemic Stroke', y = 'Features') +
  theme(  legend.position = 'top'
          , axis.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.text = element_text(size = 12, color = 'black', family = 'serif')
          , legend.text = element_text(size = 12, color = 'black', family = 'serif')
          , strip.text = element_text(size = 12, color = 'black', family = 'serif')
          , legend.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.text.y = element_text(hjust = .5)
          , axis.title.y = element_blank()
  ) +
  guides(color = guide_legend(title.vjust = .5))
tiff('fig.3.tiff', units = "in", width = 16, height = 8, res = 300, compression = 'jpeg')
legend <- get_legend(fig.3.gt)
plot_grid(legend,
          plot_grid(
            fig.3.lt + theme(legend.position="none")
            , fig.3.gt + theme(legend.position="none")
            , rel_widths = c(.45,.55)
          ), 
          ncol = 1, rel_heights = c(.05,1))
dev.off()
# only lt age groups
fig.3.lt +
  theme(
    
  )
# model-based 
fig.3.lt.modelbased <- ggplot(subset(feaimp.long.modelbased, CONDITION %in% c('lt')), 
       aes(y = abbr, x = CRITERIA, alpha = `Feature Importance\nScore`)) +
  geom_tile(colour = 'white', size = .5)  +
  geom_rect(data = tmp, aes(fill = SEX),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = .3, show.legend = FALSE) +
  theme_bw() +
  facet_wrap(SEX ~ MODEL, ncol = 3) +
  labs(x = '\n Age at Index Ischemic Stroke', y = 'Features') +
  theme(  legend.position = 'top'
          , axis.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.text.x = element_text(size = 12, color = 'black', family = 'serif')
          , legend.text = element_text(size = 12, color = 'black', family = 'serif')
          , strip.text = element_text(size = 12, color = 'black', family = 'serif')
          , legend.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.title.y = element_blank()
          #, axis.text.y.right = element_blank()
  ) + 
  scale_y_discrete(position = 'right') +
  guides(color = guide_legend(title.vjust = .5))
fig.3.gt.modelbased <- ggplot(subset(feaimp.long.modelbased, CONDITION %in% c('gt')), 
                              aes(y = abbr, x = CRITERIA, alpha = `Feature Importance\nScore`)) +
  geom_tile(colour = 'white', size = .5)  +
  geom_rect(data = tmp, aes(fill = SEX),
            xmin = -Inf, xmax = Inf, 
            ymin = -Inf, ymax = Inf, alpha = .3, show.legend = FALSE) +
  theme_bw() +
  facet_wrap(SEX ~ MODEL, ncol = 3) +
  labs(x = '\n Age at Index Ischemic Stroke', y = 'Features') +
  theme(  legend.position = 'top'
          , axis.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.text = element_text(size = 12, color = 'black', family = 'serif')
          , legend.text = element_text(size = 12, color = 'black', family = 'serif')
          , strip.text = element_text(size = 12, color = 'black', family = 'serif')
          , legend.title = element_text(size = 12, color = 'black', family = 'serif')
          , axis.text.y = element_text(hjust = .5)
          , axis.title.y = element_blank()
  ) + 
  scale_y_discrete(position = 'left') +
  guides(color = guide_legend(title.vjust = .5))
tiff('fig.3.modelbased.tiff', units = "in", width = 20, height = 12, res = 300, compression = 'jpeg')
legend <- get_legend(fig.3.gt.modelbased)
plot_grid(legend,
          plot_grid(
            fig.3.lt.modelbased + theme(legend.position="none")
            , fig.3.gt.modelbased + theme(legend.position="none")
            , rel_widths = c(.45,.55)
          ), 
          ncol = 1, rel_heights = c(.05,1))
dev.off()


#-----
# Cosine sample generation
#-----
cosine.similarity <- function(x, y){
  (x %*% y) / sqrt(x%*%x * y%*%y)
}
feaimp.model.long <- readxl::read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx", 
                                        sheet = "feaimp.long_model_based_cosine")
feaimp.model.long <- data.frame(feaimp.model.long, row.names = 1)
feaimp.model.long <- data.frame(t(feaimp.model.long))
get.cosine <- function(nvar){
  return(
    data.frame(
  f.40.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.40.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.45.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.45.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.50.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.50.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.55.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.55.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.60.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.60.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.65.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.65.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.70.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.70.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.75.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.75.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.80.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.lt.80.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  f.bt.55.70.GLM = c(cosine.similarity(feaimp.model.long$FEMALE.bt55.70.GLM[1 : nvar], feaimp.model.long$FEMALE.ALL.GLM[1 : nvar])),
  m.40.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.40.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.45.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.45.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.50.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.50.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.55.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.55.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.60.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.60.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.65.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.65.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.70.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.70.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.75.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.75.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.80.GLM = c(cosine.similarity(feaimp.model.long$MALE.lt.80.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  m.bt.55.70.GLM = c(cosine.similarity(feaimp.model.long$MALE.bt55.70.GLM[1 : nvar], feaimp.model.long$MALE.ALL.GLM[1 : nvar])),
  
  f.40.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.40.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.45.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.45.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.50.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.50.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.55.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.55.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.60.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.60.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.65.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.65.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.70.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.70.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.75.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.75.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.80.RF = c(cosine.similarity(feaimp.model.long$FEMALE.lt.80.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  f.bt.55.70.RF = c(cosine.similarity(feaimp.model.long$FEMALE.bt55.70.RF[1 : nvar], feaimp.model.long$FEMALE.ALL.RF[1 : nvar])),
  m.40.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.40.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.45.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.45.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.50.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.50.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.55.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.55.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.60.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.60.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.65.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.65.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.70.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.70.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.75.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.75.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.80.RF = c(cosine.similarity(feaimp.model.long$MALE.lt.80.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  m.bt.55.70.RF = c(cosine.similarity(feaimp.model.long$MALE.bt55.70.RF[1 : nvar], feaimp.model.long$MALE.ALL.RF[1 : nvar])),
  
  f.40.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.40.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.45.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.45.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.50.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.50.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.55.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.55.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.60.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.60.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.65.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.65.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.70.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.70.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.75.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.75.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.80.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.lt.80.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  f.bt.55.70.XGB = c(cosine.similarity(feaimp.model.long$FEMALE.bt55.70.XGB[1 : nvar], feaimp.model.long$FEMALE.ALL.XGB[1 : nvar])),
  m.40.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.40.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.45.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.45.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.50.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.50.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.55.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.55.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.60.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.60.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.65.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.65.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.70.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.70.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.75.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.75.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.80.XGB = c(cosine.similarity(feaimp.model.long$MALE.lt.80.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar])),
  m.bt.55.70.XGB = c(cosine.similarity(feaimp.model.long$MALE.bt55.70.XGB[1 : nvar], feaimp.model.long$MALE.ALL.XGB[1 : nvar]))
)
)
}
collect.cosines <- as.data.frame(list())
i <- 0
for (var in 24:12) {
  i <- i+1
  collect.cosines <- rbind(
    collect.cosines,
    get.cosine(var)
  )
  row.names(collect.cosines)[i] <- var
}

tmp <- subset(cosine.long, !age == '55-70')
tmp$age <- as.numeric(tmp$age)
tmp <- readxl::read_excel("~/Clare-Clustering/figuresCode/latest_figuresData.xlsx", 
                          sheet = "cosine.copy")
tmp$sex <- factor(tmp$sex)
par(mfrow = c(3,1))
  plot(maxstat.test(cosine.lt.vs.ALL ~ age,
                    data = subset(tmp, model == 'GLM' & sex == 'MALE')
  )) + title(
    main = 'GLM', xlab = NULL
  )
  
  plot(maxstat.test(cosine.lt.vs.ALL ~ age,
                    data = subset(tmp, model == 'RF' & sex == 'MALE')
  )) + title(
    main = 'RF', xlab = NULL
  )
  
  plot(maxstat.test(cosine.lt.vs.ALL ~ age,
                    data = subset(tmp, model == 'XGB' & sex == 'MALE')
  )) + title(
    main = 'XGB'
  )
dev.off()

par(mfrow = c(3,1))
  plot(maxstat.test(cosine.lt.vs.ALL ~ age,
                    data = subset(tmp, model == 'GLM' & sex == 'FEMALE'),
                    main = "GLM"
  ))
  
  plot(maxstat.test(cosine.lt.vs.ALL ~ age,
                    data = subset(tmp, model == 'RF' & sex == 'FEMALE')
  ))
  
  plot(maxstat.test(cosine.lt.vs.ALL ~ age,
                    data = subset(tmp, model == 'XGB' & sex == 'FEMALE')
  ))

#### final versions
tiff('fig.1B.lt_vs_ALL_modelbased.tiff', units = "in", width = 8, height = 4, res = 300, compression = 'jpeg')
fig.1B.lt_vs_ALL_modelbased <- ggplot(data = subset(na.omit(cosine.long), !(age == '55-70') & !(age == '45')), 
                                      aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  #geom_text(aes(label = round(cosine.lt.vs.ALL,2))
  #          , vjust = 2
  #          , check_overlap = TRUE, size = 3.5
  #) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  facet_wrap(model ~ .)+
  theme_classic() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  ) +
  geom_vline(xintercept = 2, linetype = 'dotted', color = 'blue', size = .5)
fig.1B.lt_vs_ALL_modelbased
dev.off()
tiff('fig.1B.lt_vs_ALL_modelbased_sampled.tiff', units = "in", width = 8, height = 4, res = 300, compression = 'jpeg')
fig.1B.lt_vs_ALL_modelbased_sampled  <- ggplot(data = subset(cosine.long, !(age == '55-70') & !(age == '45')), 
                                               aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  facet_wrap(model ~ .) +
  theme_classic() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  ) +
  geom_vline(xintercept = 6, linetype = 'dotted', color = 'blue', size = .5)
fig.1B.lt_vs_ALL_modelbased_sampled
dev.off()
## separate line for each facet
dummy <- cosine.long %>%
  mutate(
    intercept = ifelse(sex == "MALE" & (model == "GLM" | model == "RF"), "52",
                       ifelse(model == "XGB" & sex == "MALE", "51",
                              ifelse(sex == "FEMALE" & model == "GLM", "53",
                                     ifelse(sex == "FEMALE" & model == "RF", "52", "54")
                                     )
                              )
                       )
  )
ggplot(data = subset(dummy, !(age == '55-70') & !(age == '45')), 
       aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  geom_vline(aes(xintercept = intercept, color = sex), size = 1, linetype = "longdash") +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  scale_x_discrete('\n Age at Index Ischemic Stroke') +
  facet_wrap(model ~ .) +
  theme_classic() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  )
# try the zoom out features
test <- subset(dummy, !(age == '55-70'))
test$age <- as.numeric(as.character(test$age))
test$intercept <- as.numeric(as.character(test$intercept))
ggplot(data = subset(test, model == 'GLM'), 
       aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  facet_zoom(xlim = c(45, 55)) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 10)) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0.5, 1)) +
  theme_classic() +
  theme(
    legend.position = 'top'
    , axis.title = element_text(size = 12, color = 'black', family = 'serif')
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  )
p.glm <- ggplot(data = subset(test, model == 'GLM'), 
                aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  geom_vline(aes(xintercept = intercept, color = sex), size = .75, linetype = "dashed") +
  facet_zoom(xlim = c(45, 55)) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 10)) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  theme_classic() +
  theme(
    legend.position = 'top'
    , plot.title = element_text(hjust = 0.5,size = 10, color = 'black', family = 'serif', face = 'bold')
    , axis.title.y = element_text(size = 10, color = 'black', family = 'serif')
    , axis.title.x = element_blank() 
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  )  + 
  ggtitle("GLM")
p.rf <- ggplot(data = subset(test, model == 'RF'), 
                aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  geom_vline(aes(xintercept = intercept, color = sex), size = .75, linetype = "dashed") +
  facet_zoom(xlim = c(45, 55)) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 10)) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  theme_classic() +
  theme(
    legend.position = 'top'
    , plot.title = element_text(hjust = 0.5,size = 10, color = 'black', family = 'serif', face = 'bold')
    , axis.title = element_blank() 
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  )  + 
  ggtitle("RF")
p.XGB <- ggplot(data = subset(test, model == 'XGB'), 
                aes(x = age, y = cosine.lt.vs.ALL, group = sex, color = sex)) +
  geom_line(size = .75) +
  geom_vline(aes(xintercept = intercept, color = sex), size = .75, linetype = "dashed") +
  facet_zoom(xlim = c(45, 55)) +
  scale_x_continuous(breaks = function(x) pretty(x, n = 10)) +
  scale_y_continuous('Cosine Similarity Measure \n', limits = c(0, 1)) +
  theme_classic() +
  theme(
    legend.position = 'top'
    , plot.title = element_text(hjust = 0.5,size = 10, color = 'black', family = 'serif', face = 'bold')
    , axis.title = element_blank() 
    , axis.text = element_text(size = 8, color = 'black', family = 'serif')
    , legend.text = element_text(size = 10, color = 'black', family = 'serif')
    , strip.text = element_text(size = 10, color = 'black', family = 'serif', face = 'bold')
    , legend.title = element_blank()
    , panel.spacing = unit(0.2, 'lines')
    , panel.border = element_rect(colour="black",size=.01, fill = NA)
  )  + 
  ggtitle("XGB")
p <- cowplot::plot_grid(
    p.glm + theme(legend.position = "none")
  , p.rf + theme(legend.position = "none")
  , p.XGB + theme(legend.position = "none")
  , labels = c("A", "B", "C")
  , nrow = 1)
legend_b <- get_legend(
  p.glm +  #guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "top"
          , legend.text = element_text(size = 10, color = 'black', family = 'serif', face = "bold"))
)
tiff('fig.1.tiff', units = "in", width = 10, height = 6, res = 300, compression = 'jpeg')
ggdraw(
  add_sub(
    plot_grid(legend_b, p, ncol = 1, rel_heights = c(.05, 1)
    )
    , "Age at Index Ischemic Stroke", fontfamily = "serif", size = 10, color = "black"
    , vpadding=grid::unit(0,"lines"),y= 4.75, x=0.5, vjust=4.5)
)
dev.off()
