library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gplots)
library(ggpubr)
library(tidyverse)
options(scipen = 999)
options(readr.default_locale=readr::locale(tz="US/Eastern"))


#-----------
# Figure 1: Cosine
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
fig.2.data <- subset(results.long, variable == c('AUROC', 'ACCURACY'))
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
  'Anxiety Disorder',
  'Atrial Fibrillation / Flutter',
  'Body Mass Index',
  'Brain Tumor',
  'Chronic Systemic Disorders',
  'Congestive Heart Failure',
  'Conversion Disorder',
  'Convulsions',
  'Depression',
  'Diabetes Mellitus',
  'Drug Dependance or Abuse',
  'Dyslipidemia',
  'Epilepsy',
  'Family History of Stroke',
  'Hypercoagulable States',
  'Hypertension',
  'Manic Bipolor Disorder',
  'Migraine',
  'Myocardial Infarction',
  'Neoplasm',
  'Other Arteriopathies',
  'Patent Foramen Ovale',
  'Peripheral Neuropathy',
  'Peripheral Vascular Disease',
  'Rheumatic Diseases',
  'Smoking Status',
  'Syncope'
))

# create a data frame with facting variables
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
