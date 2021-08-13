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
cosine.long <- read_excel("~/Clare-Clustering/figuresCode/figuresData.xlsx"
                          , sheet = 'cosine'
                          , na=c("NA")
                          , col_names = TRUE) 
cosine.long$age <- factor(cosine.long$age)
cosine.long$sex <- factor(cosine.long$sex)
# match colors with fig.2 for SEX
unique(ggplot_build(fig.2)$data[[1]]["fill"])
fig.1 <- ggplot(data = cosine.long, aes(x = age, y = score, group = sex, color = sex)) +
  geom_line(size = 1.75) + 
  geom_point(size = 5, alpha = .75)+
  geom_text(aes(label = round(score,2)), hjust = 1, vjust = 2, check_overlap = TRUE,
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
#-----------
# Figure 2: Results
#----------
# try Box plots instead of line plots; 4 models into one box 
# include only less than & ALL criteria
# try colors for FEAMLE and MALE
results.wide <- read_excel("~/Clare-Clustering/figuresCode/figuresData.xlsx"
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
ggplot(results.long, 
       aes(x = CRITERIA, y = value, group = MODEL, color = MODEL)) +
  geom_line(aes(linetype = MODEL), size = 1) +
  facet_wrap(variable~SEX, scales = 'free_y') +
  theme(
    legend.position = 'top'
  ) +
  scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'dotdash'))
# finalized
tmp <- subset(results.long, variable == c('AUROC', 'ACCURACY'))
tmp <- unique(tmp[,c('variable','SEX')])
tmp$CRITERIA <- tmp$value <- 1
fig.2 <- ggplot(subset(results.long, variable == c('AUROC', 'ACCURACY')), 
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
#-----------
# Figure 3: Feature Importance
#----------
#  include only less than & ALL criteria
feaimp.wide <- read_excel("~/Clare-Clustering/figuresCode/figuresData.xlsx"
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
feaimp.long <- read_excel("~/Clare-Clustering/figuresCode/figuresData.xlsx"
                          , sheet = 'feaimp.long'
                          , na=c("NA")
                          , col_names = TRUE)
colnames(feaimp.long) <- c("SEX","CRITERIA","variable",'abbr',"Feature Importance\nScore")
feaimp.long$abbr <- trimws(feaimp.long$abbr, which = c('both'))
feaimp.long$abbr <- factor(feaimp.long$abbr, levels = c(
  'Alcohol Dependance or Abuse',
  'Atrial Fibrillation / Flutter',
  'Body Mass Index',
  'Cervical Artery Dissection',
  'Chronic Systemic Disorders',
  'Congestive Heart Failure',
  'Diabetes Mellitus',
  'Drug Dependance or Abuse',
  'Dyslipidemia',
  'Family History of Stroke',
  'Giant Cell Temporal Arteritis',
  'Hypercoagulable States',
  'Hypertension',
  'Migraine',
  'Myocardial Infarction',
  'Non-Central Nervous System Neoplasm',
  'Other Arteriopathies',
  'Patent Foramen Ovale',
  'Peripheral Vascular Disease',
  'Rheumatic Diseases',
  'Smoking Status'
))

# create a data frame with faceting variables to add background colors for Facets
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
        , axis.text = element_text(size = 12, color = 'black', family = 'serif')
        , legend.text = element_text(size = 12, color = 'black', family = 'serif')
        , strip.text = element_text(size = 12, color = 'black', family = 'serif')
        , legend.title = element_text(size = 12, color = 'black', family = 'serif')
        ) +
  guides(color = guide_legend(title.vjust = .5))
tiff('fig.3.tiff', units = "in", width = 16, height = 8, res = 300, compression = 'jpeg')
fig.3
dev.off()
