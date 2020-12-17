# Extract feature importance
feaImp <- data.frame(features = character(0))                                               # create empty data frame with a column to append data iteratively
for (i in 1:length(ml.train.v2)) {
  ifelse(
    # condition: Check for 'pattern'(glm|rf|xgb) in the 'string'(female.age.lt.40.glm)
    # use 'paste' to check for multiple patterns in the 'string'
    grepl(paste(c('glm','rf','xgb'), collapse = '|'),                           
          names(ml.train.v2[i])),                                               
    # if TRUE then do
    feaImp <- full_join(feaImp, 
                        varImp(ml.train.v2[[i]])$importance%>%              
                          mutate(features = rownames(.),
                                 model = names(ml.train.v2[i]))%>%
                          select(features, model, Overall)%>%
                          arrange(model, features)%>%
                          rename(!!quo_name(names(ml.train.v2[i])) := Overall),              # dynamically rename the default 'Overall' column
                        by = c('features')
    )%>%
      select(-starts_with('model'))%>%
      replace(is.na(.), 0) , 
    # Else, then do
    print("it's SVM, ignore!!")                                               
  )
}
