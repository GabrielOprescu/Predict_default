library(caret)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(xgboost)


train = read.csv('application_train.csv', header = TRUE, stringsAsFactors = FALSE)
ads

# scoatere na / transformare caracter in numeric --------------------------

lapply(train, class) %>% unlist() %>% data.frame()

lapply(train, function(x) {if(is.character(x)) length(unique(x))}) %>% 
  unlist() %>% 
  as.data.frame() 

tr = train %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
  mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>%
  mutate_if(is.character, funs(replace(., is.na(.), 'Blank'))) 
  # mutate_if(is.character, funs(factor(.) %>% as.integer()))

lapply(tr, class) %>% unlist() %>% data.frame()



# centrare si scalare -----------------------------------------------------

# preproc_scale = preProcess(tr[ ,c(-1,-2)], method = c('center', 'scale'))
# tr = predict(preproc, tr[ ,c(-1,-2)])
# 
# tr = cbind(train[, c(1,2)], tr)


# dummy vars --------------------------------------------------------------

preproc_dummy = dummyVars( ~., data = tr[ ,c(-1,-2)])
tr = predict(preproc_dummy, newdata = tr[ ,c(-1,-2)])
tr = cbind(train[, c(1,2)], tr)


x = tr %>% 
  select(-SK_ID_CURR, -TARGET)  %>% 
  data.matrix()

y = tr %>% 
  select(TARGET)  %>% 
  data.matrix()

save(x, y, file = 'date_prelucrate.rda')


lm_model = lm(TARGET ~., data = tr[, -1])

summary(lm_model)

step(lm_model, direction = 'backward', steps = 500)
