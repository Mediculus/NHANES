library(rnhanesdata)
library(tidyverse)

## activity data

clean_loader <- function(datain) {
  
  as_tibble(datain) %>% 
    janitor::clean_names() %>% 
    mutate(across(c("seqn", "paxcal", "paxstat", "weekday", "sddsrvyr"), factor))
  
}

#load activity and flag data
actC <- clean_loader(PAXINTEN_C)
actD <- clean_loader(PAXINTEN_D)

flagC <- clean_loader(Flags_C)
flagD <- clean_loader(Flags_D)

#make activity = 0 if flag = 0 (non-wear indicator)
col_vars = paste0("min",1:1440)
actC[, col_vars] <- actC[, col_vars]*flagC[, col_vars]
actD[, col_vars] <- actD[, col_vars]*flagD[, col_vars]

variable_list <- read_csv("./Datasets/variable_list.csv")

## default covariate from rnhanesdata package
covC <- as_tibble(Covariate_C) %>% janitor::clean_names()
covD <- as_tibble(Covariate_D) %>% janitor::clean_names()

## get additional covariates
## variables of interest were selected that are likely to be associated with diabetes; this was done by manually parsing through CDC's NHANES codebook

extra_covariates <- c(# Demographics
                      "INDHHINC", "INDFMPIR", "RIDEXPRG",
                      # Exam: HR, pulse, BP
                      "BPXPLS", "BPXPULS", "BPXSY1","BPXSY2","BPXSY3", "BPXSY4", "BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4",
                      # Exam: Body measures
                      "BMXWT", "BMXHEAD", "BMXHT", "BMXWAIST",
                      # Exam: CV fitness
                      "CVDFITLV",
                      # Labs: total cholesterol and direct HDL (xhdd wave c, hdd wave d) in mg/dL
                      "LBXTC","LBXHDD","LBDHDD",
                      # Labs: plasma fasting glucose mg/dL
                      "LBXGLU",
                      # Questionnaire: Age dx'ed with diabetes (only entered if marked yes on diabetes)
                      "DID040G"
                      )


extra_data <- process_covar(varnames = extra_covariates)

extra_covC <- extra_data$Covariate_C %>% janitor::clean_names()
extra_covD <- extra_data$Covariate_D %>% janitor::clean_names()

covD %>% count(diabetes)


cov_clean <- bind_rows(cov_c, cov_d) %>% 
  janitor::clean_names() %>% 
  select(-bmi_cat, -sdmvpsu, -sdmvstra, -wtint2yr, -wtmec2yr) %>% 
  drop_na() %>% 
  filter(across(where(is.factor), ~ !(. %in% ("Don't know")))) %>% 
  mutate(diabetes = fct_collapse(diabetes, Yes = c("Yes", "Borderline"))) %>% 
  mutate(across(where(is.factor), fct_drop))

cov_clean <- cov_clean %>% select(-sddsrvyr, -seqn, -ridagemn, -ridageex)

cov_d_clean2 <- cov_d %>% 
  janitor::clean_names() %>% 
  select(-bmi_cat) %>% 
  drop_na() %>% 
  filter(across(where(is.factor), ~ !(. %in% ("Don't know")))) %>% 
  mutate(diabetes = fct_collapse(diabetes, Yes = c("Yes", "Borderline"))) %>% 
  mutate(across(where(is.factor), fct_drop))

skimr::skim(cov_d_clean2)


# Modeling ----

library(caret)
set.seed(1)

#LASSO controls & grid
lasso_control <- trainControl(method = "repeatedcv",
                              number = 5, repeats = 5,
                              search = "random")


# bootstrapped data partitioning to train and test
train_index <- createDataPartition(cov_clean$diabetes, p = 0.8, list = FALSE, times = 1)

train_df <- cov_clean[train_index,]
test_df <- cov_clean[-train_index,]

test_df %>% count(diabetes)

# model fit
lasso_fit <- train(diabetes ~ ., data = train_df,
                   method = "glmnet",
                   trControl = lasso_control,
                   family = "binomial")

print(lasso_fit)

varImp(lasso_fit)

lasso_pred <- predict(lasso_fit, test_df)
# compare predicted outcome and true outcome
confusionMatrix(lasso_pred, test_df$diabetes)


coef(lasso_fit$finalModel, lasso_fit$bestTune$lambda)



# Random Forest
rf_control <- trainControl(method = "repeatedcv",
                           number = 5, repeats = 5)

rf_grid <- expand.grid(mtry = 1:5)

rf_fit <- train(diabetes ~ ., data = train_df,
                method = "rf",
                trControl = rf_control, 
                tuneGrid = rf_grid,
                ntree = 200)

varImp(rf_fit)

print(rf_fit)

rf_pred <- predict(rf_fit, test_df)
# compare predicted outcome and true outcome
confusionMatrix(rf_pred, test_df$diabetes)

coef(rf_fit$finalModel, rf_fit$bestTune$mtry)
