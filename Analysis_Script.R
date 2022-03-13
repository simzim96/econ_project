library(haven) # read stata file
library(glmnet) # LASSO regression
library(ggplot2) # ploting
set.seed(1234)

# Data preparation
# read data
data<- read_dta(file = "/Users/simon/Documents/Uni/Statistic/Semester 4/Econ Projects/Analysis/SCF_plus.dta")
fpg <- read.csv2("/Users/simon/Documents/Uni/Statistic/Semester 4/Econ Projects/Analysis/fpg.csv")

# assign households to income groups
data$Persons.in.family.household <- data$adults+data$children
data$Persons.in.family.household <- ifelse(data$Persons.in.family.household == 0,1,data$Persons.in.family.household )
data <- merge(x = data, y = fpg, by = "Persons.in.family.household", all.x = TRUE)
data$high <- ifelse(data$Poverty.guideline*5 < data$tinc, 1, 0 )

# get relevant variables
cols <- c('tinc','hdebt','prepaid','oestdebt','life','moneymarketacc','bnd','ofin','pen','onfin',
          'collegeh','raceh','blackh','agehgroup','ageh','adults','children','pdebt',
          'ccdebt','oest','vehi','ffabus','house','incws','incwsse','inctrans',
          'inccap','mfun','cerde','savbnd','liq','ffaequ', 'ffaass','high','ffanfin')
data_sub <- data.frame(data)[cols]

# descriptive statistics 
# histogram for income groups
ggplot(data, aes(log(tinc), fill = as.factor(high))) +  
  geom_histogram(bins = 200) +
  labs(x = "log(total income)", y = "frequency", fill = "Income Group",title="Histogram for log(total income)")+scale_fill_discrete(
    limits = c(0, 1),
    labels = c("Low", "High"))
# define post double selection estimator
double_lasso <- function(data, target, treatment){
  # check that all relevant columns are in the model
  cols <- c('tinc','hdebt','prepaid','oestdebt','life','moneymarketacc','bnd','ofin','pen','onfin',
            'collegeh','raceh','blackh','agehgroup','ageh','adults','children','pdebt',
            'ccdebt','oest','vehi','ffabus','house','incws','incwsse','inctrans',
            'inccap','mfun','cerde','savbnd','liq','ffaequ', 'ffaass','high','ffanfin')
  data <- data.frame(data)[cols]
  
  
  # mean imputation could also be in the data prep section, but here it makes sure that the algorithm does not crash
  for(i in 1:ncol(data)) {
    data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
  }
  
  
  for(i in 1:ncol(data)) {
    data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
  }
  
  # prepair data for the LASSO regression stage 1
  y <- as.matrix(data[target]) 
  X <- data
  X[target] <- NULL
  X[treatment] <- NULL
  # LASSO regression
  mod_cv <- cv.glmnet(x=as.matrix(X), y=y, family='gaussian',intercept = F, alpha=1)
  # extract coefficients
  tmp_coeffs <- coef(mod_cv, s = "lambda.min")
  selected_names_y <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  
  # prepair data for the LASSO regression stage 2
  y <- as.matrix(data[treatment])
  X <- data
  X[treatment] <- NULL
  X[target] <- NULL
  mod_cv <- cv.glmnet(x=as.matrix(X), y=y, family='gaussian',intercept = F, alpha=1)
  tmp_coeffs <- coef(mod_cv, s = "lambda.min")
  selected_names_treat <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  
  # combine selected names
  all_names <- union(selected_names_treat$name,selected_names_y$name)
  all_names <- union(all_names,treatment)
  
  fm = paste(paste0(target," ~"), paste(all_names, collapse="+"))
  fm = as.formula(fm)
  
  # post LASSO
  result_pl <- lm(fm, data[c(all_names,target)])
  
  results <- list(result_pl=result_pl,
                  selected_names_treat=selected_names_treat, 
                  selected_names_y=selected_names_y
  )
  return(results)
}

# split data
data_high <- data_sub[data_sub$high==1,]
data_low <- data_sub[data_sub$high==0,]

# estimate models
model_results_high <- double_lasso(data_high, 'tinc', 'pdebt')
model_results_low <- double_lasso(data_low, 'tinc', 'pdebt')

