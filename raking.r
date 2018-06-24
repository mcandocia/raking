# scraping
library(plyr)
library(dplyr)
library(scales)
library(reshape2)
library(ggplot2)

# will make sure a list's elements match the levels of
# the data it corresponds to
reorder_list <- function(x, reference_data){
  new_list = list()
  data_levels = levels(reference_data)
  for (level in levels(as.factor(reference_data)))
    new_list[[level]] = x[[level]]
  return(new_list)
}

# calcualtes weights for data based on selected variables and their "true" margins
rake_data <- function(data, variables, true_or_estimated_margins, max_iter=50, truncate_extreme_weights=TRUE){
  weights = rep(1, nrow(data))
  
  # calculate the sum and proportions of each variable + level in advance
  # and make sure that order matches the same order in the data
  total_margins = list()
  for (variable in variables){
    original_margins = true_or_estimated_margins[[variable]]
    reordered_margins = reorder_list(original_margins, data[,variable])
    
    total_margin =  sum(unlist(reordered_margins[[variable]]))
    total_margins[[variable]] = total_margin
    for (level in names(true_or_estimated_margins[[variable]]))
      reordered_margins[[variable]][[level]] = 
          reordered_margins[[variable]][[level]]/total_margin
  }
  
  # create design matrices (columns of 1s and 0s in this case) for faster calculations
  design_matrices = list()
  for (variable in variables){
    # create model matrix with 0-intercept, which removes the concept of "reference variable"
    design_matrices[[variable]] = as.data.frame(model.matrix(~.+0, data=data[,variable,drop=FALSE]))
    # remove variable name from column name so only level remains
    colnames(design_matrices[[variable]]) = substr(colnames(design_matrices), 1, nchar(variable)) 
  }
  
  # perform raking
  for (i in 1:max_iter){
    for (variable in variables){
      weighted_margins = colSums(weights * design_matrices[[variable]])
      level_weight_modifications = unlist(true_or_estimated_margins[[variable]])/weighted_margins
      
      # this multiplies each column of the design matrices by the corresponding weight change factor
      # then each column is multiplied by the weights, and the rows are added up, since each row only
      # has one non-zero value
      weights = rowSums(weights * mapply(`*`, design_matrices[[variable]], level_weight_modifications))
    }
  }
  
  # limits extreme weights to median plus 6 times inter-quartile range
  # IQR = difference between 75th percentile and 25th percentile of data
  if (truncate_extreme_weights){
    weight_threshold = median(weights) + 6*IQR(weights)
    weights = pmin(weights, weight_threshold)
  }
  #normalize to population size
  weights = weights*length(weights)/(sum(weights))
  
  return(weights)
}

# formula in http://www.analyticalgroup.com/download/WEIGHTED_MEAN.pdf
weighted.var.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  x_weighted_mean = sum(x*w)/sum(w)
  sum(w*(x-x_weighted_mean)^2)/(sum(w)-1)
  
}

weighted.var.sigma <- function(x, w){
  root_b = sqrt(sum(w)^2/sum(w^2))
  weighted.var.se(x,w)/root_b
}



## EXAMPLE
set.seed(525600)
size_1 = 200
example_data = data.frame(
  gender=c("M","F")[1+rbinom(size_1, 1, 0.7)],
  race=sample(c('White','Black','Other'), size_1, replace=TRUE, prob=c(0.5,0.4,0.1))
)

actual_margins=list(gender=list('M'=0.5,'F'=0.5), race=list('White'=0.7, 'Black'=0.15, 'Other'=0.15))

survey_weights = rake_data(example_data, c('gender','race'), actual_margins)

example_data = example_data %>% mutate(random_score = 2+rnorm(size_1)/3 + 
                                         0.2*(gender=='F') - 
                                         0.25 * (race=='Other') + 
                                         0.35 * (race=='Black')
                                       )

# table(example_data$gender, example_data$race)
# table( survey_weights,example_data$gender, example_data$race)

#lm(random_score ~ race + gender, data=example_data)
#lm(random_score ~ race + gender, data=example_data, weights=survey_weights)


## bigger sample
set.seed(525600)
example_data_longer = data.frame(
  gender=c("M","F")[1+rbinom(2000, 1, 0.7)],
  race=sample(c('White','Black','Other'), 2000, replace=TRUE, prob=c(0.5,0.4,0.1))
)

actual_margins=list(gender=list('M'=0.5,'F'=0.5), race=list('White'=0.7, 'Black'=0.15, 'Other'=0.15))

survey_weights_longer = rake_data(example_data_longer, c('gender','race'), actual_margins)

example_data_longer = example_data_longer %>% mutate(random_score = rnorm(2000) + 
                                         0.2*(gender=='F') - 
                                         0.25 * (race=='Other') + 
                                         0.35 * (race=='Black')
)

# table(example_data_longer$gender, example_data_longer$race)
# table( survey_weights_longer,example_data_longer$gender, example_data_longer$race)

#lm(random_score ~ race + gender, data=example_data_longer)
#lm(random_score ~ race + gender, data=example_data_longer, weights=survey_weights_longer)





example_data_summary = rbind(
  example_data %>% summarize(
  score_mean = mean(random_score),
  score_error = sd(random_score),
  uses_weighting='Unweighted',
  mean_estimate_sigma=sd(random_score)/sqrt(length(random_score))
  ),
  example_data %>% summarize(
    score_mean=sum(survey_weights*random_score)/sum(survey_weights),
    score_error=sqrt(weighted.var.se(random_score, survey_weights)),
    uses_weighting='Weighted',
    mean_estimate_sigma=weighted.var.sigma(random_score, survey_weights)
  )
) %>%
  mutate(uses_weighting=factor(uses_weighting))


print(example_data_summary)

ggplot(example_data_summary) + geom_bar(aes(x=uses_weighting, y=score_mean), stat='identity') +
  geom_rect(aes(xmin=as.numeric(uses_weighting)-0.45, 
                xmax=as.numeric(uses_weighting)+0.45,
                ymin=score_mean-1.96*score_error, 
                ymax=score_mean+1.96*score_error),
            fill='#EE555599', color='#00000000') +
  # plot data alongside bars
  geom_point(data=example_data, aes(x='Unweighted', y=random_score)) + 
  geom_point(data=example_data, aes(x='Weighted', y=random_score))

ggplot(example_data_summary) + geom_bar(aes(x=uses_weighting, y=score_mean), stat='identity') +
  geom_rect(aes(xmin=as.numeric(uses_weighting)-0.45, 
                xmax=as.numeric(uses_weighting)+0.45,
                ymin=score_mean-1.96*mean_estimate_sigma, 
                ymax=score_mean+1.96*mean_estimate_sigma),
            fill='#EE555599', color='#00000000') +
  # plot data alongside bars
  geom_point(data=example_data, aes(x='Unweighted', y=random_score)) + 
  geom_point(data=example_data, aes(x='Weighted', y=random_score))
