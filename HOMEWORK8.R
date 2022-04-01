#HOMEWORK8
library(tidyverse)
library(ggplot2)

#Question1
setwd("C:/Users/MCT40/OneDrive - University of Pittsburgh/Classes/Spring2022/CMU_STATS/HOMEWORK")

d1 <- read.csv("hcp_data/unrestricted_trimmed_1_7_2020_10_50_44.csv")

d1 %>% 
    select(Subject, Gender, Flanker_Unadj, FS_IntraCranial_Vol, FS_Tot_WM_Vol, FS_Total_GM_Vol) %>%
    drop_na() -> d2

any(d2 == "n/a")

head(d2)

# 1b)
ggplot(d2, aes(FS_Total_GM_Vol, FS_IntraCranial_Vol, colour=Gender)) +
  geom_point() +
  theme_bw()

# 2
d2$Gender <- as.numeric(as.factor(d2$Gender))
Log_reg_hcp <- glm(as.factor(Gender) ~ FS_Tot_WM_Vol*FS_Total_GM_Vol*FS_IntraCranial_Vol, data= d2, family = binomial)
summary(Log_reg_hcp)

# 2b)
threshold = 0.50

d2_predict = data.frame(predict(Log_reg_hcp, type = "response"))

## Rename the predicted values generated above to "predicted_prob" for ease of interpretability
colnames(d2_predict) = c('predicted_prob')
head(d2_predict)

# Make a list of "Incorrect" responses (coded as 0)
num_obs = nrow(d2)
d2_predict$predicted_binary=rep(1,num_obs)
d2_predict$predicted_binary[d2_predict$predicted_prob>threshold]= 2

# Look at the prediction accuracy in form of confusion matrix table
d2_conf = data.frame(d2_predict$predicted_binary, as.factor(d2$Gender))
colnames(d2_conf) = c('predicted', 'actual')
table(d2_conf)

## Abbreviated summary: 
## 522 predicted 0 actual F (true neg)
## 115 predicted 0 actual M (false neg)
## 84  predicted 1 actual F (false pos)
## 392 predicted 1 actual M (true pos)

## Calculate the general accuracy of the model's predictions: 
print(paste("Accuracy:",mean(d2_conf$predicted == d2_conf$actual)))


# 3 Boostraped accuracy
# install.packages("boot")
# install.packages("ISLR")
library(ISLR)
library(boot)
set.seed(33)

#write boostrapping function
boot.fn <- function(data, index){  
  threshold = 0.50
  
  d2_predict = data.frame(predict(Log_reg_hcp, type = "response"))
  
  ## Rename the predicted values generated above to "predicted_prob" for ease of interpretability
  colnames(d2_predict) = c('predicted_prob')
  head(d2_predict)
  
  # Make a list of "Incorrect" responses (coded as 0)
  num_obs = nrow(d2)
  d2_predict$predicted_binary=rep(1,num_obs)
  d2_predict$predicted_binary[d2_predict$predicted_prob>threshold]= 2
  
  # Look at the prediction accuracy in form of confusion matrix table
  d2_conf = data.frame(d2_predict$predicted_binary, as.factor(d2$Gender))
  colnames(d2_conf) = c('predicted', 'actual')
  table(d2_conf)
  
  ## Abbreviated summary: 
  ## 522 predicted 0 actual F (true neg)
  ## 115 predicted 0 actual M (false neg)
  ## 84  predicted 1 actual F (false pos)
  ## 392 predicted 1 actual M (true pos)
  
  ## Calculate the general accuracy of the model's predictions: 
  #print(paste("Accuracy:",mean(d2_conf$predicted == d2_conf$actual)))
  
  return(paste("Accuracy:",mean(d2_conf$predicted == d2_conf$actual)))
  }

#run bootstrapping function
print(boot.fn(d2, 1:1113))

##Run it with random resampling: 
boot_obj = boot(d2,boot.fn ,R=1000) #R=repetitions 
print(boot_obj[["t"]]) 


#hist(boot(d2,boot.fn ,R=1000)$t[,2], xlab="Horsepower coefficient") #we get a distribution of all of the estimates
#                               ^^ indexing the second "t" value



# 4 Permutation test
# Now run a permutation test, with 1000 iterations, to evaluate how much 
# grey matter volume contributes to the prediction accuracy. Compare the 
# prediction accuracy of the full (unpermuted model) with the distribution of 
# accuracies you get with a randomized grey matter volume term using a histogram 
# (Hint: use the abline function to show the original accuracy on the histogram).

perm_d2 = d2

R =1000

perm.coefs=matrix(NA,nrow=R, ncol=2) #

for (i in 1:R){
  perm_d2$XXX = d2$XXXr[sample(392)] # This is a shuffled version of the Auto$horsepower vector
  perm.acc[i,]= XXXaccuracy ###
}

# Take a look at the null distributions
hist(perm.acc[,2])


# Now re-estimate the real (unpermuted) effect 
perm.real = XXXaccuracy
perm.real
