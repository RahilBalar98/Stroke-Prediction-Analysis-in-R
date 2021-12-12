data = read.csv("/home/rahilbalar98/Documents/STAT_652/Project3/healthcare-dataset-stroke-data.csv", header = TRUE)

# Data
head(data)

# Dimension 
dim(data)

# any missing values
sum(is.na(data))

summary(data)

# Converting data type from string to numeric
data <- data[-which(data$bmi=="N/A"),]
data$bmi <- as.numeric(as.character(data$bmi))
dim(data)

data$heart_disease <- factor(data$heart_disease, levels = c(0,1), labels =c('No','Yes'))
data$hypertension <- factor(data$hypertension,levels = c(0,1), labels = c('No','Yes'))

cor(data[,c(9,10)])

hist(data$avg_glucose_level,xlab = 'Glucose level')
hist(data$bmi,xlab = 'BMI')

data_scaled <- data
data_scaled$avg_glucose_level <- scale(data_scaled$avg_glucose_level, scale = TRUE, center = TRUE)
data_scaled$bmi <- scale(data_scaled$bmi, scale = TRUE, center = TRUE)

library(bestglm)
library(ROCR)

R = 10
output = matrix(NA,10,3) 
colnames(output) = c("Training accuracy", "Validation accuracy", "Testing accuracy")
N = nrow(data_scaled)

for(r in 1:R){  
  
  train = sample(1:N, size = 0.70*N) 
  val = sample( setdiff(1:N, train), size = 0.15*N )
  
  test = setdiff(1:N, union(train, val))
  moddata = data_scaled[,1:11]
  
  # Logistic regression 
  fitlog = glm(stroke ~ ., data = data_scaled[train,],family = "binomial")
  predObj = prediction(fitted(fitlog),data_scaled$stroke[train])
  sens = performance(predObj,"sens")
  spec = performance(predObj,"spec")
  tau = sens@x.values[[1]]
  sensSpec = sens@y.values[[1]]+spec@y.values[[1]] 
  best = which.max(sensSpec)
  tau[best]
  
  predtrian = ifelse(predict(fitlog,newdata=moddata[train,]) > tau[best],1,0)
  logtrain = table(data_scaled$stroke[train],predtrian) 
  acctrain = sum(diag(logtrain))/sum(logtrain)
  
  predval = ifelse(predict(fitlog,newdata=moddata[val,]) > tau[best],1,0)
  logval = table(data_scaled$stroke[val],predval) 
  accval = sum(diag(logval))/sum(logval)
  
  predtest = ifelse(predict(fitlog,newdata=moddata[test,]) > tau[best],1,0)
  logtest = table(data_scaled$stroke[test],predtest)
  acctest = sum(diag(logtest))/sum(logtest)
  
  output[r,1] = acctrain
  output[r,2] = accval
  output[r,3] = acctest
}
