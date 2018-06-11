library(randomForest)
library(caret)

library(dplyr)

setwd("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
source("helper_functions.R")

data = fetchAndCleanData()
data_fac=data %>% mutate_if(is.character, as.factor)

sample.ind = sample(2,  nrow(data), replace = T, prob = c(0.75,0.25))
train = data[sample.ind==1,]  
test = data[sample.ind==2,]  

plotdata = train
p = ggplot(plotdata,aes(x=StalkColorBelowRing,  y=StalkColorAboveRing, color=Edible))
p + geom_jitter(alpha=0.3) +  scale_color_manual(breaks = c('Edible','Poisonous'),   values=c('darkgreen','red'))


p = ggplot(plotdata,aes(x=Odor,  y=SporePrintColor, color=Edible))
p + geom_jitter(alpha=0.3) +  scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))


p = ggplot(plotdata,aes(x=Edible,  y=Odor,  color = Edible))
p + geom_jitter(alpha=0.2) +  scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))

p = ggplot(plotdata,aes(x=Edible,  y=SporePrintColor, color = Edible))
p + geom_jitter(alpha=0.2) +  scale_color_manual(breaks = c('Edible','Poisonous'), values=c('darkgreen','red'))

rf = randomForest(Edible ~ .,  ntree = 100, data = train)
plot(rf)  
print(rf) 

# Variable Importance
varImpPlot(rf,sort = T,n.var=10, main="Top 10 - Variable Importance")

#Variable Importance
var.imp = data.frame(importance(rf,  type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
test$predicted.response = predict(rf , test)
# Create Confusion Matrix
print(confusionMatrix(data = test$predicted.response,  
                      reference = test$Edible,
                      positive = 'Edible'))
