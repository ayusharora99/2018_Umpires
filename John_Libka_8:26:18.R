#8/26/18: Giants vs  : John Libka

#136 pitches were called strikes/balls

#The robot-ump called 51 of those pitches as called strikes & 85 as balls

#Brian Gorman called 50 of those pitches as called strikes & 86 as balls 

#Accuracy: 92%

John_Libka <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/John_Libka_8:26:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(John_Libka)
names(John_Libka)
is.na(John_Libka)
colSums(is.na(John_Libka)) 
John_Libka = John_Libka[,colSums(is.na(John_Libka)) == 0] 
dim(John_Libka)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
John_Libka = John_Libka[ , !(names(John_Libka) %in% drops)]
# John_Libka = John_Libka[keep]
dim(John_Libka)
John_Libka

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
John_Libka_train = John_Libka[0:(0.8 * nrow(John_Libka)),]
dim(John_Libka_train)
prop.table(table(John_Libka_train$type))
John_Libka_test = John_Libka[(0.8*nrow(John_Libka)):nrow(John_Libka),]
dim(John_Libka_test)
prop.table(table(John_Libka_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = John_Libka_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_JL<-predict(tree_model, newdata = John_Libka_test, type = 'class')

confusionMatrix(table(prediction_JL, John_Libka_test$type))
confusionMatrix(prediction_JL,John_Libka_test$type)
# 78.57% Accuracy, 82.35% Precision, 82.35% Recall w/ Keeps
# 100% in ALL w/ Drops
#sensitivity, specificity which is more important: 1.0000, 1.0000


#  Accuracy, precision, recall,sensitivity, specificity, ROC, AUC, Kappa, F score, LogLoss etc
# Run ROC for imbalanced data

# Kmeans Clustering
# https://www.youtube.com/watch?v=4R8nWDh-wA0&index=5&list=PLaozD487rOR_LgH8ObhA2-ns2ijiB-uaf 
# Hierarichal Clustering?

# Check for overfitting & underfitting

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls

# Seperate Ball & Strike Types
John_Libka_Strikes = subset(John_Libka, John_Libka$type == "S")
John_Libka_Balls = subset(John_Libka, John_Libka$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
John_Libka_Strikes$AdjustedCall = ifelse((John_Libka_Strikes$plate_x < 0.833 & John_Libka_Strikes$plate_x > -0.833) & (John_Libka_Strikes$plate_z > John_Libka_Strikes$sz_bot & John_Libka_Strikes$plate_z < John_Libka_Strikes$sz_top), 'S', 'B')
table(John_Libka_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
John_Libka_Balls$AdjustedCall = ifelse((John_Libka_Balls$plate_x > 0.833 | John_Libka_Balls$plate_x < -0.833)|(John_Libka_Balls$plate_z < John_Libka_Balls$sz_bot | John_Libka_Balls$plate_z > John_Libka_Balls$sz_top),'B','S')
table(John_Libka_Balls$AdjustedCall)
# Merge to create new dataset
John_Libka_AdjustedCalls = rbind(John_Libka_Strikes,John_Libka_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = John_Libka)
plot(tree_model)
text(tree_model, use.n = T)
prediction_JL<-predict(tree_model, newdata = John_Libka_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_JL,John_Libka_AdjustedCalls$AdjustedCall))

# John_Libka Strike Zone: 89.11% Accurate, 94% Precision, 90% Recall

