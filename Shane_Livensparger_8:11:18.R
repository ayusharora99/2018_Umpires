#8/11/18: Giants vs Pirates : Shane Livensparger

#127 pitches were called strikes/balls

#The robot-ump called 41 of those pitches as called strikes & 84 as balls

#Shane Livensparger called 45 of those pitches as called strikes & 80 as balls 

#Accuracy: 92%

Shane_Livensparger <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Shane_Livensparger_8:11:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Shane_Livensparger)
names(Shane_Livensparger)
is.na(Shane_Livensparger)
colSums(is.na(Shane_Livensparger)) 
Shane_Livensparger = Shane_Livensparger[,colSums(is.na(Shane_Livensparger)) == 0] 
dim(Shane_Livensparger)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Shane_Livensparger = Shane_Livensparger[ , !(names(Shane_Livensparger) %in% drops)]
# Shane_Livensparger = Shane_Livensparger[keep]
dim(Shane_Livensparger)
Shane_Livensparger

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Shane_Livensparger_train = Shane_Livensparger[0:(0.8 * nrow(Shane_Livensparger)),]
dim(Shane_Livensparger_train)
prop.table(table(Shane_Livensparger_train$type))
Shane_Livensparger_test = Shane_Livensparger[(0.8*nrow(Shane_Livensparger)):nrow(Shane_Livensparger),]
dim(Shane_Livensparger_test)
prop.table(table(Shane_Livensparger_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Shane_Livensparger_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_SL<-predict(tree_model, newdata = Shane_Livensparger_test, type = 'class')

confusionMatrix(table(prediction_SL, Shane_Livensparger_test$type))
confusionMatrix(prediction_SL,Shane_Livensparger_test$type)
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
Shane_Livensparger_Strikes = subset(Shane_Livensparger, Shane_Livensparger$type == "S")
Shane_Livensparger_Balls = subset(Shane_Livensparger, Shane_Livensparger$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Shane_Livensparger_Strikes$AdjustedCall = ifelse((Shane_Livensparger_Strikes$plate_x < 0.833 & Shane_Livensparger_Strikes$plate_x > -0.833) & (Shane_Livensparger_Strikes$plate_z > Shane_Livensparger_Strikes$sz_bot & Shane_Livensparger_Strikes$plate_z < Shane_Livensparger_Strikes$sz_top), 'S', 'B')
table(Shane_Livensparger_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Shane_Livensparger_Balls$AdjustedCall = ifelse((Shane_Livensparger_Balls$plate_x > 0.833 | Shane_Livensparger_Balls$plate_x < -0.833)|(Shane_Livensparger_Balls$plate_z < Shane_Livensparger_Balls$sz_bot | Shane_Livensparger_Balls$plate_z > Shane_Livensparger_Balls$sz_top),'B','S')
table(Shane_Livensparger_Balls$AdjustedCall)
# Merge to create new dataset
Shane_Livensparger_AdjustedCalls = rbind(Shane_Livensparger_Strikes,Shane_Livensparger_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Shane_Livensparger)
plot(tree_model)
text(tree_model, use.n = T)
prediction_SL<-predict(tree_model, newdata = Shane_Livensparger_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_SL,Shane_Livensparger_AdjustedCalls$AdjustedCall))
