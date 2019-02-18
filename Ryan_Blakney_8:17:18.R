#8/27/18: Giants vs Reds : Ryan Blakney

#140 pitches were called strikes/balls

#The robot-ump called 55 of those pitches as called strikes & 85 as balls

#Ryan Blakney called 49 of those pitches as called strikes & 91 as balls 

#Accuracy: 90%

Ryan_Blakney <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Ryan_Blakney_8:17:18.csv")
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Ryan_Blakney)
names(Ryan_Blakney)
is.na(Ryan_Blakney)
colSums(is.na(Ryan_Blakney)) 
Ryan_Blakney = Ryan_Blakney[,colSums(is.na(Ryan_Blakney)) == 0] 
dim(Ryan_Blakney)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Ryan_Blakney = Ryan_Blakney[ , !(names(Ryan_Blakney) %in% drops)]
# Ryan_Blakney = Ryan_Blakney[keep]
dim(Ryan_Blakney)
Ryan_Blakney

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Ryan_Blakney_train = Ryan_Blakney[0:(0.8 * nrow(Ryan_Blakney)),]
dim(Ryan_Blakney_train)
prop.table(table(Ryan_Blakney_train$type))
Ryan_Blakney_test = Ryan_Blakney[(0.8*nrow(Ryan_Blakney)):nrow(Ryan_Blakney),]
dim(Ryan_Blakney_test)
prop.table(table(Ryan_Blakney_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Ryan_Blakney_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_RB<-predict(tree_model, newdata = Ryan_Blakney_test, type = 'class')

confusionMatrix(table(prediction_RB, Ryan_Blakney_test$type))
confusionMatrix(prediction_RB,Ryan_Blakney_test$type)
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
Ryan_Blakney_Strikes = subset(Ryan_Blakney, Ryan_Blakney$type == "S")
Ryan_Blakney_Balls = subset(Ryan_Blakney, Ryan_Blakney$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Ryan_Blakney_Strikes$AdjustedCall = ifelse((Ryan_Blakney_Strikes$plate_x < 0.833 & Ryan_Blakney_Strikes$plate_x > -0.833) & (Ryan_Blakney_Strikes$plate_z > Ryan_Blakney_Strikes$sz_bot & Ryan_Blakney_Strikes$plate_z < Ryan_Blakney_Strikes$sz_top), 'S', 'B')
table(Ryan_Blakney_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Ryan_Blakney_Balls$AdjustedCall = ifelse((Ryan_Blakney_Balls$plate_x > 0.833 | Ryan_Blakney_Balls$plate_x < -0.833)|(Ryan_Blakney_Balls$plate_z < Ryan_Blakney_Balls$sz_bot | Ryan_Blakney_Balls$plate_z > Ryan_Blakney_Balls$sz_top),'B','S')
table(Ryan_Blakney_Balls$AdjustedCall)
# Merge to create new dataset
Ryan_Blakney_AdjustedCalls = rbind(Ryan_Blakney_Strikes,Ryan_Blakney_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Ryan_Blakney)
plot(tree_model)
text(tree_model, use.n = T)
prediction_RB<-predict(tree_model, newdata = Ryan_Blakney_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_RB,Ryan_Blakney_AdjustedCalls$AdjustedCall))



