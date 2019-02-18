#8/29/18 Giants-Diamondbacks: Corey Blaser 

#136 pitches were called strikes or balls.

#The robo-ump called 44 of those pitches as called strikes & 92 as balls.

#Corey Blaser called 52 of those pitches as called strikes and 84 as balls. 

#Accuracy: 91%

CoreyBlaser <- read.csv("~/Downloads/Corey_Blaser.csv")

library(e1071)
library(caret)
library(rpart)
dim(CoreyBlaser)
names(CoreyBlaser)
is.na(CoreyBlaser)
colSums(is.na(CoreyBlaser)) 
CoreyBlaser = CoreyBlaser[,colSums(is.na(CoreyBlaser)) == 0] 
dim(CoreyBlaser)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
CoreyBlaser = CoreyBlaser[ , !(names(CoreyBlaser) %in% drops)]
CoreyBlaser = CoreyBlaser[keep]
dim(CoreyBlaser)
CoreyBlaser

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
CoreyBlaser_test = CoreyBlaser[0:28,]
dim(CoreyBlaser_test)
prop.table(table(CoreyBlaser_test$type))
CoreyBlaser_train = CoreyBlaser[29:136,]
dim(CoreyBlaser_train)
prop.table(table(CoreyBlaser_train$type))

# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = CoreyBlaser_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CB<-predict(tree_model, newdata = CoreyBlaser_test, type = 'class')
table(prediction_CB, CoreyBlaser_test$type)
confusionMatrix(prediction_CB,CoreyBlaser_test$type)
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
CoreyBlaser_Strikes = subset(CoreyBlaser, CoreyBlaser$type == "S")
CoreyBlaser_Balls = subset(CoreyBlaser, CoreyBlaser$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
CoreyBlaser_Strikes$AdjustedCall = ifelse((CoreyBlaser_Strikes$plate_x < 0.833 & CoreyBlaser_Strikes$plate_x > -0.833) & (CoreyBlaser_Strikes$plate_z > CoreyBlaser_Strikes$sz_bot & CoreyBlaser_Strikes$plate_z < CoreyBlaser_Strikes$sz_top), 'S', 'B')
table(CoreyBlaser_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
CoreyBlaser_Balls$AdjustedCall = ifelse((CoreyBlaser_Balls$plate_x > 0.833 | CoreyBlaser_Balls$plate_x < -0.833)|(CoreyBlaser_Balls$plate_z < CoreyBlaser_Balls$sz_bot | CoreyBlaser_Balls$plate_z > CoreyBlaser_Balls$sz_top),'B','S')
table(CoreyBlaser_Balls$AdjustedCall)
# Merge to create new dataset
CoreyBlaser_AdjustedCalls = rbind(CoreyBlaser_Strikes,CoreyBlaser_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = CoreyBlaser)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CB<-predict(tree_model, newdata = CoreyBlaser_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_CB,CoreyBlaser_AdjustedCalls$AdjustedCall))

# HUNTER WENDELSTEDT's Strike Zone: 88.24% Accurate, 91.56% Precision, 89.41% Recall
