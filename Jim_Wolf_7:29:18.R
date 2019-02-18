#7/29/18: Giants vs Brewers : Jim Wolf

#152 pitches were called strikes/balls

#The robot-ump called 45 of those pitches as called strikes & 107 as balls

#Jim Wolf called 60 of those pitches as called strikes & 92 as balls 

#Accuracy: 88%

Jim_Wolf <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jim_Wolf_7:29:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Jim_Wolf)
names(Jim_Wolf)
is.na(Jim_Wolf)
colSums(is.na(Jim_Wolf)) 
Jim_Wolf = Jim_Wolf[,colSums(is.na(Jim_Wolf)) == 0] 
dim(Jim_Wolf)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jim_Wolf = Jim_Wolf[ , !(names(Jim_Wolf) %in% drops)]
# Jim_Wolf = Jim_Wolf[keep]
dim(Jim_Wolf)
Jim_Wolf

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Jim_Wolf_train = Jim_Wolf[0:(0.8 * nrow(Jim_Wolf)),]
dim(Jim_Wolf_train)
prop.table(table(Jim_Wolf_train$type))
Jim_Wolf_test = Jim_Wolf[(0.8*nrow(Jim_Wolf)):nrow(Jim_Wolf),]
dim(Jim_Wolf_test)
prop.table(table(Jim_Wolf_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jim_Wolf_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Jim_Wolf_test, type = 'class')

confusionMatrix(table(prediction_BG, Jim_Wolf_test$type))
confusionMatrix(prediction_BG,Jim_Wolf_test$type)
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
Jim_Wolf_Strikes = subset(Jim_Wolf, Jim_Wolf$type == "S")
Jim_Wolf_Balls = subset(Jim_Wolf, Jim_Wolf$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jim_Wolf_Strikes$AdjustedCall = ifelse((Jim_Wolf_Strikes$plate_x < 0.833 & Jim_Wolf_Strikes$plate_x > -0.833) & (Jim_Wolf_Strikes$plate_z > Jim_Wolf_Strikes$sz_bot & Jim_Wolf_Strikes$plate_z < Jim_Wolf_Strikes$sz_top), 'S', 'B')
table(Jim_Wolf_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jim_Wolf_Balls$AdjustedCall = ifelse((Jim_Wolf_Balls$plate_x > 0.833 | Jim_Wolf_Balls$plate_x < -0.833)|(Jim_Wolf_Balls$plate_z < Jim_Wolf_Balls$sz_bot | Jim_Wolf_Balls$plate_z > Jim_Wolf_Balls$sz_top),'B','S')
table(Jim_Wolf_Balls$AdjustedCall)
# Merge to create new dataset
Jim_Wolf_AdjustedCalls = rbind(Jim_Wolf_Strikes,Jim_Wolf_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Jim_Wolf)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Jim_Wolf_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Jim_Wolf_AdjustedCalls$AdjustedCall))



