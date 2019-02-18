#8/15/18: Giants vs Dodgers : Stu Scherwater

#193 pitches were called strikes/balls

#The robot-ump called 59 of those pitches as called strikes & 134 as balls

#Stu Scherwater called 67 of those pitches as called strikes & 126 as balls 

#Accuracy: 88%

Stu_Scherwater <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Stu_Scherwater_8:15:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Stu_Scherwater)
names(Stu_Scherwater)
is.na(Stu_Scherwater)
colSums(is.na(Stu_Scherwater)) 
Stu_Scherwater = Stu_Scherwater[,colSums(is.na(Stu_Scherwater)) == 0] 
dim(Stu_Scherwater)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Stu_Scherwater = Stu_Scherwater[ , !(names(Stu_Scherwater) %in% drops)]
# Stu_Scherwater = Stu_Scherwater[keep]
dim(Stu_Scherwater)
Stu_Scherwater

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Stu_Scherwater_train = Stu_Scherwater[0:(0.8 * nrow(Stu_Scherwater)),]
dim(Stu_Scherwater_train)
prop.table(table(Stu_Scherwater_train$type))
Stu_Scherwater_test = Stu_Scherwater[(0.8*nrow(Stu_Scherwater)):nrow(Stu_Scherwater),]
dim(Stu_Scherwater_test)
prop.table(table(Stu_Scherwater_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Stu_Scherwater_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_SS<-predict(tree_model, newdata = Stu_Scherwater_test, type = 'class')

confusionMatrix(table(prediction_SS, Stu_Scherwater_test$type))
confusionMatrix(prediction_SS,Stu_Scherwater_test$type)
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
Stu_Scherwater_Strikes = subset(Stu_Scherwater, Stu_Scherwater$type == "S")
Stu_Scherwater_Balls = subset(Stu_Scherwater, Stu_Scherwater$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Stu_Scherwater_Strikes$AdjustedCall = ifelse((Stu_Scherwater_Strikes$plate_x < 0.833 & Stu_Scherwater_Strikes$plate_x > -0.833) & (Stu_Scherwater_Strikes$plate_z > Stu_Scherwater_Strikes$sz_bot & Stu_Scherwater_Strikes$plate_z < Stu_Scherwater_Strikes$sz_top), 'S', 'B')
table(Stu_Scherwater_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Stu_Scherwater_Balls$AdjustedCall = ifelse((Stu_Scherwater_Balls$plate_x > 0.833 | Stu_Scherwater_Balls$plate_x < -0.833)|(Stu_Scherwater_Balls$plate_z < Stu_Scherwater_Balls$sz_bot | Stu_Scherwater_Balls$plate_z > Stu_Scherwater_Balls$sz_top),'B','S')
table(Stu_Scherwater_Balls$AdjustedCall)
# Merge to create new dataset
Stu_Scherwater_AdjustedCalls = rbind(Stu_Scherwater_Strikes,Stu_Scherwater_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Stu_Scherwater)
plot(tree_model)
text(tree_model, use.n = T)
prediction_SS<-predict(tree_model, newdata = Stu_Scherwater_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_SS,Stu_Scherwater_AdjustedCalls$AdjustedCall))
