#7/22/18: Giants vs A's : Gerry Davis

#150 pitches were called strikes/balls

#The robot-ump called 42 of those pitches as called strikes & 108 as balls

#Gerry Davis called 41 of those pitches as called strikes & 109 as balls 

#Accuracy: 94%

Gerry_Davis <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Gerry_Davis_7:22:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Gerry_Davis)
names(Gerry_Davis)
is.na(Gerry_Davis)
colSums(is.na(Gerry_Davis)) 
Gerry_Davis = Gerry_Davis[,colSums(is.na(Gerry_Davis)) == 0] 
dim(Gerry_Davis)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Gerry_Davis = Gerry_Davis[ , !(names(Gerry_Davis) %in% drops)]
# Gerry_Davis = Gerry_Davis[keep]
dim(Gerry_Davis)
Gerry_Davis

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Gerry_Davis_train = Gerry_Davis[0:(0.8 * nrow(Gerry_Davis)),]
dim(Gerry_Davis_train)
prop.table(table(Gerry_Davis_train$type))
Gerry_Davis_test = Gerry_Davis[(0.8*nrow(Gerry_Davis)):nrow(Gerry_Davis),]
dim(Gerry_Davis_test)
prop.table(table(Gerry_Davis_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Gerry_Davis_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Gerry_Davis_test, type = 'class')

confusionMatrix(table(prediction_BG, Gerry_Davis_test$type))
confusionMatrix(prediction_BG,Gerry_Davis_test$type)
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
Gerry_Davis_Strikes = subset(Gerry_Davis, Gerry_Davis$type == "S")
Gerry_Davis_Balls = subset(Gerry_Davis, Gerry_Davis$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Gerry_Davis_Strikes$AdjustedCall = ifelse((Gerry_Davis_Strikes$plate_x < 0.833 & Gerry_Davis_Strikes$plate_x > -0.833) & (Gerry_Davis_Strikes$plate_z > Gerry_Davis_Strikes$sz_bot & Gerry_Davis_Strikes$plate_z < Gerry_Davis_Strikes$sz_top), 'S', 'B')
table(Gerry_Davis_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Gerry_Davis_Balls$AdjustedCall = ifelse((Gerry_Davis_Balls$plate_x > 0.833 | Gerry_Davis_Balls$plate_x < -0.833)|(Gerry_Davis_Balls$plate_z < Gerry_Davis_Balls$sz_bot | Gerry_Davis_Balls$plate_z > Gerry_Davis_Balls$sz_top),'B','S')
table(Gerry_Davis_Balls$AdjustedCall)
# Merge to create new dataset
Gerry_Davis_AdjustedCalls = rbind(Gerry_Davis_Strikes,Gerry_Davis_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Gerry_Davis)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Gerry_Davis_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Gerry_Davis_AdjustedCalls$AdjustedCall))



