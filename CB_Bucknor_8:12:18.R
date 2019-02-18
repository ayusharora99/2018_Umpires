#8/12/18: Giants vs Pirates : CB Bucknor

#125 pitches were called strikes/balls

#The robot-ump called 41 of those pitches as called strikes & 84 as balls

#CB Bucknor called 45 of those pitches as called strikes & 80 as balls 

#Accuracy: 87%

CB_Bucknor <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/CB_Bucknor_8:12:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(CB_Bucknor)
names(CB_Bucknor)
is.na(CB_Bucknor)
colSums(is.na(CB_Bucknor)) 
CB_Bucknor = CB_Bucknor[,colSums(is.na(CB_Bucknor)) == 0] 
dim(CB_Bucknor)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
CB_Bucknor = CB_Bucknor[ , !(names(CB_Bucknor) %in% drops)]
# CB_Bucknor = CB_Bucknor[keep]
dim(CB_Bucknor)
CB_Bucknor

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
CB_Bucknor_train = CB_Bucknor[0:(0.8 * nrow(CB_Bucknor)),]
dim(CB_Bucknor_train)
prop.table(table(CB_Bucknor_train$type))
CB_Bucknor_test = CB_Bucknor[(0.8*nrow(CB_Bucknor)):nrow(CB_Bucknor),]
dim(CB_Bucknor_test)
prop.table(table(CB_Bucknor_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = CB_Bucknor_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CB<-predict(tree_model, newdata = CB_Bucknor_test, type = 'class')

confusionMatrix(table(prediction_CB, CB_Bucknor_test$type))
confusionMatrix(prediction_CB,CB_Bucknor_test$type)
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
CB_Bucknor_Strikes = subset(CB_Bucknor, CB_Bucknor$type == "S")
CB_Bucknor_Balls = subset(CB_Bucknor, CB_Bucknor$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
CB_Bucknor_Strikes$AdjustedCall = ifelse((CB_Bucknor_Strikes$plate_x < 0.833 & CB_Bucknor_Strikes$plate_x > -0.833) & (CB_Bucknor_Strikes$plate_z > CB_Bucknor_Strikes$sz_bot & CB_Bucknor_Strikes$plate_z < CB_Bucknor_Strikes$sz_top), 'S', 'B')
table(CB_Bucknor_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
CB_Bucknor_Balls$AdjustedCall = ifelse((CB_Bucknor_Balls$plate_x > 0.833 | CB_Bucknor_Balls$plate_x < -0.833)|(CB_Bucknor_Balls$plate_z < CB_Bucknor_Balls$sz_bot | CB_Bucknor_Balls$plate_z > CB_Bucknor_Balls$sz_top),'B','S')
table(CB_Bucknor_Balls$AdjustedCall)
# Merge to create new dataset
CB_Bucknor_AdjustedCalls = rbind(CB_Bucknor_Strikes,CB_Bucknor_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = CB_Bucknor)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CB<-predict(tree_model, newdata = CB_Bucknor_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_CB,CB_Bucknor_AdjustedCalls$AdjustedCall))
