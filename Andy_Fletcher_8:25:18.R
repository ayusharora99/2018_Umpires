#8/27/18: Giants vs Diamondbacks : Andy Fletcher

#159 pitches were called strikes/balls

#The robot-ump called 44 of those pitches as called strikes & 115 as balls

# Andy Fletcher called 50 of those pitches as called strikes & 109 as balls 

#Accuracy: 89%

Andy_Fletcher <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Andy_Fletcher_8:25:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Andy_Fletcher)
names(Andy_Fletcher)
is.na(Andy_Fletcher)
colSums(is.na(Andy_Fletcher)) 
Andy_Fletcher = Andy_Fletcher[,colSums(is.na(Andy_Fletcher)) == 0] 
dim(Andy_Fletcher)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Andy_Fletcher = Andy_Fletcher[ , !(names(Andy_Fletcher) %in% drops)]
# Andy_Fletcher = Andy_Fletcher[keep]
dim(Andy_Fletcher)
Andy_Fletcher

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Andy_Fletcher_train = Andy_Fletcher[0:(0.8 * nrow(Andy_Fletcher)),]
dim(Andy_Fletcher_train)
prop.table(table(Andy_Fletcher_train$type))
Andy_Fletcher_test = Andy_Fletcher[(0.8*nrow(Andy_Fletcher)):nrow(Andy_Fletcher),]
dim(Andy_Fletcher_test)
prop.table(table(Andy_Fletcher_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Andy_Fletcher_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_AF<-predict(tree_model, newdata = Andy_Fletcher_test, type = 'class')

confusionMatrix(table(prediction_AF, Andy_Fletcher_test$type))
confusionMatrix(prediction_AF,Andy_Fletcher_test$type)
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
Andy_Fletcher_Strikes = subset(Andy_Fletcher, Andy_Fletcher$type == "S")
Andy_Fletcher_Balls = subset(Andy_Fletcher, Andy_Fletcher$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Andy_Fletcher_Strikes$AdjustedCall = ifelse((Andy_Fletcher_Strikes$plate_x < 0.833 & Andy_Fletcher_Strikes$plate_x > -0.833) & (Andy_Fletcher_Strikes$plate_z > Andy_Fletcher_Strikes$sz_bot & Andy_Fletcher_Strikes$plate_z < Andy_Fletcher_Strikes$sz_top), 'S', 'B')
table(Andy_Fletcher_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Andy_Fletcher_Balls$AdjustedCall = ifelse((Andy_Fletcher_Balls$plate_x > 0.833 | Andy_Fletcher_Balls$plate_x < -0.833)|(Andy_Fletcher_Balls$plate_z < Andy_Fletcher_Balls$sz_bot | Andy_Fletcher_Balls$plate_z > Andy_Fletcher_Balls$sz_top),'B','S')
table(Andy_Fletcher_Balls$AdjustedCall)
# Merge to create new dataset
Andy_Fletcher_AdjustedCalls = rbind(Andy_Fletcher_Strikes,Andy_Fletcher_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Andy_Fletcher)
plot(tree_model)
text(tree_model, use.n = T)
prediction_AF<-predict(tree_model, newdata = Andy_Fletcher_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_AF,Andy_Fletcher_AdjustedCalls$AdjustedCall))


