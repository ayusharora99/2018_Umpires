#7/13/18: Giants vs A's : Greg Gibson

#139 pitches were called strikes/balls

#The robot-ump called 36 of those pitches as called strikes & 103 as balls

#Greg Gibson called 45 of those pitches as called strikes & 94 as balls 

#Accuracy: 91%

Greg_Gibson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Greg_Gibson_7:13:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Greg_Gibson)
names(Greg_Gibson)
is.na(Greg_Gibson)
colSums(is.na(Greg_Gibson)) 
Greg_Gibson = Greg_Gibson[,colSums(is.na(Greg_Gibson)) == 0] 
dim(Greg_Gibson)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Greg_Gibson = Greg_Gibson[ , !(names(Greg_Gibson) %in% drops)]
# Greg_Gibson = Greg_Gibson[keep]
dim(Greg_Gibson)
Greg_Gibson

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Greg_Gibson_train = Greg_Gibson[0:(0.8 * nrow(Greg_Gibson)),]
dim(Greg_Gibson_train)
prop.table(table(Greg_Gibson_train$type))
Greg_Gibson_test = Greg_Gibson[(0.8*nrow(Greg_Gibson)):nrow(Greg_Gibson),]
dim(Greg_Gibson_test)
prop.table(table(Greg_Gibson_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Greg_Gibson_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Greg_Gibson_test, type = 'class')

confusionMatrix(table(prediction_BG, Greg_Gibson_test$type))
confusionMatrix(prediction_BG,Greg_Gibson_test$type)
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
Greg_Gibson_Strikes = subset(Greg_Gibson, Greg_Gibson$type == "S")
Greg_Gibson_Balls = subset(Greg_Gibson, Greg_Gibson$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Greg_Gibson_Strikes$AdjustedCall = ifelse((Greg_Gibson_Strikes$plate_x < 0.833 & Greg_Gibson_Strikes$plate_x > -0.833) & (Greg_Gibson_Strikes$plate_z > Greg_Gibson_Strikes$sz_bot & Greg_Gibson_Strikes$plate_z < Greg_Gibson_Strikes$sz_top), 'S', 'B')
table(Greg_Gibson_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Greg_Gibson_Balls$AdjustedCall = ifelse((Greg_Gibson_Balls$plate_x > 0.833 | Greg_Gibson_Balls$plate_x < -0.833)|(Greg_Gibson_Balls$plate_z < Greg_Gibson_Balls$sz_bot | Greg_Gibson_Balls$plate_z > Greg_Gibson_Balls$sz_top),'B','S')
table(Greg_Gibson_Balls$AdjustedCall)
# Merge to create new dataset
Greg_Gibson_AdjustedCalls = rbind(Greg_Gibson_Strikes,Greg_Gibson_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Greg_Gibson)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Greg_Gibson_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Greg_Gibson_AdjustedCalls$AdjustedCall))



