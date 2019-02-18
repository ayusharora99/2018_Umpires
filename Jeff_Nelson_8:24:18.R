#8/27/18: Giants vs Rangers : Jeff Nelson

#195 pitches were called strikes/balls

#The robot-ump called 49 of those pitches as called strikes & 146 as balls

# Jeff Nelson called 59 of those pitches as called strikes & 136 as balls 

#Accuracy: 92%

Jeff_Nelson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Jeff_Nelson_8:24:18)csv.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Jeff_Nelson)
names(Jeff_Nelson)
is.na(Jeff_Nelson)
colSums(is.na(Jeff_Nelson)) 
Jeff_Nelson = Jeff_Nelson[,colSums(is.na(Jeff_Nelson)) == 0] 
dim(Jeff_Nelson)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Jeff_Nelson = Jeff_Nelson[ , !(names(Jeff_Nelson) %in% drops)]
# Jeff_Nelson = Jeff_Nelson[keep]
dim(Jeff_Nelson)
Jeff_Nelson

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Jeff_Nelson_train = Jeff_Nelson[0:(0.8 * nrow(Jeff_Nelson)),]
dim(Jeff_Nelson_train)
prop.table(table(Jeff_Nelson_train$type))
Jeff_Nelson_test = Jeff_Nelson[(0.8*nrow(Jeff_Nelson)):nrow(Jeff_Nelson),]
dim(Jeff_Nelson_test)
prop.table(table(Jeff_Nelson_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Jeff_Nelson_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_JF<-predict(tree_model, newdata = Jeff_Nelson_test, type = 'class')

confusionMatrix(table(prediction_JF, Jeff_Nelson_test$type))
confusionMatrix(prediction_JF,Jeff_Nelson_test$type)
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
Jeff_Nelson_Strikes = subset(Jeff_Nelson, Jeff_Nelson$type == "S")
Jeff_Nelson_Balls = subset(Jeff_Nelson, Jeff_Nelson$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Jeff_Nelson_Strikes$AdjustedCall = ifelse((Jeff_Nelson_Strikes$plate_x < 0.833 & Jeff_Nelson_Strikes$plate_x > -0.833) & (Jeff_Nelson_Strikes$plate_z > Jeff_Nelson_Strikes$sz_bot & Jeff_Nelson_Strikes$plate_z < Jeff_Nelson_Strikes$sz_top), 'S', 'B')
table(Jeff_Nelson_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Jeff_Nelson_Balls$AdjustedCall = ifelse((Jeff_Nelson_Balls$plate_x > 0.833 | Jeff_Nelson_Balls$plate_x < -0.833)|(Jeff_Nelson_Balls$plate_z < Jeff_Nelson_Balls$sz_bot | Jeff_Nelson_Balls$plate_z > Jeff_Nelson_Balls$sz_top),'B','S')
table(Jeff_Nelson_Balls$AdjustedCall)
# Merge to create new dataset
Jeff_Nelson_AdjustedCalls = rbind(Jeff_Nelson_Strikes,Jeff_Nelson_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Jeff_Nelson)
plot(tree_model)
text(tree_model, use.n = T)
prediction_JF<-predict(tree_model, newdata = Jeff_Nelson_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_JF,Jeff_Nelson_AdjustedCalls$AdjustedCall))



