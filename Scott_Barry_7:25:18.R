#7/25/18: Giants vs Mariners : Scott Barry

#114 pitches were called strikes/balls

#The robot-ump called 34 of those pitches as called strikes & 80 as balls

#Scott Barry called 36 of those pitches as called strikes & 78 as balls 

#Accuracy: 93%

Scott_Barry <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Scott_Barry_7:25:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Scott_Barry)
names(Scott_Barry)
is.na(Scott_Barry)
colSums(is.na(Scott_Barry)) 
Scott_Barry = Scott_Barry[,colSums(is.na(Scott_Barry)) == 0] 
dim(Scott_Barry)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Scott_Barry = Scott_Barry[ , !(names(Scott_Barry) %in% drops)]
# Scott_Barry = Scott_Barry[keep]
dim(Scott_Barry)
Scott_Barry

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Scott_Barry_train = Scott_Barry[0:(0.8 * nrow(Scott_Barry)),]
dim(Scott_Barry_train)
prop.table(table(Scott_Barry_train$type))
Scott_Barry_test = Scott_Barry[(0.8*nrow(Scott_Barry)):nrow(Scott_Barry),]
dim(Scott_Barry_test)
prop.table(table(Scott_Barry_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Scott_Barry_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Scott_Barry_test, type = 'class')

confusionMatrix(table(prediction_BG, Scott_Barry_test$type))
confusionMatrix(prediction_BG,Scott_Barry_test$type)
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
Scott_Barry_Strikes = subset(Scott_Barry, Scott_Barry$type == "S")
Scott_Barry_Balls = subset(Scott_Barry, Scott_Barry$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Scott_Barry_Strikes$AdjustedCall = ifelse((Scott_Barry_Strikes$plate_x < 0.833 & Scott_Barry_Strikes$plate_x > -0.833) & (Scott_Barry_Strikes$plate_z > Scott_Barry_Strikes$sz_bot & Scott_Barry_Strikes$plate_z < Scott_Barry_Strikes$sz_top), 'S', 'B')
table(Scott_Barry_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Scott_Barry_Balls$AdjustedCall = ifelse((Scott_Barry_Balls$plate_x > 0.833 | Scott_Barry_Balls$plate_x < -0.833)|(Scott_Barry_Balls$plate_z < Scott_Barry_Balls$sz_bot | Scott_Barry_Balls$plate_z > Scott_Barry_Balls$sz_top),'B','S')
table(Scott_Barry_Balls$AdjustedCall)
# Merge to create new dataset
Scott_Barry_AdjustedCalls = rbind(Scott_Barry_Strikes,Scott_Barry_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Scott_Barry)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Scott_Barry_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Scott_Barry_AdjustedCalls$AdjustedCall))



