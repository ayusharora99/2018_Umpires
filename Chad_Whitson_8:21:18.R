#8/27/18: Giants vs Mets : Chad Whitson

#114 pitches were called strikes/balls

#The robot-ump called 32 of those pitches as called strikes & 82 as balls

#Chad Whitson called 37 of those pitches as called strikes & 77 as balls 

#Accuracy: 90%

Chad_Whitson <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chad_Whitson_8:21:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Chad_Whitson)
names(Chad_Whitson)
is.na(Chad_Whitson)
colSums(is.na(Chad_Whitson)) 
Chad_Whitson = Chad_Whitson[,colSums(is.na(Chad_Whitson)) == 0] 
dim(Chad_Whitson)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chad_Whitson = Chad_Whitson[ , !(names(Chad_Whitson) %in% drops)]
# Chad_Whitson = Chad_Whitson[keep]
dim(Chad_Whitson)
Chad_Whitson

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Chad_Whitson_train = Chad_Whitson[0:(0.8 * nrow(Chad_Whitson)),]
dim(Chad_Whitson_train)
prop.table(table(Chad_Whitson_train$type))
Chad_Whitson_test = Chad_Whitson[(0.8*nrow(Chad_Whitson)):nrow(Chad_Whitson),]
dim(Chad_Whitson_test)
prop.table(table(Chad_Whitson_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chad_Whitson_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CW<-predict(tree_model, newdata = Chad_Whitson_test, type = 'class')

confusionMatrix(table(prediction_CW, Chad_Whitson_test$type))
confusionMatrix(prediction_CW,Chad_Whitson_test$type)
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
Chad_Whitson_Strikes = subset(Chad_Whitson, Chad_Whitson$type == "S")
Chad_Whitson_Balls = subset(Chad_Whitson, Chad_Whitson$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chad_Whitson_Strikes$AdjustedCall = ifelse((Chad_Whitson_Strikes$plate_x < 0.833 & Chad_Whitson_Strikes$plate_x > -0.833) & (Chad_Whitson_Strikes$plate_z > Chad_Whitson_Strikes$sz_bot & Chad_Whitson_Strikes$plate_z < Chad_Whitson_Strikes$sz_top), 'S', 'B')
table(Chad_Whitson_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chad_Whitson_Balls$AdjustedCall = ifelse((Chad_Whitson_Balls$plate_x > 0.833 | Chad_Whitson_Balls$plate_x < -0.833)|(Chad_Whitson_Balls$plate_z < Chad_Whitson_Balls$sz_bot | Chad_Whitson_Balls$plate_z > Chad_Whitson_Balls$sz_top),'B','S')
table(Chad_Whitson_Balls$AdjustedCall)
# Merge to create new dataset
Chad_Whitson_AdjustedCalls = rbind(Chad_Whitson_Strikes,Chad_Whitson_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Chad_Whitson)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CW<-predict(tree_model, newdata = Chad_Whitson_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_CW,Chad_Whitson_AdjustedCalls$AdjustedCall))



