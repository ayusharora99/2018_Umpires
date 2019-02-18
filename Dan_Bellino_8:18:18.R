#8/27/18: Giants vs Reds: Dan Bellino

#119 pitches were called strikes/balls

#The robot-ump called 34 of those pitches as called strikes & 85 as balls

#Dan Bellino called 40 of those pitches as called strikes & 79 as balls 

#Accuracy: 88%

Dan_Bellino <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Dan_Bellino_8:18:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Dan_Bellino)
names(Dan_Bellino)
is.na(Dan_Bellino)
colSums(is.na(Dan_Bellino)) 
Dan_Bellino = Dan_Bellino[,colSums(is.na(Dan_Bellino)) == 0] 
dim(Dan_Bellino)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Dan_Bellino = Dan_Bellino[ , !(names(Dan_Bellino) %in% drops)]
# Dan_Bellino = Dan_Bellino[keep]
dim(Dan_Bellino)
Dan_Bellino

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Dan_Bellino_train = Dan_Bellino[0:(0.8 * nrow(Dan_Bellino)),]
dim(Dan_Bellino_train)
prop.table(table(Dan_Bellino_train$type))
Dan_Bellino_test = Dan_Bellino[(0.8*nrow(Dan_Bellino)):nrow(Dan_Bellino),]
dim(Dan_Bellino_test)
prop.table(table(Dan_Bellino_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Dan_Bellino_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_DB<-predict(tree_model, newdata = Dan_Bellino_test, type = 'class')

confusionMatrix(table(prediction_DB, Dan_Bellino_test$type))
confusionMatrix(prediction_DB,Dan_Bellino_test$type)
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
Dan_Bellino_Strikes = subset(Dan_Bellino, Dan_Bellino$type == "S")
Dan_Bellino_Balls = subset(Dan_Bellino, Dan_Bellino$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Dan_Bellino_Strikes$AdjustedCall = ifelse((Dan_Bellino_Strikes$plate_x < 0.833 & Dan_Bellino_Strikes$plate_x > -0.833) & (Dan_Bellino_Strikes$plate_z > Dan_Bellino_Strikes$sz_bot & Dan_Bellino_Strikes$plate_z < Dan_Bellino_Strikes$sz_top), 'S', 'B')
table(Dan_Bellino_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Dan_Bellino_Balls$AdjustedCall = ifelse((Dan_Bellino_Balls$plate_x > 0.833 | Dan_Bellino_Balls$plate_x < -0.833)|(Dan_Bellino_Balls$plate_z < Dan_Bellino_Balls$sz_bot | Dan_Bellino_Balls$plate_z > Dan_Bellino_Balls$sz_top),'B','S')
table(Dan_Bellino_Balls$AdjustedCall)
# Merge to create new dataset
Dan_Bellino_AdjustedCalls = rbind(Dan_Bellino_Strikes,Dan_Bellino_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Dan_Bellino)
plot(tree_model)
text(tree_model, use.n = T)
prediction_DB<-predict(tree_model, newdata = Dan_Bellino_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_DB,Dan_Bellino_AdjustedCalls$AdjustedCall))



