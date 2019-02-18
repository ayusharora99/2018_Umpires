#9/3/18: Giants vs Rockies : Chris Guccione

#140 pitches were called strikes/balls

#The robot-ump called 38 of those pitches as called strikes & 102 as balls

#Chris Guccione called 39 of those pitches as called strikes & 101 as balls 

#Accuracy: 94%

Chris_Guccione <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Chriss_Guccione_9:3:18.csv")
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Chris_Guccione)
names(Chris_Guccione)
is.na(Chris_Guccione)
colSums(is.na(Chris_Guccione)) 
Chris_Guccione = Chris_Guccione[,colSums(is.na(Chris_Guccione)) == 0] 
dim(Chris_Guccione)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Chris_Guccione = Chris_Guccione[ , !(names(Chris_Guccione) %in% drops)]
# Chris_Guccione = Chris_Guccione[keep]
dim(Chris_Guccione)
Chris_Guccione

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Chris_Guccione_train = Chris_Guccione[0:(0.8 * nrow(Chris_Guccione)),]
dim(Chris_Guccione_train)
prop.table(table(Chris_Guccione_train$type))
Chris_Guccione_test = Chris_Guccione[(0.8*nrow(Chris_Guccione)):nrow(Chris_Guccione),]
dim(Chris_Guccione_test)
prop.table(table(Chris_Guccione_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Chris_Guccione_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CG<-predict(tree_model, newdata = Chris_Guccione_test, type = 'class')

confusionMatrix(table(prediction_CG, Chris_Guccione_test$type))
confusionMatrix(prediction_CG,Chris_Guccione_test$type)
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
Chris_Guccione_Strikes = subset(Chris_Guccione, Chris_Guccione$type == "S")
Chris_Guccione_Balls = subset(Chris_Guccione, Chris_Guccione$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Chris_Guccione_Strikes$AdjustedCall = ifelse((Chris_Guccione_Strikes$plate_x < 0.833 & Chris_Guccione_Strikes$plate_x > -0.833) & (Chris_Guccione_Strikes$plate_z > Chris_Guccione_Strikes$sz_bot & Chris_Guccione_Strikes$plate_z < Chris_Guccione_Strikes$sz_top), 'S', 'B')
table(Chris_Guccione_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Chris_Guccione_Balls$AdjustedCall = ifelse((Chris_Guccione_Balls$plate_x > 0.833 | Chris_Guccione_Balls$plate_x < -0.833)|(Chris_Guccione_Balls$plate_z < Chris_Guccione_Balls$sz_bot | Chris_Guccione_Balls$plate_z > Chris_Guccione_Balls$sz_top),'B','S')
table(Chris_Guccione_Balls$AdjustedCall)
# Merge to create new dataset
Chris_Guccione_AdjustedCalls = rbind(Chris_Guccione_Strikes,Chris_Guccione_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Chris_Guccione)
plot(tree_model)
text(tree_model, use.n = T)
prediction_CG<-predict(tree_model, newdata = Chris_Guccione_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_CG,Chris_Guccione_AdjustedCalls$AdjustedCall))

