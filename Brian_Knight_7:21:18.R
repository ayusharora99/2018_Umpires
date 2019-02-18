#7/21/18: Giants vs A's : Brian Knight

#187 pitches were called strikes/balls

#The robot-ump called 38 of those pitches as called strikes & 149 as balls

#Brian Knight called 52 of those pitches as called strikes & 135 as balls 

#Accuracy: 92%

Brian_Knight <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brian_Knight_7:21:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Brian_Knight)
names(Brian_Knight)
is.na(Brian_Knight)
colSums(is.na(Brian_Knight)) 
Brian_Knight = Brian_Knight[,colSums(is.na(Brian_Knight)) == 0] 
dim(Brian_Knight)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Brian_Knight = Brian_Knight[ , !(names(Brian_Knight) %in% drops)]
# Brian_Knight = Brian_Knight[keep]
dim(Brian_Knight)
Brian_Knight

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Brian_Knight_train = Brian_Knight[0:(0.8 * nrow(Brian_Knight)),]
dim(Brian_Knight_train)
prop.table(table(Brian_Knight_train$type))
Brian_Knight_test = Brian_Knight[(0.8*nrow(Brian_Knight)):nrow(Brian_Knight),]
dim(Brian_Knight_test)
prop.table(table(Brian_Knight_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Brian_Knight_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Brian_Knight_test, type = 'class')

confusionMatrix(table(prediction_BG, Brian_Knight_test$type))
confusionMatrix(prediction_BG,Brian_Knight_test$type)
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
Brian_Knight_Strikes = subset(Brian_Knight, Brian_Knight$type == "S")
Brian_Knight_Balls = subset(Brian_Knight, Brian_Knight$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Brian_Knight_Strikes$AdjustedCall = ifelse((Brian_Knight_Strikes$plate_x < 0.833 & Brian_Knight_Strikes$plate_x > -0.833) & (Brian_Knight_Strikes$plate_z > Brian_Knight_Strikes$sz_bot & Brian_Knight_Strikes$plate_z < Brian_Knight_Strikes$sz_top), 'S', 'B')
table(Brian_Knight_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Brian_Knight_Balls$AdjustedCall = ifelse((Brian_Knight_Balls$plate_x > 0.833 | Brian_Knight_Balls$plate_x < -0.833)|(Brian_Knight_Balls$plate_z < Brian_Knight_Balls$sz_bot | Brian_Knight_Balls$plate_z > Brian_Knight_Balls$sz_top),'B','S')
table(Brian_Knight_Balls$AdjustedCall)
# Merge to create new dataset
Brian_Knight_AdjustedCalls = rbind(Brian_Knight_Strikes,Brian_Knight_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Brian_Knight)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Brian_Knight_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Brian_Knight_AdjustedCalls$AdjustedCall))



