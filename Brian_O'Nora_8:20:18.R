#8/27/18: Giants vs Mets : Brian ONora

#201 pitches were called strikes/balls

#The robot-ump called 54 of those pitches as called strikes & 147 as balls

#Brian ONora called 61 of those pitches as called strikes & 140 as balls 

#Accuracy: 93%

Brian_ONora <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Brian_O'Nora_8:20:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Brian_ONora)
names(Brian_ONora)
is.na(Brian_ONora)
colSums(is.na(Brian_ONora)) 
Brian_ONora = Brian_ONora[,colSums(is.na(Brian_ONora)) == 0] 
dim(Brian_ONora)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Brian_ONora = Brian_ONora[ , !(names(Brian_ONora) %in% drops)]
# Brian_ONora = Brian_ONora[keep]
dim(Brian_ONora)
Brian_ONora

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Brian_ONora_train = Brian_ONora[0:(0.8 * nrow(Brian_ONora)),]
dim(Brian_ONora_train)
prop.table(table(Brian_ONora_train$type))
Brian_ONora_test = Brian_ONora[(0.8*nrow(Brian_ONora)):nrow(Brian_ONora),]
dim(Brian_ONora_test)
prop.table(table(Brian_ONora_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Brian_ONora_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BN<-predict(tree_model, newdata = Brian_ONora_test, type = 'class')

confusionMatrix(table(prediction_BN, Brian_ONora_test$type))
confusionMatrix(prediction_BN,Brian_ONora_test$type)
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
Brian_ONora_Strikes = subset(Brian_ONora, Brian_ONora$type == "S")
Brian_ONora_Balls = subset(Brian_ONora, Brian_ONora$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Brian_ONora_Strikes$AdjustedCall = ifelse((Brian_ONora_Strikes$plate_x < 0.833 & Brian_ONora_Strikes$plate_x > -0.833) & (Brian_ONora_Strikes$plate_z > Brian_ONora_Strikes$sz_bot & Brian_ONora_Strikes$plate_z < Brian_ONora_Strikes$sz_top), 'S', 'B')
table(Brian_ONora_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Brian_ONora_Balls$AdjustedCall = ifelse((Brian_ONora_Balls$plate_x > 0.833 | Brian_ONora_Balls$plate_x < -0.833)|(Brian_ONora_Balls$plate_z < Brian_ONora_Balls$sz_bot | Brian_ONora_Balls$plate_z > Brian_ONora_Balls$sz_top),'B','S')
table(Brian_ONora_Balls$AdjustedCall)
# Merge to create new dataset
Brian_ONora_AdjustedCalls = rbind(Brian_ONora_Strikes,Brian_ONora_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Brian_ONora)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BN<-predict(tree_model, newdata = Brian_ONora_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BN,Brian_ONora_AdjustedCalls$AdjustedCall))



