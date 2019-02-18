#8/7/18: Giants vs Astros : Quinn Wolcott

# 142 pitches were called strikes/balls

# The robot-ump called 49 of those pitches as called strikes & 93 as balls

# Quinn Wolcott called 65 of those pitches as called strikes & 77 as balls 

# Accuracy: 87%

Quinn_Wolcott <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Quin_Wolcott_8:6:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Quinn_Wolcott)
names(Quinn_Wolcott)
is.na(Quinn_Wolcott)
colSums(is.na(Quinn_Wolcott)) 
Quinn_Wolcott = Quinn_Wolcott[,colSums(is.na(Quinn_Wolcott)) == 0] 
dim(Quinn_Wolcott)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Quinn_Wolcott = Quinn_Wolcott[ , !(names(Quinn_Wolcott) %in% drops)]
# Quinn_Wolcott = Quinn_Wolcott[keep]
dim(Quinn_Wolcott)
Quinn_Wolcott

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Quinn_Wolcott_train = Quinn_Wolcott[0:(0.8 * nrow(Quinn_Wolcott)),]
dim(Quinn_Wolcott_train)
prop.table(table(Quinn_Wolcott_train$type))
Quinn_Wolcott_test = Quinn_Wolcott[(0.8*nrow(Quinn_Wolcott)):nrow(Quinn_Wolcott),]
dim(Quinn_Wolcott_test)
prop.table(table(Quinn_Wolcott_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Quinn_Wolcott_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_QW<-predict(tree_model, newdata = Quinn_Wolcott_test, type = 'class')

confusionMatrix(table(prediction_QW, Quinn_Wolcott_test$type))
confusionMatrix(prediction_QW,Quinn_Wolcott_test$type)
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
Quinn_Wolcott_Strikes = subset(Quinn_Wolcott, Quinn_Wolcott$type == "S")
Quinn_Wolcott_Balls = subset(Quinn_Wolcott, Quinn_Wolcott$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Quinn_Wolcott_Strikes$AdjustedCall = ifelse((Quinn_Wolcott_Strikes$plate_x < 0.833 & Quinn_Wolcott_Strikes$plate_x > -0.833) & (Quinn_Wolcott_Strikes$plate_z > Quinn_Wolcott_Strikes$sz_bot & Quinn_Wolcott_Strikes$plate_z < Quinn_Wolcott_Strikes$sz_top), 'S', 'B')
table(Quinn_Wolcott_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Quinn_Wolcott_Balls$AdjustedCall = ifelse((Quinn_Wolcott_Balls$plate_x > 0.833 | Quinn_Wolcott_Balls$plate_x < -0.833)|(Quinn_Wolcott_Balls$plate_z < Quinn_Wolcott_Balls$sz_bot | Quinn_Wolcott_Balls$plate_z > Quinn_Wolcott_Balls$sz_top),'B','S')
table(Quinn_Wolcott_Balls$AdjustedCall)
# Merge to create new dataset
Quinn_Wolcott_AdjustedCalls = rbind(Quinn_Wolcott_Strikes,Quinn_Wolcott_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Quinn_Wolcott)
plot(tree_model)
text(tree_model, use.n = T)
prediction_QW<-predict(tree_model, newdata = Quinn_Wolcott_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_QW,Quinn_Wolcott_AdjustedCalls$AdjustedCall))



