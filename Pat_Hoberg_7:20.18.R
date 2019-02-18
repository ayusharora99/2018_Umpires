#8/27/18: Giants vs Diamondbacks : Pat Hoberg

#126 pitches were called strikes/balls

#The robot-ump called 40 of those pitches as called strikes & 86 as balls

#Pat Hoberg called 50 of those pitches as called strikes & 76 as balls 

#Accuracy: 92%

Pat_Hoberg <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Pat_Hoberg_7:20:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Pat_Hoberg)
names(Pat_Hoberg)
is.na(Pat_Hoberg)
colSums(is.na(Pat_Hoberg)) 
Pat_Hoberg = Pat_Hoberg[,colSums(is.na(Pat_Hoberg)) == 0] 
dim(Pat_Hoberg)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Pat_Hoberg = Pat_Hoberg[ , !(names(Pat_Hoberg) %in% drops)]
# Pat_Hoberg = Pat_Hoberg[keep]
dim(Pat_Hoberg)
Pat_Hoberg

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Pat_Hoberg_train = Pat_Hoberg[0:(0.8 * nrow(Pat_Hoberg)),]
dim(Pat_Hoberg_train)
prop.table(table(Pat_Hoberg_train$type))
Pat_Hoberg_test = Pat_Hoberg[(0.8*nrow(Pat_Hoberg)):nrow(Pat_Hoberg),]
dim(Pat_Hoberg_test)
prop.table(table(Pat_Hoberg_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Pat_Hoberg_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Pat_Hoberg_test, type = 'class')

confusionMatrix(table(prediction_BG, Pat_Hoberg_test$type))
confusionMatrix(prediction_BG,Pat_Hoberg_test$type)
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
Pat_Hoberg_Strikes = subset(Pat_Hoberg, Pat_Hoberg$type == "S")
Pat_Hoberg_Balls = subset(Pat_Hoberg, Pat_Hoberg$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Pat_Hoberg_Strikes$AdjustedCall = ifelse((Pat_Hoberg_Strikes$plate_x < 0.833 & Pat_Hoberg_Strikes$plate_x > -0.833) & (Pat_Hoberg_Strikes$plate_z > Pat_Hoberg_Strikes$sz_bot & Pat_Hoberg_Strikes$plate_z < Pat_Hoberg_Strikes$sz_top), 'S', 'B')
table(Pat_Hoberg_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Pat_Hoberg_Balls$AdjustedCall = ifelse((Pat_Hoberg_Balls$plate_x > 0.833 | Pat_Hoberg_Balls$plate_x < -0.833)|(Pat_Hoberg_Balls$plate_z < Pat_Hoberg_Balls$sz_bot | Pat_Hoberg_Balls$plate_z > Pat_Hoberg_Balls$sz_top),'B','S')
table(Pat_Hoberg_Balls$AdjustedCall)
# Merge to create new dataset
Pat_Hoberg_AdjustedCalls = rbind(Pat_Hoberg_Strikes,Pat_Hoberg_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Pat_Hoberg)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Pat_Hoberg_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Pat_Hoberg_AdjustedCalls$AdjustedCall))



