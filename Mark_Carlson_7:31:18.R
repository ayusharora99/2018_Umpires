#7/31/18: Giants vs Padres : Mark Carslon

#167 pitches were called strikes/balls

#The robot-ump called 39 of those pitches as called strikes & 128 as balls

#Mark Carslon called 49 of those pitches as called strikes & 118 as balls 

#Accuracy: 93%

Mark_Carslon <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mark_Carslon_7:31:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Mark_Carslon)
names(Mark_Carslon)
is.na(Mark_Carslon)
colSums(is.na(Mark_Carslon)) 
Mark_Carslon = Mark_Carslon[,colSums(is.na(Mark_Carslon)) == 0] 
dim(Mark_Carslon)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mark_Carslon = Mark_Carslon[ , !(names(Mark_Carslon) %in% drops)]
# Mark_Carslon = Mark_Carslon[keep]
dim(Mark_Carslon)
Mark_Carslon

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Mark_Carslon_train = Mark_Carslon[0:(0.8 * nrow(Mark_Carslon)),]
dim(Mark_Carslon_train)
prop.table(table(Mark_Carslon_train$type))
Mark_Carslon_test = Mark_Carslon[(0.8*nrow(Mark_Carslon)):nrow(Mark_Carslon),]
dim(Mark_Carslon_test)
prop.table(table(Mark_Carslon_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mark_Carslon_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Mark_Carslon_test, type = 'class')

confusionMatrix(table(prediction_BG, Mark_Carslon_test$type))
confusionMatrix(prediction_BG,Mark_Carslon_test$type)
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
Mark_Carslon_Strikes = subset(Mark_Carslon, Mark_Carslon$type == "S")
Mark_Carslon_Balls = subset(Mark_Carslon, Mark_Carslon$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mark_Carslon_Strikes$AdjustedCall = ifelse((Mark_Carslon_Strikes$plate_x < 0.833 & Mark_Carslon_Strikes$plate_x > -0.833) & (Mark_Carslon_Strikes$plate_z > Mark_Carslon_Strikes$sz_bot & Mark_Carslon_Strikes$plate_z < Mark_Carslon_Strikes$sz_top), 'S', 'B')
table(Mark_Carslon_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mark_Carslon_Balls$AdjustedCall = ifelse((Mark_Carslon_Balls$plate_x > 0.833 | Mark_Carslon_Balls$plate_x < -0.833)|(Mark_Carslon_Balls$plate_z < Mark_Carslon_Balls$sz_bot | Mark_Carslon_Balls$plate_z > Mark_Carslon_Balls$sz_top),'B','S')
table(Mark_Carslon_Balls$AdjustedCall)
# Merge to create new dataset
Mark_Carslon_AdjustedCalls = rbind(Mark_Carslon_Strikes,Mark_Carslon_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Mark_Carslon)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Mark_Carslon_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Mark_Carslon_AdjustedCalls$AdjustedCall))



