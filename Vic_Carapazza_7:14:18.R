#7/14/18: Giants vs A's : Vic Carapazza

#145 pitches were called strikes/balls

#The robot-ump called 44 of those pitches as called strikes & 101 as balls

#Vic Carapazza called 51 of those pitches as called strikes & 94 as balls 

#Accuracy: 88%

Vic_Carapazza <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Vic_Carapazza_7:14:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Vic_Carapazza)
names(Vic_Carapazza)
is.na(Vic_Carapazza)
colSums(is.na(Vic_Carapazza)) 
Vic_Carapazza = Vic_Carapazza[,colSums(is.na(Vic_Carapazza)) == 0] 
dim(Vic_Carapazza)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Vic_Carapazza = Vic_Carapazza[ , !(names(Vic_Carapazza) %in% drops)]
# Vic_Carapazza = Vic_Carapazza[keep]
dim(Vic_Carapazza)
Vic_Carapazza

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Vic_Carapazza_train = Vic_Carapazza[0:(0.8 * nrow(Vic_Carapazza)),]
dim(Vic_Carapazza_train)
prop.table(table(Vic_Carapazza_train$type))
Vic_Carapazza_test = Vic_Carapazza[(0.8*nrow(Vic_Carapazza)):nrow(Vic_Carapazza),]
dim(Vic_Carapazza_test)
prop.table(table(Vic_Carapazza_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Vic_Carapazza_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Vic_Carapazza_test, type = 'class')

confusionMatrix(table(prediction_BG, Vic_Carapazza_test$type))
confusionMatrix(prediction_BG,Vic_Carapazza_test$type)
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
Vic_Carapazza_Strikes = subset(Vic_Carapazza, Vic_Carapazza$type == "S")
Vic_Carapazza_Balls = subset(Vic_Carapazza, Vic_Carapazza$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Vic_Carapazza_Strikes$AdjustedCall = ifelse((Vic_Carapazza_Strikes$plate_x < 0.833 & Vic_Carapazza_Strikes$plate_x > -0.833) & (Vic_Carapazza_Strikes$plate_z > Vic_Carapazza_Strikes$sz_bot & Vic_Carapazza_Strikes$plate_z < Vic_Carapazza_Strikes$sz_top), 'S', 'B')
table(Vic_Carapazza_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Vic_Carapazza_Balls$AdjustedCall = ifelse((Vic_Carapazza_Balls$plate_x > 0.833 | Vic_Carapazza_Balls$plate_x < -0.833)|(Vic_Carapazza_Balls$plate_z < Vic_Carapazza_Balls$sz_bot | Vic_Carapazza_Balls$plate_z > Vic_Carapazza_Balls$sz_top),'B','S')
table(Vic_Carapazza_Balls$AdjustedCall)
# Merge to create new dataset
Vic_Carapazza_AdjustedCalls = rbind(Vic_Carapazza_Strikes,Vic_Carapazza_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Vic_Carapazza)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Vic_Carapazza_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Vic_Carapazza_AdjustedCalls$AdjustedCall))



