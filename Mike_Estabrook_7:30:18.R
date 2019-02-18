#7/30/18: Giants vs Padres : Mike Estabrook

#196 pitches were called strikes/balls

#The robot-ump called 65 of those pitches as called strikes & 131 as balls

#Mike Estabrook called 81 of those pitches as called strikes & 115 as balls 

#Accuracy: 88%

Mike_Estabrook <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mike_Estabrook_7:30:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Mike_Estabrook)
names(Mike_Estabrook)
is.na(Mike_Estabrook)
colSums(is.na(Mike_Estabrook)) 
Mike_Estabrook = Mike_Estabrook[,colSums(is.na(Mike_Estabrook)) == 0] 
dim(Mike_Estabrook)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mike_Estabrook = Mike_Estabrook[ , !(names(Mike_Estabrook) %in% drops)]
# Mike_Estabrook = Mike_Estabrook[keep]
dim(Mike_Estabrook)
Mike_Estabrook

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Mike_Estabrook_train = Mike_Estabrook[0:(0.8 * nrow(Mike_Estabrook)),]
dim(Mike_Estabrook_train)
prop.table(table(Mike_Estabrook_train$type))
Mike_Estabrook_test = Mike_Estabrook[(0.8*nrow(Mike_Estabrook)):nrow(Mike_Estabrook),]
dim(Mike_Estabrook_test)
prop.table(table(Mike_Estabrook_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mike_Estabrook_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Mike_Estabrook_test, type = 'class')

confusionMatrix(table(prediction_BG, Mike_Estabrook_test$type))
confusionMatrix(prediction_BG,Mike_Estabrook_test$type)
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
Mike_Estabrook_Strikes = subset(Mike_Estabrook, Mike_Estabrook$type == "S")
Mike_Estabrook_Balls = subset(Mike_Estabrook, Mike_Estabrook$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mike_Estabrook_Strikes$AdjustedCall = ifelse((Mike_Estabrook_Strikes$plate_x < 0.833 & Mike_Estabrook_Strikes$plate_x > -0.833) & (Mike_Estabrook_Strikes$plate_z > Mike_Estabrook_Strikes$sz_bot & Mike_Estabrook_Strikes$plate_z < Mike_Estabrook_Strikes$sz_top), 'S', 'B')
table(Mike_Estabrook_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mike_Estabrook_Balls$AdjustedCall = ifelse((Mike_Estabrook_Balls$plate_x > 0.833 | Mike_Estabrook_Balls$plate_x < -0.833)|(Mike_Estabrook_Balls$plate_z < Mike_Estabrook_Balls$sz_bot | Mike_Estabrook_Balls$plate_z > Mike_Estabrook_Balls$sz_top),'B','S')
table(Mike_Estabrook_Balls$AdjustedCall)
# Merge to create new dataset
Mike_Estabrook_AdjustedCalls = rbind(Mike_Estabrook_Strikes,Mike_Estabrook_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Mike_Estabrook)
plot(tree_model)
text(tree_model, use.n = T)
prediction_BG<-predict(tree_model, newdata = Mike_Estabrook_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_BG,Mike_Estabrook_AdjustedCalls$AdjustedCall))



