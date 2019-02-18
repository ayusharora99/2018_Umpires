#8/7/18: Giants vs Astros : Sean Barber

# 141 pitches were called strikes/balls

# The robot-ump called 41 of those pitches as called strikes & 100 as balls

# Sean Barber called 49 of those pitches as called strikes & 92 as balls 

# Accuracy: 89%

Sean_Barber <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Sean_Barber_8:7:18.csv")

install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

dim(Sean_Barber)
names(Sean_Barber)
is.na(Sean_Barber)
colSums(is.na(Sean_Barber)) 
Sean_Barber = Sean_Barber[,colSums(is.na(Sean_Barber)) == 0] 
dim(Sean_Barber)

# Subsetting Relevant Info
keeps = c("at_bat_number","player_name","pitch_type","plate_x","plate_z","zone","sz_top","sz_bot","type")
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Sean_Barber = Sean_Barber[ , !(names(Sean_Barber) %in% drops)]
# Sean_Barber = Sean_Barber[keep]
dim(Sean_Barber)
Sean_Barber

# Split Testing(28) & Training(108) data
# USE n fold cross validation or bootstrap if needed
Sean_Barber_train = Sean_Barber[0:(0.8 * nrow(Sean_Barber)),]
dim(Sean_Barber_train)
prop.table(table(Sean_Barber_train$type))
Sean_Barber_test = Sean_Barber[(0.8*nrow(Sean_Barber)):nrow(Sean_Barber),]
dim(Sean_Barber_test)
prop.table(table(Sean_Barber_test$type))
# Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Sean_Barber_train)
plot(tree_model)
text(tree_model, use.n = T)
prediction_SB<-predict(tree_model, newdata = Sean_Barber_test, type = 'class')

confusionMatrix(table(prediction_SB, Sean_Barber_test$type))
confusionMatrix(prediction_SB,Sean_Barber_test$type)
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
Sean_Barber_Strikes = subset(Sean_Barber, Sean_Barber$type == "S")
Sean_Barber_Balls = subset(Sean_Barber, Sean_Barber$type == "B")

# Adjust call for Strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Sean_Barber_Strikes$AdjustedCall = ifelse((Sean_Barber_Strikes$plate_x < 0.833 & Sean_Barber_Strikes$plate_x > -0.833) & (Sean_Barber_Strikes$plate_z > Sean_Barber_Strikes$sz_bot & Sean_Barber_Strikes$plate_z < Sean_Barber_Strikes$sz_top), 'S', 'B')
table(Sean_Barber_Strikes$AdjustedCall)
# Adjust call for Balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Sean_Barber_Balls$AdjustedCall = ifelse((Sean_Barber_Balls$plate_x > 0.833 | Sean_Barber_Balls$plate_x < -0.833)|(Sean_Barber_Balls$plate_z < Sean_Barber_Balls$sz_bot | Sean_Barber_Balls$plate_z > Sean_Barber_Balls$sz_top),'B','S')
table(Sean_Barber_Balls$AdjustedCall)
# Merge to create new dataset
Sean_Barber_AdjustedCalls = rbind(Sean_Barber_Strikes,Sean_Barber_Balls)

# Predict using Hunter Wendelstedt's Decision Tree on the modified dataset & map with adjusted_call to find margin of error
tree_model <-rpart(type~., data = Sean_Barber)
plot(tree_model)
text(tree_model, use.n = T)
prediction_SB<-predict(tree_model, newdata = Sean_Barber_AdjustedCalls, type = 'class')
confusionMatrix(table(prediction_SB,Sean_Barber_AdjustedCalls$AdjustedCall))



