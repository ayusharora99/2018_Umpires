# 9/21/18 : Giants vs Cardinals : Mike Estabrook

# 168 pitches were called strikes/balls

# The robot-ump called 46 of those pitches as called strikes & 122 as balls

# Mike Estabrook called 57 of those pitches as called strikes & 111 as balls 

# Accuracy: 90%

# File --> Import Dataset --> From text (base) (YES TO HEADER) & import csv file downloaded from https://baseballsavant.mlb.com/statcast_search
Mike_Estabrook <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Mike_Estabrook_9:21:18.csv")

# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Mike_Estabrook)
names(Mike_Estabrook)
is.na(Mike_Estabrook)
colSums(is.na(Mike_Estabrook)) 
Mike_Estabrook = Mike_Estabrook[,colSums(is.na(Mike_Estabrook)) == 0] 
dim(Mike_Estabrook)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Mike_Estabrook = Mike_Estabrook[ , !(names(Mike_Estabrook) %in% drops)]
dim(Mike_Estabrook)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Mike_Estabrook_train = Mike_Estabrook[0:(0.8 * nrow(Mike_Estabrook)),]
dim(Mike_Estabrook_train)
prop.table(table(Mike_Estabrook_train$type))
Mike_Estabrook_test = Mike_Estabrook[(0.8*nrow(Mike_Estabrook)):nrow(Mike_Estabrook),]
dim(Mike_Estabrook_test)
prop.table(table(Mike_Estabrook_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Mike_Estabrook_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Mike_Estabrook_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Mike_Estabrook_test$type))

# Subset for Borderline Calls
Mike_Estabrook$Borderline = ifelse(((abs(Mike_Estabrook$plate_x)> 0.748) & (abs(Mike_Estabrook$plate_x)<0.914)) 
                                & (((Mike_Estabrook$plate_z > Mike_Estabrook$sz_top-0.83) & (Mike_Estabrook$plate_z < Mike_Estabrook$sz_top+0.83))
                                   | (((Mike_Estabrook$plate_z)<Mike_Estabrook$sz_bot+0.83) & ((Mike_Estabrook$plate_z) > Mike_Estabrook$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike Types
Mike_Estabrook_Strikes = subset(Mike_Estabrook, Mike_Estabrook$type == "S")
Mike_Estabrook_Balls = subset(Mike_Estabrook, Mike_Estabrook$type == "B")

# Borderline
Mike_Estabrook_Borderline = subset(Mike_Estabrook, Mike_Estabrook$Borderline == "T")


# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Mike_Estabrook_Strikes$AdjustedCall = ifelse((Mike_Estabrook_Strikes$plate_x < 0.833 & Mike_Estabrook_Strikes$plate_x > -0.833) & (Mike_Estabrook_Strikes$plate_z > Mike_Estabrook_Strikes$sz_bot & Mike_Estabrook_Strikes$plate_z < Mike_Estabrook_Strikes$sz_top), 'S', 'B')
table(Mike_Estabrook_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Mike_Estabrook_Balls$AdjustedCall = ifelse((Mike_Estabrook_Balls$plate_x > 0.833 | Mike_Estabrook_Balls$plate_x < -0.833)|(Mike_Estabrook_Balls$plate_z < Mike_Estabrook_Balls$sz_bot | Mike_Estabrook_Balls$plate_z > Mike_Estabrook_Balls$sz_top),'B','S')
table(Mike_Estabrook_Balls$AdjustedCall)

# Borderline
Mike_Estabrook_Borderline$AdjustedCall = ifelse((Mike_Estabrook_Borderline$plate_x < 0.833 & Mike_Estabrook_Borderline$plate_x > -0.833) & (Mike_Estabrook_Borderline$plate_z > Mike_Estabrook_Borderline$sz_bot & Mike_Estabrook_Borderline$plate_z < Mike_Estabrook_Borderline$sz_top), 'S', 'B')

# Merge to create new dataset
Mike_Estabrook_AdjustedCalls = rbind(Mike_Estabrook_Strikes,Mike_Estabrook_Balls)
Mike_Estabrook_AdjustedCalls$OnFieldRuling = ifelse(Mike_Estabrook_AdjustedCalls$type == "S","S","B")

# Re-create Decision Tree but this time with whole Data rather than just training set.
tree_model <-rpart(type~., data = Mike_Estabrook)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy
Prediction_UMP<-predict(tree_model, newdata = Mike_Estabrook_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Mike_Estabrook_AdjustedCalls$AdjustedCall))

# Borderline
Prediction_BORDERLINE<-predict(tree_model, newdata = Mike_Estabrook_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Mike_Estabrook_Borderline$AdjustedCall))

# Correct vs InCorrect Call

# Correct Calls
Mike_Estabrook_AdjustedCalls$Call = ifelse( ((Mike_Estabrook_AdjustedCalls$type == 'B') & ( (Mike_Estabrook_AdjustedCalls$AdjustedCall == "B") | (Mike_Estabrook_AdjustedCalls$Borderline == "T") ) ), "C","I" )
Mike_Estabrook_AdjustedCalls$Call = ifelse( ((Mike_Estabrook_AdjustedCalls$type == 'S') & ((Mike_Estabrook_AdjustedCalls$AdjustedCall == "S") | (Mike_Estabrook_AdjustedCalls$Borderline == "T") ) ), "C","I")

# InCorrect Calls
Mike_Estabrook_AdjustedCalls$Call = ifelse( ( (Mike_Estabrook_AdjustedCalls$type == 'B') & ((Mike_Estabrook_AdjustedCalls$AdjustedCall == "S") & (Mike_Estabrook_AdjustedCalls$Borderline == "F") ) ), "I","C")
Mike_Estabrook_AdjustedCalls$Call = ifelse( ( (Mike_Estabrook_AdjustedCalls$type == 'S') & ((Mike_Estabrook_AdjustedCalls$AdjustedCall == "B") & (Mike_Estabrook_AdjustedCalls$Borderline == "F") ) ), "I","C")

table(Mike_Estabrook_AdjustedCalls$Call)

# Which Pitchers Recieved the InCorrect Calls
Mike_Estabrook_Incorrect = subset(Mike_Estabrook_AdjustedCalls, Mike_Estabrook_AdjustedCalls$Call == "I")
print(Mike_Estabrook_Incorrect$player_name)
print(Mike_Estabrook_Incorrect$AdjustedCall)
