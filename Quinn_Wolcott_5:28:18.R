# 5/28/18 : Giants vs Rockies : Quinn Wolcott

# 136 pitches were called strikes/balls

# The robot-ump called 38 of those pitches as called strikes & 98 as balls

# Quinn Wolcott called 45 of those pitches as called strikes & 91 as balls 

# Accuracy: 90% 


Quinn_Wolcott <- read.csv("~/Desktop/Analyzing Baseball Data with R/2018 Giants Umpires/Quinn_Wolcott_5:28:18.csv")


# Packages needed for Analysis
install.packages(c("e1071","caret","rpart"))

library(e1071)
library(caret)
library(rpart)

# Getting Familiar With Dataset & removing any NULL values
dim(Quinn_Wolcott)
names(Quinn_Wolcott)
is.na(Quinn_Wolcott)
colSums(is.na(Quinn_Wolcott)) 
Quinn_Wolcott = Quinn_Wolcott[,colSums(is.na(Quinn_Wolcott)) == 0] 
dim(Quinn_Wolcott)

# Subsetting Relevant Info
drops = c("event","des","hit_location","bb_type","on_3b","on_2b","on_1b","hc_x","hc_y","hit_distance_sc","launch_speed","launch_angle","estimated_ba_using_speedangle","estimated_woba_using_speedangle","woba_value","woba_denom","launch_speed_angle","iso_value","babip_value")
Quinn_Wolcott = Quinn_Wolcott[ , !(names(Quinn_Wolcott) %in% drops)]
dim(Quinn_Wolcott)

# Splitting data into Training (80% of data) & Testing (20% of data) sets
Quinn_Wolcott_train = Quinn_Wolcott[0:(0.8 * nrow(Quinn_Wolcott)),]
dim(Quinn_Wolcott_train)
prop.table(table(Quinn_Wolcott_train$type))
Quinn_Wolcott_test = Quinn_Wolcott[(0.8*nrow(Quinn_Wolcott)):nrow(Quinn_Wolcott),]
dim(Quinn_Wolcott_test)
prop.table(table(Quinn_Wolcott_test$type))


# Creating Decision Tree to Predict Umpire's Call 
tree_model <-rpart(type~., data = Quinn_Wolcott_train)
plot(tree_model)
text(tree_model, use.n = T)

# Testing Decision Tree with Test Data 
Prediction_UMP<-predict(tree_model, newdata = Quinn_Wolcott_test, type = 'class')

# Accuracy of Decision Tree created for specific Umpire
confusionMatrix(table(Prediction_UMP, Quinn_Wolcott_test$type))

# Subset for Borderline Calls
Quinn_Wolcott$Borderline = ifelse(((abs(Quinn_Wolcott$plate_x)> 0.748) & (abs(Quinn_Wolcott$plate_x)<0.914)) 
                               & (((Quinn_Wolcott$plate_z > Quinn_Wolcott$sz_top-0.83) & (Quinn_Wolcott$plate_z < Quinn_Wolcott$sz_top+0.83))
                                  | (((Quinn_Wolcott$plate_z)<Quinn_Wolcott$sz_bot+0.83) & ((Quinn_Wolcott$plate_z) > Quinn_Wolcott$sz_bot-0.83))), 'T','F')

# Copy Pitch Calls into another data set and adjust type to the electronic strike zone calls
# Seperate Ball & Strike & Borderline Types 
Quinn_Wolcott_Strikes = subset(Quinn_Wolcott, Quinn_Wolcott$type == "S")
Quinn_Wolcott_Balls = subset(Quinn_Wolcott, Quinn_Wolcott$type == "B")
Quinn_Wolcott_Borderline = subset(Quinn_Wolcott, Quinn_Wolcott$Borderline == "T")

# Create new column for adjusted call based on electronic strike zone on Umpire's called strikes 
# (plate_x < 0.833 & $plate_x > -0.833) & ($plate_z > sz_bot & plate_z < sz_top) == S
Quinn_Wolcott_Strikes$AdjustedCall = ifelse((Quinn_Wolcott_Strikes$plate_x < 0.833 & Quinn_Wolcott_Strikes$plate_x > -0.833) & (Quinn_Wolcott_Strikes$plate_z > Quinn_Wolcott_Strikes$sz_bot & Quinn_Wolcott_Strikes$plate_z < Quinn_Wolcott_Strikes$sz_top), 'S', 'B')
table(Quinn_Wolcott_Strikes$AdjustedCall)

# Create new column for adjusted call based on electronic strike zone on Umpire's called balls
# (plate_x > 0.833 | $plate_x < -0.833) | ($plate_z < sz_bot | plate_z > sz_top) == B
Quinn_Wolcott_Balls$AdjustedCall = ifelse((Quinn_Wolcott_Balls$plate_x > 0.833 | Quinn_Wolcott_Balls$plate_x < -0.833)|(Quinn_Wolcott_Balls$plate_z < Quinn_Wolcott_Balls$sz_bot | Quinn_Wolcott_Balls$plate_z > Quinn_Wolcott_Balls$sz_top),'B','S')
table(Quinn_Wolcott_Balls$AdjustedCall)

Quinn_Wolcott_Borderline$AdjustedCall = ifelse((Quinn_Wolcott_Borderline$plate_x < 0.833 & Quinn_Wolcott_Borderline$plate_x > -0.833) & (Quinn_Wolcott_Borderline$plate_z > Quinn_Wolcott_Borderline$sz_bot & Quinn_Wolcott_Borderline$plate_z < Quinn_Wolcott_Borderline$sz_top), 'S', 'B')


# Merge to create new dataset
Quinn_Wolcott_AdjustedCalls = rbind(Quinn_Wolcott_Strikes,Quinn_Wolcott_Balls)
tree_model <-rpart(type~., data = Quinn_Wolcott)
plot(tree_model)
text(tree_model, use.n = T)
# Predict using Umpire's Decision Tree on the AdjustedCalls dataset & compare calls with adjusted_call to find Accuracy

Prediction_UMP<-predict(tree_model, newdata = Quinn_Wolcott_AdjustedCalls, type = 'class')
confusionMatrix(table(Prediction_UMP,Quinn_Wolcott_AdjustedCalls$AdjustedCall))

Prediction_BORDERLINE<-predict(tree_model, newdata = Quinn_Wolcott_Borderline, type = 'class')
confusionMatrix(table(Prediction_BORDERLINE,Quinn_Wolcott_Borderline$AdjustedCall))









