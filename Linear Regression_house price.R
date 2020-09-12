#Linear Regression on Housing Price Prediction
library(ggplot2)
library(ggthemes) 
library(scales)
library(dplyr) 
library(gridExtra)
library(corrplot) 
library(GGally)
library(e1071)
library(DAAG)
library(caret)

#read data file
setwd("C:/Users/hp/Desktop/DSP/New folder (2)")
data= read.csv("Property_Price_Train.csv", na.strings=c("","NA"))
#View(data.train)

#Str of train data.
dim(data)
str(data)
summary(data)


#removing id as it has unique values
data <- data[-1]

# #detecting and treating missing values
colSums(is.na(data)) 

#% Missing analysis
Missing_index <- data %>% is.na() %>% colMeans() * 100
Missing_index <- sort(round(Missing_index[Missing_index > 0], 2), decreasing  = TRUE)
View(Missing_index)
#barplot(Missing_index)

# % of NA's in dataframe
sum(is.na(data))/prod(dim(data)) *100


# % of NA's contains as row
nrow(data[!complete.cases(data),])/nrow(data) *100


#droping columns having more 75%
drop <- names(Missing_index[c(1:4)])
data <- data[(!colnames(data) %in% drop)]
dim(data)


#segregating Numeric & factor data
data.numeric <- data[sapply(data, is.numeric)]
data.factor <- data[sapply(data, is.factor)]

dim(data.numeric)
dim(data.factor)


#Numerical Data analysis for cleaning
str(data.numeric)
summary(data.numeric[11:16])


#Numeric to factor conversion
# Overall_Material
# House_Condition
# Kitchen_Above_Grade
# Rooms_Above_Grade
# Fireplaces
# Garage_Size
# Month_Sold
# "Underground_Full_Bathroom",
# "Underground_Half_Bathroom",
# "Full_Bathroom_Above_Grade",
# "Half_Bathroom_Above_Grade",
# "Bedroom_Above_Grade",   


data$Overall_Material <- as.factor(data$Overall_Material)
data$House_Condition <- as.factor(data$House_Condition)
data$Kitchen_Above_Grade <- as.factor(data$Kitchen_Above_Grade)
data$Rooms_Above_Grade <- as.factor(data$Rooms_Above_Grade)
data$Fireplaces <- as.factor(data$Fireplaces)
data$Garage_Size <- as.factor(data$Garage_Size)
data$Building_Class <- as.factor(data$Building_Class)
data$Underground_Full_Bathroom<- as.factor(data$Underground_Full_Bathroom)
data$Underground_Half_Bathroom   <- as.factor(data$Underground_Half_Bathroom)
data$Full_Bathroom_Above_Grade   <- as.factor(data$Full_Bathroom_Above_Grade)
data$Half_Bathroom_Above_Grade   <- as.factor(data$Half_Bathroom_Above_Grade)
data$Bedroom_Above_Grade   <- as.factor(data$Bedroom_Above_Grade)
data$Month_Sold<-as.factor(data$Month_Sold)

#factor data analysis for cleaning

str(data.factor)
summary(data.factor)

#Highly biased or same value data.
# Utility_Type
# Road_Type


data <- data[(!colnames(data) %in% c("Utility_Type","Road_Type"))]
dim(data)

#Replacing na values as per the dataset descriptiption

data$Basement_Height<-factor(data$Basement_Height,levels=c(levels(data$Basement_Height),'noBsmt'))
data$Basement_Height[is.na(data$Basement_Height)]<-'noBsmt'

data$Basement_Condition<-factor(data$Basement_Condition,levels=c(levels(data$Basement_Condition),'noBsmt'))
data$Basement_Condition[is.na(data$Basement_Condition)]<-'noBsmt'

data$Exposure_Level<-factor(data$Exposure_Level,levels=c(levels(data$Exposure_Level),'noBsmt'))
data$Exposure_Level[is.na(data$Exposure_Level)]<-'noBsmt'

data$BsmtFinType1<-factor(data$BsmtFinType1,levels=c(levels(data$BsmtFinType1),'noBsmt'))
data$BsmtFinType1[is.na(data$BsmtFinType1)]<-'noBsmt'

data$BsmtFinType2<-factor(data$BsmtFinType2,levels=c(levels(data$BsmtFinType2),'noBsmt'))
data$BsmtFinType2[is.na(data$BsmtFinType2)]<-'noBsmt'

data$Garage=factor(data$Garage,levels=c(levels(data$Garage),"No garage"))
data$Garage[is.na(data$Garage)]="No garage"

data$Garage_Finish_Year=factor(data$Garage_Finish_Year,levels = c(levels(data$Garage_Finish_Year),"No garage"))
data$Garage_Finish_Year[is.na(data$Garage_Finish_Year)]="No garage"

data$Garage_Quality=factor(data$Garage_Quality,levels = c(levels(data$Garage_Quality),"No garage"))
data$Garage_Quality[is.na(data$Garage_Quality)]="No garage"

data$Garage_Condition=factor(data$Garage_Condition,levels = c(levels(data$Garage_Condition),"No garage"))
data$Garage_Condition[is.na(data$Garage_Condition)]="No garage"

data$Fireplace_Quality=factor(data$Fireplace_Quality,levels = c(levels(data$Fireplace_Quality),"No fireplace"))
data$Fireplace_Quality[is.na(data$Fireplace_Quality)]="No fireplace"


#Again segregating Numeric & factor data
data.numeric <- data[sapply(data, is.numeric)]
data.factor <- data[sapply(data, is.factor)]

dim(data.numeric)
dim(data.factor)

#checking Na's in target value
any(is.na(data.numeric$Sale_Price))


#Imputation of NAs with mean for numeric variables
for(i in seq(data.numeric)) {
     data.numeric[i]<- ifelse(is.na(data.numeric[,i]), 
                              median(data.numeric[,i], na.rm = T), data.numeric[,i])
}



#Imputation of NAs with mode for categorical variables

#mode function
getmode <- function(x) {
     x <- x[!is.na(x)]
     uniqv <- unique(x)
     uniqv[which.max(tabulate(match(x, uniqv)))]
}


#imputation with mode
for(i in seq(data.factor))
     data.factor[,i][is.na(data.factor[,i])] <- getmode(data.factor[,i])

str(data.factor)
summary(data.factor)


#Exploratory data analysis
#Analysing histogram of each numeric values
numplot <- function(column, df)
{
     ggplot(df, aes_string(x=column))+
          geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
          geom_density(fill='blue', alpha=0.2)+
          xlab(column)
}


np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
do.call("grid.arrange", np)


#Checking skewness
data.skewed <- apply(data.numeric, c(2), skewness)


colnames(data.numeric)

#Dropping the variables having majority values as zero or zero mean 
drops <- data.numeric[c("LowQualFinSF",
                        "BsmtFinSF2",
                        "Three_Season_Lobby_Area",  
                        "Screen_Lobby_Area",        
                        "Pool_Area",                
                        "Miscellaneous_Value")] 


np <- lapply(colnames(drops), numplot, df=drops)
do.call("grid.arrange", np)

summary(drops)
View(drops)


data.numeric <- data.numeric[(!colnames(data.numeric) %in% colnames(drops))]
dim(data.numeric)



#Checking for Outlier
out_std_check = function(x){
     m=mean(x)
     s=sd(x)
     lc=m-3*s #lower cut-off
     uc=m+3*s
     n=list( val=sum(x>uc | x<lc), lc=lc, uc=uc)
     return(n)
}

np <- apply(data.numeric, c(2), out_std_check)
np

#Outlier treatment-imputing outliers with mean
out_std_fix = function(x){
     m=mean(x)
     s=sd(x)
     lc=m-3*s #lower cut-off
     uc=m+3*s
     out_value <- which(x > uc | x < lc)
     x[out_value] <- m
     return(x)
}


data.numeric <- apply(data.numeric, c(2), out_std_fix)
data.numeric <- as.data.frame(data.numeric)
summary(data.numeric)


np <- lapply(colnames(data.numeric), numplot, df=data.numeric)
do.call("grid.arrange", np)

apply(data.numeric, c(2), skewness) 

data.numeric$Brick_Veneer_Area<-log(data.numeric$Brick_Veneer_Area+1)
data.numeric$Lot_Size<-log(data.numeric$Lot_Size)
data.numeric$Sale_Price<-log(data.numeric$Sale_Price)

summary(data.numeric)
corrplot::corrplot(cor(data.numeric))
corrplot.mixed(cor(data.numeric), lower.col = "black", number.cex = .7)

colnames(data.numeric)
#removing variables based on correlation
data.numeric <- data.numeric[!colnames(data.numeric) %in% c("Garage_Area","W_Deck_Area","Open_Lobby_Area","Enclosed_Lobby_Area")]



#Now factor analysis

#bar plot for categorical varibale etc.

factplot <- function(column, df)
{
     ggplot(df, aes_string(x=column))+
          geom_bar(fill = "blue", color = "black", alpha= 0.2)+
          xlab(column)
}

#calling all bar plot
fp <- lapply(colnames(data.factor), factplot, df=data.factor)
do.call("grid.arrange", fp)

#dropping highly biased or same value variables

drps <- c("Land_Outline", "Property_Slope", "Condition1","Condition2" 
          ,"House_Type", "Roof_Quality","Heating_Type" ,
          "BsmtFinType2","Functional_Rate", "Kitchen_Above_Grade",
          "Garage_Quality","Garage_Condition")


data.factor <- data.factor[!colnames(data.factor) %in% drps]

#Feature engineering
data.numeric$Age<-data.numeric$Year_Sold-data.numeric$Construction_Year
data.numeric$YearsSinceRemod<-data.numeric$Year_Sold-data.numeric$Remodel_Year
data.factor$RemodeledHouse<-as.factor(ifelse(data.numeric$YearsSinceRemod>1,'Yes','No'))

data<- cbind(data.factor,data.numeric)

#Bivariate data analysis
#Categorical variables versus dependent variable


catnumplot <- function(df,column)
{
        ggplot(df, aes_string(x=column,y=exp(df$Sale_Price)/1000))+
                geom_point(alpha=0.4)+
                xlab(column)+ylab("Sale price in $")

}
#calling all bar plot
fp <- lapply(df=data,colnames(data.factor), catnumplot)
do.call("grid.arrange", fp)


#Numeric variables versus dependent variable
numtonumplot <- function(column, df)
{
        ggplot(df, aes_string(x=column,y=exp(df$Sale_Price)/1000))+
                geom_point()+geom_smooth()+
                xlab(column)+ylab("Sale price in $")
}


np <- lapply(colnames(data.numeric), numtonumplot, df=data)
do.call("grid.arrange", np)

summary(data.numeric)
colnames(data.factor)

#Analyzing new features
summary(data$RemodeledHouse)
#85% of the houses were remodeled

#Analysisng Age and YearsSinceRemod
np <- lapply(colnames(data.numeric[15:16]), numtonumplot, df=data)
do.call("grid.arrange", np)
#As expected the cost of house goes down with age and recently remodeled houses have higher sale price

library(dummies)
data.factor <- dummy.data.frame(data.factor)
colnames(data.factor)


data<- cbind(data.factor,data.numeric)

      
str(data)
dim(data)

colnames(data)

#sampling
set.seed(90)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
dim(train)
dim(test)
colnames(test1)
test1 <- test[!colnames(test) %in% "Sale_Price"]

options(max.print = 9999)
#Applying lm model
model <- lm(Sale_Price~., data=train)
summary(model)

# RMSE       Rsquared  MAE      
#0.1624455  0.811399  0.1123345

#Stepwise modeling

#stepwise sleection:

nullModel<- lm(Sale_Price ~ 1, train)

#summary(nullModel)

fullmodel <- lm(Sale_Price~.,data = (train))
#summary(fullmodel)

fit <- step(nullModel, scope=list(lower=nullModel, upper=fullmodel), direction="both")

#revel the model
fit
model<-lm(formula = Sale_Price ~ Grade_Living_Area + Construction_Year + 
            Total_Basement_Area + Remodel_Year + Fireplaces0 + `GarageNo garage` + 
            NeighborhoodCrawfor + BsmtFinSF1 + Overall_Material8 + House_Condition3 + 
            Overall_Material7 + Lot_Size + House_Condition4 + House_Condition5 + 
            Overall_Material9 + Overall_Material6 + Building_Class30 + 
            NeighborhoodClearCr + Sale_ConditionAbnorml + Rooms_Above_Grade11 + 
            Overall_Material2 + Zoning_ClassFVR + Exterior1stBrkFace + 
            Bedroom_Above_Grade8 + Garage_Size1 + GarageCarPort + Rooms_Above_Grade12 + 
            Building_Class75 + Zoning_ClassRLD + Overall_Material1 + 
            Underground_Full_Bathroom0 + Second_Floor_Area + First_Floor_Area + 
            Building_Class160 + Air_ConditioningN + Sale_TypeConLI + 
            Exterior1stBrkComm + NeighborhoodNridgHt + House_Condition9 + 
            Bedroom_Above_Grade1 + Building_Class60 + `Exterior2ndWd Shng` + 
            Underground_Full_Bathroom3 + NeighborhoodOldTown + Electrical_SystemMix + 
            Zoning_ClassCommer + Foundation_TypeS + House_Condition2 + 
            Fireplace_QualityEx + Building_Class90 + Basement_ConditionFa + 
            Foundation_TypeW + BsmtFinType1GLQ + Underground_Full_Bathroom2 + 
            Month_Sold1 + Full_Bathroom_Above_Grade0 + Lot_Extent + Kitchen_QualityEx + 
            Roof_DesignFlat + Brick_Veneer_TypeBrkCmn + Exterior_MaterialEx + 
            NeighborhoodTimber + Building_Class120 + Month_Sold4 + Month_Sold3 + 
            Bedroom_Above_Grade6 + House_Condition6 + Electrical_SystemSBrkr + 
            Exterior_ConditionEx + Garage_Built_Year + Garage2TFes + 
            Brick_Veneer_TypeNone + NeighborhoodNAmes + NeighborhoodEdwards + 
            Zoning_ClassRHD + Exterior1stHdBoard + Exposure_LevelnoBsmt + 
            Rooms_Above_Grade10 + Garage_Size3 + Heating_QualityPo + 
            RemodeledHouseNo + Exposure_LevelNo + House_Design1Story + 
            House_Design1.5Unf + Exterior2ndCmentBd + Exterior1stCemntBd + 
            NeighborhoodMeadowV + NeighborhoodStoneBr + Half_Bathroom_Above_Grade0 + 
            Exterior2ndPlywood + Overall_Material3 + Month_Sold2 + Rooms_Above_Grade5 + 
            GarageBasment + Sale_TypeCWD + Kitchen_QualityFa + Electrical_SystemFuseP + 
            Roof_DesignGable + Month_Sold10, data = train)
summary(model)

#using regularization
library(glmnet)
x <- as.matrix(train[!colnames(train) %in% "Sale_Price"])
y<-train$Sale_Price
z<-as.matrix(test1)

#Ridge model
ridge_model <- cv.glmnet(x,y, alpha = 0,lambda = seq(0.0001,1,length=5))
ridge_model$lambda.1se
best_lambda=ridge_model$lambda.1se
ridge_coeff=ridge_model$glmnet.fit$beta[,ridge_model$glmnet.fit$lambda==best_lambda]

ridge_feat <- names(ridge_coeff[ridge_coeff > 0])
ridge_model <- glmnet(x,y, alpha = 0, lambda = best_lambda)

#lasso model
lasso_model <- cv.glmnet(x,y, alpha = 1,lambda = seq(0.0001,1,length=5))
lasso_model$lambda.1se
best_lambda=lasso_model$lambda.1se
lasso_coeff=lasso_model$glmnet.fit$beta[,lasso_model$glmnet.fit$lambda==best_lambda]

lasso_feat <- names(lasso_coeff[lasso_coeff > 0])
lasso_model <- glmnet(x,y, alpha = 1, lambda = best_lambda)



#Prediction and RMSE
lmpred <- predict(model, test1)
RMSE(lmpred,test$Sale_Price)
#RMSE=0.1535619

ridgepred <- predict(ridge_model, z)
RMSE(ridgepred,test$Sale_Price)
#RMSE=0.1548363

lassopred <- predict(lasso_model, z)
RMSE(lassopred,test$Sale_Price)
#RMSE=0.1562365


