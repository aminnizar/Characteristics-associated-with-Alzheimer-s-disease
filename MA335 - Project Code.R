#Required packages
library(plyr)
library(dplyr)
library(tidyverse)
library(vtable)
library(na.tools)
library(psych)
library(gridExtra)
library(grid)
library(patchwork)
library(GGally)
library(tableone)
library(xtable)
library(ggplot2)
library(scales)
library(knitr)
library(corrplot)
library(randomForest)
library(faraway)
library(caret)
library(nnet)
library(MASS)
library(factoextra)
library(Hmisc)
library(VIM)
library(gridExtra)

#DATA2R
house_data <- read.csv("C:/Users/Ryan/Documents/University of Essex/MA335 - Modelling Experimental and Observational Data/Assessments/2.0/house_data.csv")
attach(house_data)
detach(house_data)

#IMPUTTING MISSING DATA & LABEL ENCODING
NAcol <- which(colSums(is.na(house_data)) > 0)
sort(colSums(sapply(house_data[NAcol], is.na)), decreasing = TRUE)
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
#POOL QUALITY (Na's & FACTOR2ORDINAL)
house_data$PoolQC[is.na(house_data$PoolQC)] <- 'None'
house_data$PoolQC<-as.integer(revalue(house_data$PoolQC, Qualities))
#MISCELLANEOUS FEATURES (Na's & CHARACTER2FACTOR))
house_data$MiscFeature[is.na(house_data$MiscFeature)] <- 'None'
house_data$MiscFeature <- as.factor(house_data$MiscFeature)
#ALLEY (Na's & CHARACTER2FACTOR)
house_data$Alley[is.na(house_data$Alley)] <- 'None'
house_data$Alley <- as.factor(house_data$Alley)
#FENCE (Na's & CHARACTER2FACTOR)
house_data$Fence[is.na(house_data$Fence)] <- 'None'
house_data$Fence <- as.factor(house_data$Fence)
#LOT FRONTAGE (Na's)
for (i in 1:nrow(house_data)){
  if(is.na(house_data$LotFrontage[i])){
    house_data$LotFrontage[i] <- as.integer(median(house_data$LotFrontage[house_data$Neighborhood==house_data$Neighborhood[i]], na.rm=TRUE)) 
  }
}
#GARAGE TYPE (Na's & CHARACTER2FACTOR)
house_data$GarageType[is.na(house_data$GarageType)] <- 'No Garage'
house_data$GarageType <- as.factor(house_data$GarageType)
#GARAGE CONDITION (Na's & FACTOR2ORDINAL)
house_data$GarageCond[is.na(house_data$GarageCond)] <- 'None'
house_data$GarageCond<-as.integer(revalue(house_data$GarageCond, Qualities))
#BASEMENT QUALITY (Na's & FACTOR2ORDINAL)
house_data$BsmtQual[is.na(house_data$BsmtQual)] <- 'None'
house_data$BsmtQual<-as.integer(revalue(house_data$BsmtQual, Qualities))
#BASEMENT CONDITION (Na's & FACTOR2ORDINAL)
house_data$BsmtCond[is.na(house_data$BsmtCond)] <- 'None'
house_data$BsmtCond<-as.integer(revalue(house_data$BsmtCond, Qualities))
#MASONRY VENEER AREA (Na's)
house_data$MasVnrArea[is.na(house_data$MasVnrArea)] <-0
#KITCHEN QUALITY (FACTOR2ORDINAL)
house_data$KitchenQual<-as.integer(revalue(house_data$KitchenQual, Qualities))
#HOME FUNCTIONALITY (FACTOR2ORDINAL)
house_data$Functional <- as.integer(revalue(house_data$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
#EXTERIOR QUALITY (FACTOR2ORDINAL)
house_data$ExterQual<-as.integer(revalue(house_data$ExterQual, Qualities))
#EXTERIOR CONDITION (FACTOR2ORDINAL)
house_data$ExterCond<-as.integer(revalue(house_data$ExterCond, Qualities))
#SALE TYPE (CHARACTER2FACTOR)
house_data$SaleType <- as.factor(house_data$SaleType)
#SALE CONDITION (CHARACTER2FACTOR)
house_data$SaleCondition <- as.factor(house_data$SaleCondition)
#FOUNDATION (CHARACTER2FACTOR)
house_data$Foundation <- as.factor(house_data$Foundation)
#HEATING (CHARACTER2FACTOR)
house_data$Heating <- as.factor(house_data$Heating)
#ROOF STYLE (CHARACTER2FACTOR)
house_data$RoofStyle <- as.factor(house_data$RoofStyle)
#ROOF MATERIAL (CHARACTER2FACTOR)
house_data$RoofMatl <- as.factor(house_data$RoofMatl)
#BUILDING TYPE (CHARACTER2FACTOR)
house_data$BldgType <- as.factor(house_data$BldgType)
#HOUSE STYLE (CHARACTER2FACTOR))
house_data$HouseStyle <- as.factor(house_data$HouseStyle)
#NEIGHBORHOOD (CHARACTER2FACTOR)
house_data$Neighborhood <- as.factor(house_data$Neighborhood)
#CONDITION1 (CHARACTER2FACTOR)
house_data$Condition1 <- as.factor(house_data$Condition1)
#CONDITION2 (CHARACTER2FACTOR)
house_data$Condition2 <- as.factor(house_data$Condition2)
#STREET (CHARACTER2ORDINAL)
house_data$Street <- as.integer(revalue(house_data$Street, c('Grvl' = 0, 'Pave' = 1)))
#PAVED DRIVEWAY (CHARACTER2ORDINAL)
house_data$PavedDrive <- as.integer(revalue(house_data$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
#EXTERIOR1ST (CHARACTER2FACTOR)
house_data$Exterior1st <- as.factor(house_data$Exterior1st)
#LOT CONFIGURATION
house_data$LotConfig <- as.factor(house_data$LotConfig)
#MONTH SOLD (INTEGER2FACTOR)
house_data$MoSold <- as.factor(house_data$MoSold)
#YEAR SOLD (INTEGER2FACTOR)
house_data$YrSold <- as.factor(house_data$YrSold)
#REMOVING UTILITIES VARIABLE
house_data$Utilities <- NULL

#1(EDA)

length(select_if(house_data,is.numeric))

#Summary Statistics Table
house_data %>%
  dplyr::select(SalePrice, OverallCond, OverallQual, Bedrooms, FullBath, LotFrontage, LotArea, YearBuilt, GrLivArea, TotalBsmtSF, TotRmsAbvGrd, Fireplaces, GarageArea) %>%
  sumtable()
sumtable(house_data)

#Bedrooms~SalesPrice 
eda1<- ggplot(house_data, aes(Bedrooms,SalePrice)) +
  scale_y_continuous(labels = comma) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
  xlab("Number of Bedrooms") + ylab("Sale Price")

#Bathrooms~SalesPrice
eda2<-ggplot(house_data, aes(FullBath,SalePrice)) +
  scale_y_continuous(labels = comma) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
xlab("Number of Bathrooms") + ylab("Sale Price")

#TotalBasementArea~SalePrice
eda3<- ggplot(house_data, aes(TotalBsmtSF,SalePrice)) +
  scale_y_continuous(labels = comma) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
xlab("Total square feet of Basement area") + ylab("Sale Price")

#Sale Price Frequency
eda8 <- ggplot(house_data, aes(SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  xlab("Sale Price") + ylab("Frequency")

#Frequency Overall Quality
ggplot(house_data, aes(OverallQual)) +
  scale_x_continuous(n.breaks = 10) +
  geom_bar() +
  xlab("Overall Quality") + ylab("Frequency")

#Quality~SalePrice
eda4<- ggplot(house_data, aes(OverallQual,SalePrice)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(n.breaks = 10) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
  xlab("Overall Quality") + ylab("Sale Price")

#Condition~SalePrice
ggplot(house_data, aes(OverallCond,SalePrice)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(n.breaks = 10) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
  xlab("Overall Condition") + ylab("Sale Price")

#LotArea~SalePrice
eda5<- ggplot(house_data, aes(LotArea,SalePrice)) +
  scale_y_continuous(labels = comma) +
  xlim(0,75000) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
  xlab("Lot Area") + ylab("Sale Price")

#YearBuilt~SalePrice
eda6<- ggplot(house_data, aes(YearBuilt,SalePrice)) +
  scale_y_continuous(labels = comma) +
  scale_x_binned()+
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
  xlab("Year Built") + ylab("Sale Price") 

#YearSold~SalePrice
ggplot(house_data, aes(YrSold,SalePrice)) +
    scale_y_continuous(labels = comma) +
    geom_jitter() +
    geom_smooth(method = "lm", col = "red") +
  xlab("Year Sold") + ylab("Sale Price") 
  
#ExteriorCondition~SalePrice
ggplot(house_data, aes(ExterCond, SalePrice)) +
  scale_y_continuous(labels = comma) +
  geom_jitter() +
  geom_smooth(method = "lm", col = "red") +
  xlab("Exterior Condition") + ylab("Sale Price") 

#NEIGHBOURHOOD MEDIAN HOUSE PRICES
eda7 <- ggplot(house_data, aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Mean SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma)

grid.arrange(eda1,eda2,eda3,eda4,eda5,eda6, nrow = 2)
grid.arrange(eda8,eda7)

#CORRELATION MATRIX
#selecting numeric variables to use
numericVars <- select_if(house_data, is.numeric) 
numericVarNames <- names(numericVars)
#correlations of all numeric variables
cor_numVar <- cor(numericVars, use="pairwise.complete.obs") 
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high correlations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
#plotting correlation matrix
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


#2 (LINEAR REGRESSION)

#full model
model1 <- lm(SalePrice~.,data=numericVars)
#model output
summary(model1)
#checking for collinearity
vif(model1)
#reduced model
model2 <- lm(SalePrice~Neighborhood+GrLivArea+OverallQual+OverallCond+GarageArea+TotalBsmtSF+BsmtQual+LotArea+Fireplaces)
#reduced model output
summary(model2)
#plotting model output
plot(model2)
#checking for collinearity 
vif(model2)
#comparing full and reduced model 
anova(model1,model2)

#3 (LR, QDA & LDA)

#PREPARING DATA 4 MODELLING

#Overall Condition (NUMERICAL2FACTOR)
classifyds <- house_data
attach(classifyds)
detach(classifyds)
#Dropping highly correlated variables & redundant variables
dropvars <- c('Id', 'GarageCond', 'MiscVal', 'SaleType')
classifyds <- classifyds[,!(names(classifyds) %in% dropvars)]
#Removing Outliers
classifyds <- classifyds[-c(524,1299)]
#Selecting only numeric variables
classifyds <- classifyds[,sapply(classifyds, is.numeric)]
#adding OverallCond to dataset
classifyds$OverallCond <- house_data$OverallCond
#changing OverallCond to factor with 3 levels 
classifyds$OverallCond=cut(classifyds$OverallCond, br=c(0,3,6,10), labels = c("Poor", "Average", "Good"))

#LOGISITC REGRESSION 

set.seed(123)
#splitting data into training and test set
training.samples <- classifyds$OverallCond %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- classifyds[training.samples, ]
test.data <- classifyds[-training.samples, ]
#fitting the model
LogRegModel <- nnet::multinom(OverallCond ~., data = train.data)
#model output
summary(LogRegModel)
str(LogRegModel)
#predictions 
predicted.classes <- LogRegModel %>% predict(test.data)
#model accuracy 
mean(predicted.classes == test.data$OverallCond)
#error rate
error <- mean(test.data$OverallCond != predicted.classes)
error

#LINEAR DISCRIMINANT ANALYSIS

#normalising data & estimating preprocessing params
preproc.param <- train.data %>%
  preProcess(method = c("center", "scale"))
#transforming data using estimated params
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)
#model fitting
ldamodel <- lda(OverallCond~., data = train.transformed)
summary(ldamodel)
#predictions
predictions <- ldamodel %>% predict(test.transformed)
#model accuracy
mean(predictions$class==test.transformed$OverallCond)

names(predictions)
head(predictions$class)
head(predictions$posterior)
head(predictions$x)

#VISUALISING LDA
lda.data <- cbind(train.transformed, predict(ldamodel)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = OverallCond))


#4 (CLUSTERING ALGORITHMS)

clusteringdf <- classifyds
clusteringdf <- subset(clusteringdf, select = -c(OverallCond, Bedrooms))
clusteringdf <- scale(clusteringdf)

#Heirarchical Kmeans clustering 

#determining optimal clusters
fviz_nbclust(clusteringdf, kmeans, method = "wss")+ 
  geom_vline(xintercept = 4, linetype = 2)	
#performing HKmeans
res.hk <-hkmeans(clusteringdf, 4, "euclidean")
#dendogram plot
FVIZDEND <- fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)
#cluster plot
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())

#VARIABLE CLUSTERING 

#variable clustering using spearman method
clus_var <- varclus(as.matrix(clusteringdf), similarity = "spearman", minlev = 0.05)
plot(clus_var, cex = 0.5)
#variable clustering using hoeffding method
clus_var2 <- varclus(as.matrix(clusteringdf), similarity = "hoeffding", minlev = 0.05)
plot(clus_var2, cex = 0.5)


