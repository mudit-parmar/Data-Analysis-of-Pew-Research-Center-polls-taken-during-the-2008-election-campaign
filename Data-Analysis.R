#install.packages("glmnet")
#install.packages("fastDummies")
#install.packages("dplyr")

#loading the important libraries
library(foreign)
library(plyr)
library(dplyr)
library(fastDummies)
library(glmnet)
library(ggplot2)
#reading the .dta file into data 
data <- read.dta('Q1Data1.dta')
#displaying the data
data

#1 a.) 1 Subset the data so that you have all states but Hawaii, Alaska, and Washington D.C and have only four columns “state,”, “marital,” “heat2,” and “heat4.”

#removing columns
data = subset(data, select = c(state,marital,heat2,heat4) )
data
#eliminating the states
data<-subset(data, state!="hawaii" & state!="alaska" & state !="washington dc")
#displaying the results after keeping only 4 columns and removing 3 states
View(data)

#1 a.) 2  If no data is available in “heat2,” replace na for the corresponding value in “heat4.” If neither of “heat2” and “heat4” has data, erase the corresponding row. 

#replacing the value of na in heat2 with value of heat4
data<- data %>% mutate(heat2=coalesce(heat4,heat2))
#removing the rows in which both heat2 and heat4 are NA
data<-data[!with(data,is.na(heat2)& is.na(heat4)),]
#displaying the results replacing na values in heat2 with heat 4 and removing for na in both heat and heat4
View(data)


#1 a.) 3 Subset the data so that you only have “dem/lean dem” and “rep/lean rep” in the “heat2” column.

#selecting only those rows where heat2 is dem/lean dem or rep/lean rep
data<-data[(data$heat2 == 'dem/lean dem'| data$heat2 == 'rep/lean rep'),]
#displaying the results
head(data)

#1 a.) 4 Change the label of all the variables but ‘married’ (married people) in the “marital” column to ‘other’ 

#dropping rows where marital status is NA
data<-data[!is.na(data$marital),]
#changing the label to married for married and others for all the remaning
data$marital <- as.character(data$marital)
data$marital<-ifelse(!data$marital=='married','others','married')
#displaying the final results
head(data)

# 1 b.) 1  the proportion of the democratic supporters

#creating a dataframe dem_sup to store the results
dem_sup=data %>%
  group_by(state) %>%    #group_by state is used for grouping all the values by state
  summarise(Proportion = mean(heat2 == 'dem/lean dem')*100)   #to get the proportion of democratic voters in the state
head(dem_sup,5) #displaying first 5 values

# 1 b.) 2 the proportion of the married people

#creating a dataframe mar_prop to store the results
mar_prop=data %>%
  group_by(state) %>%  #group_by state is used for grouping all the values by state
  summarise(Proportion = mean(marital == 'married' )*100) #to get propotion of married people and multiplying by 100.
head(mar_prop,5) #displaying first 5 values

# 1 b.) 3  the ratio of the married people among the democratic supporters to the total married people

#creating a dataframe mar_dem_sup to store the results
mar_dem_sup=data %>%
  group_by(state) %>%   #group_by state is used for grouping all the values by state
  summarise(Proportion = (mean(marital == 'married' & heat2 == 'dem/lean dem')/ mean(marital == 'married' ))*100) #finding the %
head(mar_dem_sup,5)#displaying first 5 values


# 1 b.) 4  the ratio of non-married among the democratic to the total non-married people

#creating a dataframe unmar_dem_sup to store the results
unmar_dem_sup=data %>%
  group_by(state) %>%  #group_by state is used for grouping all the values by state
  summarise(Proportion = (mean(marital == 'others' & heat2 == 'dem/lean dem')/ mean(marital == 'others' ))*100) #finding the %
head(unmar_dem_sup,5) #displaying first 5 values

# 1 b.) 5 the difference of 3) and 4)

#creating a dataframe dif to store the results
dif=data %>%
  group_by(state) %>% #group_by state is used for grouping all the values by state
  summarise(Proportion = (abs((mean(marital == 'married' & heat2 == 'dem/lean dem')/ mean(marital == 'married'))-(mean(marital == 'others' & heat2 == 'dem/lean dem')/ mean(marital == 'others' ))))*100)
head(dif,5) #displaying first 5 values

# 1 c.) 1 Subset the data so that you have all but three states, Hawaii, Alaska, and Washington D.C,

#reading .csv file
data1 <- read.csv('Q1Data2.csv')
data1 #displaying the read file
#getting only those rows where state is not Hawaii, Alaska and District of Columbia
data1<-data1[!(data1$state=="Hawaii" | data1$state=="Alaska" | data1$state=="District of Columbia"),]
data1

# 1 c.) 2 only two columns “state,” and “vote_Obama_pct” (Obama’s actual vote share)

#subset the data to get only two columns
data1 = subset(data1, select = c(state,vote_Obama_pct) )
#displaying the head
head(data1,5)

#1 d.)

# Assumption 1:No state-level heterogeneity. All states have the same intercept and slope.

#applying logistic regression in the glm function with marital as the predictor
model_assumption1 <- glm( heat2~ 0+marital, data=data, family=binomial)
#displaying the coefficients of assumption 1
coef(model_assumption1)

# Assumption 2: Complete state-level heterogeneity. All states have completely independent intercepts and slopes. No outlying coefficient is penalized.

#generating a matrix which creates dummy variables for the predictor and stores it as matrix
mm_preditor <- model.matrix(heat2 ~ 0+ marital + state ,data = data )
#storing the outcome variable of the model
mm_outcome <- data$heat2
#applying logistic regression in the glmnet function with marital as the predictor and states level heterogeneity  
model_assumption2 <- glmnet(x = mm_preditor , y = mm_outcome, alpha= 0, lambda = 0, family = binomial)
#displaying the coefficients of assumption 2
coef(model_assumption2)

#Assumption 3: State-level heterogeneity is unknown a priori. States have partially pooled intercepts and slopes. Outlying coefficients are penalized.

#using the intersection for predicting along with marital and states
mm_preditor1 <- model.matrix(heat2 ~ 0+ marital*state, data = data )

#applying cross validation on glmnet to get the best value of lambd for ridge model
cv_model_ridge <- cv.glmnet(x = mm_preditor1, y = mm_outcome, alpha = 0, family=binomial)
#finding the best lambda value by finding the minimum lambda value from cv_model_ridge
best_lambda_ridge <- cv_model_ridge$lambda.min
#displaying the best lambda value
best_lambda_ridge
#applying glmnet with best lambda value 
model_assumption3 <- glmnet(x = mm_preditor1, y = mm_outcome, alpha = 0, lambda =best_lambda_ridge, family=binomial )
#displaying the coefficients
coef(model_assumption3)

#1 e.)

#getting the prediction results using the assumption model 3
y_predicted_ridge <- predict(model_assumption3, s = best_lambda_ridge, newx = mm_preditor1,type="response")
#storing the values in a new dataframe
ridge_prob=as.data.frame(y_predicted_ridge)
#if the probability is greater than 0.5 than it is democratic else republic
ridge_prob$s1<-ifelse(ridge_prob$s1>0.5,"dem/lean dem","rep/lean rep")
#creating another dataframe to bind the predicted values of s1 in ridge prob with our original data
data_bind<-data
#binding the two datasets using cbind
data_bind <- cbind(data_bind, predicted = ridge_prob$s1)
#predicting the proportion of democratic supporters
dem_sup_pred=data_bind %>%
  group_by(state) %>% #grouping for each state
  summarise(Proportion = mean(predicted == 'dem/lean dem')*100)  # getting the propotion of democratic supporters predicted
#plotting the graph using ggplot
ggplot() + xlab("Actual and Predicted % of vote share") + ylab("Obama's Vote share") +
  geom_point(aes(x = dem_sup_pred$Proportion, y = data1$vote_Obama_pct, color="predicted",size=4)) + 
  geom_point( aes(x = dem_sup$Proportion, y = data1$vote_Obama_pct, color= "actual",size=4))+
  geom_text(aes(x = dem_sup$Proportion, y = data1$vote_Obama_pct,label=dem_sup$state),size=2.25,check_overlap = TRUE,)+
  geom_text(aes(x = dem_sup_pred$Proportion, y = data1$vote_Obama_pct,label=dem_sup_pred$state),size=2.25,check_overlap = TRUE,)



#1 f.)

#predicting marriage_gap of democratic voters for each state
dif1=data_bind %>%
  group_by(state) %>% #group_by state is used for grouping all the values by state
  summarise(Proportion = (abs((mean(marital == 'married' & predicted == 'dem/lean dem')/ mean(marital == 'married'))-(mean(marital == 'others' & predicted == 'dem/lean dem')/ mean(marital == 'others' ))))*100)
head(dif1,5) #displaying first 5 values
#plotting the graph using ggplot
ggplot() + xlab("Actual and Predicted % of marriage gap") + ylab("Obama's Vote share") +
  geom_point(aes(x = dif1$Proportion, y = data1$vote_Obama_pct, color="predicted",size=6))  +
  geom_point( aes(x = dif$Proportion, y = data1$vote_Obama_pct, color= "actual",size=6))+
  geom_text(aes(x = dif$Proportion, y = data1$vote_Obama_pct,label=dif$state),size=2.75,check_overlap = TRUE,)+
  geom_text(aes(x = dif1$Proportion, y = data1$vote_Obama_pct,label=dif1$state),size=2.75,check_overlap = TRUE,)

#1 g.) repeating e and f with model 2

#getting the prediction results with no_pooling
y_predicted_no_pooling <- predict(model_assumption2, s = 0, newx = mm_preditor,type="response")
#creating a dataframe to store the results
prob_no_pool=as.data.frame(y_predicted_no_pooling)
#if the probability is greater than 0.5 than it is democratic else republic
prob_no_pool$s1<-ifelse(prob_no_pool$s1>0.5,"dem/lean dem","rep/lean rep")
#creating another dataframe to bind the predicted values of s1 in prob_no_pool with our original data
data_bind1<-data
#binding the two datasets using cbind
data_bind1 <- cbind(data_bind1, predicted = prob_no_pool$s1)
#predicting the proportion of democratic supporters
dem_sup_pred1=data_bind1 %>%
  group_by(state) %>% #grouping for each state
  summarise(Proportion = mean(predicted == 'dem/lean dem')*100)  # getting the propotion of democratic supporters predicted


#plotting the graph for % vote share of democratics using ggplot
ggplot() + xlab("Actual and Predicted % of vote share") + ylab("Obama's Vote share") +
  geom_point(aes(x = dem_sup_pred1$Proportion, y = data1$vote_Obama_pct, color="predicted",size=4)) + 
  geom_point( aes(x = dem_sup$Proportion, y = data1$vote_Obama_pct, color= "actual",size=4))+
  geom_text(aes(x = dem_sup$Proportion, y = data1$vote_Obama_pct,label=dem_sup$state),size=2.25,check_overlap = TRUE,)+
  geom_text(aes(x = dem_sup_pred1$Proportion, y = data1$vote_Obama_pct,label=dem_sup_pred1$state),size=2.25,check_overlap = TRUE,)


#predicting and plotting marriage_gap of democratic voters for each state
dif2=data_bind1 %>%
  group_by(state) %>% #group_by state is used for grouping all the values by state
  summarise(Proportion = (abs((mean(marital == 'married' & predicted == 'dem/lean dem')/ mean(marital == 'married'))-(mean(marital == 'others' & predicted == 'dem/lean dem')/ mean(marital == 'others' ))))*100)
head(dif2,5) #displaying first 5 values
#plotting the graph using ggplot
ggplot() + xlab("Actual and Predicted % of marriage gap") + ylab("Obama's Vote share") +
  geom_point(aes(x = dif2$Proportion, y = data1$vote_Obama_pct, color="predicted",size=6))  +
  geom_point( aes(x = dif$Proportion, y = data1$vote_Obama_pct, color= "actual",size=6))+
  geom_text(aes(x = dif$Proportion, y = data1$vote_Obama_pct,label=dif$state),size=2.75,check_overlap = TRUE,)+
  geom_text(aes(x = dif2$Proportion, y = data1$vote_Obama_pct,label=dif2$state),size=2.75,check_overlap = TRUE,)
