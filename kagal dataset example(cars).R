car<-read.csv(choose.files())
dim(car)
head(car)
car<-car[,-c(1,3)]
head(car)
str(car)

colSums(is.na(car))
install.packages("mice")
library(mice)
colSums(is.na(car))
str(car)

boxplot(car)

install.packages("caret")
library(caret)
?dummyVars
dummy<-dummyVars("~ .",data=car)
dummy
car<-data.frame(predict(dummy,newdata=car))
car
head(car,50)
names(car)
car<-car[,-c(2,4,6,8,14,16,19,24,30,38)]
str(car)
boxplot(car)

###################################
#split the data into training & test
library(caTools)

set.seed(123)
split<-sample.split(car$price,SplitRatio = 0.75)
split
table(split)

training<-subset(car,split==T)
test<-subset(car,split==F)
nrow(training)
nrow(test)
cor(car)
?heatmap
heatmap(cor(car))
corrgram(car)
##################################
#building a linear regression model with training dataset
names(car)
reg<-lm(price~.,data=training)
summary(reg)

install.packages("corrgram")
library(corrgram)
#backward means(-)
#instead of selecting non-significant we take significant
#forward(means +) approach


reg_1 <-lm(price~fueltypegas+carbodyhardtop
          +carbodyhatchback+carbodysedan+carbodywagon
          +enginelocationrear+carwidth+curbweight
          +cylindernumberfive+cylindernumberfour
          +cylindernumbersix+enginesize+boreratio+stroke
          +compressionratio+peakrpm,data=training)
summary(reg_1)

reg_1<-lm(price~carbodyhardtop+carbodyhatchback
          +carbodysedan+carbodywagon
          +boreratio,data=training)
summary(reg_1)
car_pred<-predict(reg_1,newdata=test)
car_pred

test_cbind<-cbind(test$price,car_pred)
car_pred
plot(test$price~test$peakrpm,type='l')
plot(car_pred)

vif(reg_1)
dwtest(reg_1)
install.packages("lmtest")
library(lmtest)

install.packages("faraway")
library(faraway)
