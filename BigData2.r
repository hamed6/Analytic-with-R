#Hamed Ghodsinezhad
#------------------------
#Q1
file.choose()
summary(haberman)

age<-haberman$Age
operation<-haberman$OperatioYear
node<-haberman$NumberNodes
survive<-haberman$SurvivalStatus

cor(age,survive)

plot(node~age,main="Node per Age",las=1)
mod2<-lm(node~age)
abline(mod2,col=2,lwd=3)

lm(age~survive)
#for more analyzing purpose
mod1<- lm(age~survive)
summary(mod1)
attributes(mod1)


boxplot(age~survive,las=1,ylab="Age",xlab="Survival Status",main="Boxplot Age Vs Survive")

logisticR<- glm(survive~age+node)
summary(logisticR)
str(logisticR)

wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 1:3)
exp(coef(logit))

mySplit <- sample.split(haberman, SplitRatio = 0.75)
training_set <- subset(haberman,mySplit == TRUE)
test_set <- subset(haberman, mySplit == FALSE)

decision_tree <- rpart( survive~age+node+year, data = training_set ,method="class")
prp(decision_tree)

decision_tree <- rpart( survive~age+node+year, data = training_set ,method="class",minbucket=10)
prp(decision_tree)
decision_tree <- rpart( survive~age+node+year, data = training_set ,method="class",minbucket=8)
prp(decision_tree)

#------------------------
#Q2
install.packages("arules")
library(arules)
data("AdultUCI")
?AdultUCI

View(AdultUCI)
age<- AdultUCI$age
income<- AdultUCI$income
sex<-AdultUCI$sex
hours<-AdultUCI$hours-per-week
education<-AdultUCI$education-num

plot(age~income,las=1,main="Compare Age Vs Income")
plot(income,education,las=1,main="Compare Education Vs Income",ylab="Education Numb",xlab="Income")

myGplot<-ggplot(AdultUCI,aes(x=hours , y=income))
myGplot+ geom_count()

myGplot<-ggplot(AdultUCI,aes(x=sex , y=income))
myGplot+ geom_count()

mySplit <- sample.split(AdultUCI$income, SplitRatio = 1)
training_set <- subset(income, mySplit == TRUE)
test_set <- subset(income, mySplit == FALSE)
tree_me<- rpart(income ~ age + education + sex + hours, data = training_set, method="class", minbucket = 25)
prp(tree_adm_2)
#------------------------
#Q3
 rulesSmall<-apriori(Adult,parameter = list("income==small"))
 rulesLarge<-apriori(Adult,parameter = list("income==large"))
 
 summary(rulesLarge)
 summary(rulesSmall)
 
sortedSmall<- sort(rulesSmall,by="confidence",decreasing = TRUE)
inspect(sortedSmall[1:15])
 
sortedLarge<- sort(rulesLarge,by="confidence",decreasing = TRUE)
inspect(sortedLarge[1:15])
