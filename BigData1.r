Q1)
file.choose()

under15 <- WHO$Under15
over60 <- WHO$Over60
population<-WHO$Population
country<-WHO$Country
fertilityrate<-WHO$FertilityRate

region <- table(WHO$Region)

pie(region,main="Region")
box()

boxplot(fertilityrate,main="Fertility Rate")

boxplot(under15,main="Under 15",las=1)
boxplot(over60,main="Over 60",las=1)



plot(under15 ~ over60,las=1,main="Age Diversity")

describe(under15)
describe(over60)

plot(aggregate(Under15~Population,FUN = mean),las=1,main="Under 15 VS Population")
plot(aggregate(Over60~Population,FUN = mean),las=1,main="Over 60 VS Population")

lifeExpectancy<- WHO$LifeExpectancy
childMortality <- WHO$ChildMortality



region <- WHO$Region
boxplot(childMortality~region,main="Child Mortality",las=1)
boxplot(lifeExpectancy~region,main="Life Expectancy",las=1)

cellularSubscriber<- WHO$CellularSubscribers
plot(cellularSubscriber~region,las=1)


shapiro.test(childMortality[1:4])
shapiro.test(lifeExpectancy[1:4])
----------------------------------------
Q2)
cor(population,fertilityrate)
lifepredict <- lm(over60~childMortality)
summary(lifepredict)


lifepredict$coefficients


lifepredict<- lm(over60 ~ lifeExpectancy)
summary(lifepredict)

plot(over60~lifeExpectancy,las=1,main="Predict age life")
abline(lifepredict,col=3,lwd=3)