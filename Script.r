1:

boxplot( score , gender , names=c("Female" , "Male") , las=1 , main = "Comparison between Score in Two Gender" , xlab="Gender", ylab="Score",
col=c("turquoise","tomato"))

2:

 boxplot( score , gender , names=c("Female" , "Male") , las=1 , main = "Comparison between Score in Two Gender" , xlab="Gender", ylab="Score",
+          col=c("tomato","turquoise"))

3:

plot(anonymized_submissions$time_on_task ~ anonymized_submissions$score, las=1, xlab="Score", ylab="TimeOnTask", col=9, col.main=4, main="Most score Most time spent", col.lab=6 , col.axis=3 )

4:
'Import DB library'
fault <- subset(math_misconception$is_correct<1)
df = sqldf('select type , count(type) from misconception_type group by type , count(type)')


plot(da$gender , da$`sum(a.score)`, ylim=range(100000, 180000) , xlab="Gender" , ylab="TotalScore" , las=1, freq=F)

misconception_type :
 plot(df, main="Misconception Types" , xlab= "Type" , ylab="Count", las=1 , xlim=c(1,9) , cex.axis = 0.6 , col.main=3 , col.lab=2, col.axis=132 , cex.lab=1.5)
 
 5:
 boxplot(dfFemale$time_on_task~dfFemale$gender , main="Time On Task Based on Gender" , ylab="TimeOnTask", las=1, col.main=132 , cex.axis = 0.6  , col.axis= "brown" , col=c("red", "green"))

-----------------------------
par(mfrow=c(4,2))
for (i in 1:length(cs)){plot(sensor[as.integer(cs[i])])}
