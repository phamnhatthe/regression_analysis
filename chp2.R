setwd("C:/hw/330")
production<-read.table("production.txt",header=T)

model<-lm(production$RunTime ~ production$RunSize)
result<-summary(model)

# calculate se(beta1_hat)

attach(production)
beta0_hat<-result$coefficients[1,1]
beta1_hat<-result$coefficients[2,1]
e<-RunTime - (beta0_hat + beta1_hat*RunSize)
s<-sqrt(sum(e^2)/(length(RunTime) - 2))
se_beta1_hat<-s/sqrt(sum((RunSize - mean(RunSize))^2))
se_beta1_hat
result # compare result of your calculation with that of R

# Hypothesis testing to see if beta1 is significant
.25924/.03714 # test statistic 
2*pt(.25924/.03714,length(RunSize)-2,lower.tail=F) # p value for determining whether to reject
# the null hypothesis

# create confidence interval for your parameters
# beta1_hat
result$coefficients[2,1]+c(-1,1)*qt(.975,length(RunSize)-2)*result$coefficients[2,2]
# beta0_hat
result$coefficients[1,1]+c(-1,1)*qt(.975,length(RunTime)-2)*result$coefficients[1,2]

# creating confidence and prediction intervals around the regression line
conf_result<-predict(model,interval="confidence",level=0.95)
pred_result<-predict(model,data.frame(RunSize),level=.95,interval="prediction")

pop1<-data.frame(RunSize,conf_result)
pop2<-data.frame(RunSize,pred_result)
pop1<-pop1[order(pop1$RunSize),]  # these two lines smooth out the lines you plot
pop2<-pop2[order(pop2$RunSize),]

plot(RunSize,RunTime,type="p",ylim=c(140,280))
abline(result$coefficients[1,1],result$coefficients[2,1])
lines(pop1$RunSize,pop1$lwr,type="l",col="red")
lines(pop1$RunSize,pop1$upr,type="l",col="red")
lines(pop2$RunSize,pop2$lwr,type="l",col="blue")
lines(pop2$RunSize,pop2$upr,type="l",col="blue")
lines(pop1$RunSize,pop1$fit,type="l",col="green") # fitted line is same as regression line



