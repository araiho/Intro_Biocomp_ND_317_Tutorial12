rm(list=ls())

library(ggplot2)

chicks = read.csv(file = "chickwts.txt", header = TRUE, sep = ",")

#Graphing Data
d = ggplot(data = chicks)
d + geom_bar(aes(x=as.factor(feed),y=weight),stat="summary",
             fun.y="mean",fill="blue",color="black") + theme_classic() +
            xlab("Type of Feed")+ylab("Average Weight ")

#Colelcting the Right Subset
chicks1=chicks[which(chicks$feed=="soybean"),]
chicks1[,2]=0
chicks2=chicks[which(chicks$feed=="sunflower"),]
chicks2[,2]=1

TestThis = rbind(chicks1,chicks2)

#Defining the Liklihood Functions
Nullnllike<-function(p,y){
  B0=p[1]
  sigma=exp(p[2])
  
  expected=B0
  
  nll=-sum(dnorm(x=y, mean=expected, sd=sigma, log = TRUE))
  return((nll))}

nllike<-function(p,x,y){
  B0=p[1]
  B1=p[2]
  sigma=exp(p[3])
  
  expected=B0+B1*x
  
  nll=-sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
  return((nll))}

NullGuess=c(284.5,1)
Guess=c(246.4,82,1)

Fit=optim(Guess, nllike, x=TestThis$feed, y=TestThis$weight)
NullFit=optim(NullGuess, Nullnllike, y=TestThis$weight)

D=-2*(Fit$value-NullFit$value)
Result=pchisq(q=D, df=1, lower.tail=FALSE)

