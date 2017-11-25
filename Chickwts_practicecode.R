chickwts <- read.csv("chickwts.txt", header=TRUE, sep=",")
library(ggplot2)

plot <- ggplot()+
          theme_classic()+
          geom_bar(data=chickwts, aes(x=feed,y=weight), stat='summary', fun.y = 'mean')
         # labs(x="Feed",y="Weight")
plot

soybean <- chickwts[chickwts$feed == 'soybean',]
sunflower <- chickwts[chickwts$feed == 'sunflower',]

# null hypothesis = no difference in mean weight between the soybean fed and sunflower fed chicks
# alternative hypothesis = sunflower fed chick mean weight is larger than soybean fed chicks mean weight

nullike <- function(p,x,y){
  B0=p[1]
  sig=exp(p[2])
  expected=B0
  nullike=-sum(dnorm(x=y), mean=expected, sd=sig,log=TRUE)
  return(nullike)
}

#soybean null
initialGuess=c(250,1)
soyfit=optim(par=initialGuess,fn=nullike,y=soybean$weight)
print(soyfit)

#sunflower null
initialGuess=c(300,1)
sunfit=optim(par=initialGuess,fn=nullike,y=sunflower$weight)
print(sunfit)

#likelihood ratio test
difflike <- 2*(sunfit$value - soyfit$value)
pchisq(q=difflike, df=1, lower.tail = FALSE)

