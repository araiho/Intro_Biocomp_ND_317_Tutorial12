library(ggplot2)

chickwts <- read_csv("chickwts.txt")
View(chickwts)

plot1 <- ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_boxplot() + 
  theme_bw() + 
  ggtitle("Chicken Feed vs. Weight")

plot1

# Null Hypothesis : Soybeans and sunflower don't effect chicken weight.

# Alternate hypothesis: That one will effect chicken weight more than the other.

SoybeanandSunflower <- subset(chickwts, feed == "soybean"|feed == "sunflower")

nnlike0 <- function(p,x,y){
  B0 = p[1]
  sigma = exp(p[2])
  expected = B0
  nll= -sum(dnorm(x=y, mean=expected, sd = sigma, log = TRUE))
  return(nll)
}

nnlike1 <- function(p,x,y){
  B0 = p[1]
  B1 = p[2]
  sigma = exp(p[3])
  expected = B0+B1*x
  nll1= -sum(dnorm(x=y, mean=expected, sd = sigma, log = TRUE))
  return(nll1)
}

params0 <- c(400,100)
params1 <- c(400,100,100)

fit0 = optim(par = params0, fn=nnlike0, y=SoybeanandSunflower[(SoybeanandSunflower$feed%in%c("soybean","sunflower")),]$weight, x = 0)
fit1 = optim(par = params1, fn=nnlike1, y=SoybeanandSunflower[(SoybeanandSunflower$feed%in%c("soybean","sunflower")),]$weight, x = 1)

NullHypothesis <- pchisq(q = 2*abs((fit0$value-fit1$value)), df = 1, lower.tail = FALSE)

NullHypothesis

