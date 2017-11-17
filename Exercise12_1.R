library(ggplot2)

chickwts <- read_csv("chickwts.txt")
View(chickwts)

plot1 <- ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_boxplot() + 
  theme_bw() + 
  ggtitle("Chicken Feed vs. Weight")

plot1

SoybeanandSunflower <- subset(chickwts, feed == "soybean"|feed == "sunflower")

plot2 <- ggplot(data = SoybeanandSunflower, aes(x = feed, y = chickwts))+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(size = 3, shape = 21, color = "black") +
  theme_classic() +
  xlab("feed") + ylab("weight") +
  ggtitle("Chicken Feed vs. Weight") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method = "lm", se=FALSE, color="black") + 
  geom_text(aes(x = 25, y = 400, label = lm_eqn1(lm(weight ~ feed, SoybeanandSunflower))), parse = TRUE)

m= lm(weight ~ feed, SoybeanandSunflower)

lm_eqn1 = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

plot2
