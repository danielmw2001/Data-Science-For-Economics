#Date: 18/09/2023
#Author: Daniel Martin-Williams
#Investigating factors influencing price of orange juice using a general linear model
oj <- read.csv("oj.csv", stringsAsFactors = T)

#Fit and plot a univariate general linear model, check elasticities of demand
reg <- glm(log(sales) ~ log(price)*(brand), data = oj )
beta <- coef(reg)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
     cex=.1, pch=20, bty="n")

brandcol <- c("green","red","gold")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[3], b=beta[2]+beta[5], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2] + beta[6], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))
dev.off()
#Tropicana has the smallest PED, followed by minute maids, then dominicks

#Q2/3/4 - Run new, fuller regression and find SE and R^2, and plot
reg2 <- glm(log(oj$sales) ~ log(price)*(brand)*(feat), data = oj)
beta <- coef(reg2)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
     cex=.1, pch=20, bty="n")

#Dominick no feature
abline(a = beta[1], b = beta[2], col=brandcol[1], lwd = 2) 

#Dominick feature
abline(a = beta[1] + beta[5], b = beta[2] + beta[8], 
       col=brandcol[1], lwd = 2, lty = "dashed") 

# minutemaid no feature
abline(a = beta[1] + beta[3], b = beta[2]+beta[6],
       col=brandcol[2], lwd = 2) 

#minutemaid feature
abline(a = beta[1] + beta[3]+ beta[5] + beta[9], b = beta[2] + beta[11] + 
         beta[6] + beta[8], col=brandcol[2], lwd = 2, lty = "dashed") 

# Tropicana no feature
abline(a = beta[1] + beta[4], b = beta[2]+beta[7],
       col=brandcol[3], lwd = 2) 

#Tropicana feature
abline(a = beta[1] + beta[5] + beta[8] + beta[10], b = beta[2] + beta[6] +
         beta[7] + beta[12], col=brandcol[3], lwd = 2, lty = "dashed")

ojlegend <- c('Dominicks','Dominicks w Feature','Minutemaid',
            'Minutemaid w Feature', 'Tropicana', 'Tropicana w Feature')
linetype <- c(1,2)
brandcol <- c("green","green","red","red","gold","gold")
legend("bottomleft", bty="n",lty = linetype, lwd=2, col=brandcol, legend=ojlegend)
dev.off()
summary(reg2)
R2 <- 1 - (reg2$deviance/reg2$null.deviance)
#R2 of 53.5%


#Q5 - Predictions
#make new dataframe and output predictions 
newdata1 <- data.frame(price = log(4), brand = "dominicks", feat = 1)
reg1predict <- exp(predict(reg, newdata = newdata1))
reg2predict <- exp(predict(reg2, newdata = newdata1))
