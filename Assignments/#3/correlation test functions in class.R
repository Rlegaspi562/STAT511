#correlation analysis
cor(lotdata$size, lotdata$hours) #sample correlation

#find the t test statistic manually
0.9064 * sqrt(23)/sqrt(1-(0.9064^2))

#Find p-value manually
2*pt(10.29,23,lower.tail = FALSE)

#Correlation test function
#default method is pearson, two-sided test
cor.test(lotdata$size, lotdata$hours)
cor.test(lotdata$size, lotdata$hours,
         method = "pearson",
         alternative = "two.sided")
#spearman correlation test
#Specify method = "spearman"
cor(lotdata$size, lotdata$hours, method = "spearman")
cor.test(lotdata$size, lotdata$hours, method = "spearman")

#Find 95% CI manually through transformation
#transform r to z
0.5*log((1+0.9064)/(1-0.9064))

#95% CI for z
1.507-1.96*sqrt(1/22)
1.507+1.96*sqrt(1/22)

#transform back forwards to find 95% CI for rho
(exp(2*1.089127)-1)/(exp(2*1.089127)+1)
(exp(2*1.925)-1)/(exp(2*1.925)+1)

install.packages("nortest")
install.packages("car")
install.packages("lmtest")
install.packages("alr4")
