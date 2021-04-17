#Load drug data
load("C:/Users/RUMIL/Desktop/APU/STAT 511 - Millie Mao (Applied Regression Analysis)/Week 13/Drug.Rdata")

#Variables
Drug=Drugdata$Drug
FevRed=Drugdata$FevRed

#Regression with dummies
#Use as.factor() function
drug.lm1=lm(FevRed ~ as.factor(Drug))
summary(drug.lm1)

#Create dummies before regression
#Create a new variable Drugname
Drugname = as.factor(Drug)
drug.lm2 = lm(FevRed ~ Drugname)
summary(drug.lm2)

#See variable type, typeof() 
typeof(Drugname)    
#See attributes of a variable, attributes() 
attributes(Drugname)

#Change reference group
#Use Drug B ("2") as reference group
#Use relevel() function
Drugname_new = relevel(Drugname,ref="2")
drug.lm3 = lm(FevRed ~ Drugname_new)
summary(drug.lm3)


#Regression model with interaction
load("D:/APU/511/Lectures/Chapter 8/Prodlines.Rdata")
View(ProdLines)

Scrap = ProdLines$Scrap
Speed = ProdLines$Speed
Line = ProdLines$Line

#Use xyplot() function in "lattice" package
library(lattice)
plotlegend=list(space="right", text=list(c("Line2","Line1")),
                points=list(pch=c(1,3),col=c("red","blue")))

xyplot(Scrap ~ Speed, groups=Line, data = ProdLines,
       pch=c(1,3), col=c("red","blue"), type="p",
       main="Plot of Two Production Lines", key=plotlegend)

#Regression with interaction
inter.lm1 = lm(Scrap ~ Speed+Line+Speed*Line, data=ProdLines)
summary(inter.lm1)

#R shortcut: only enter interaction
inter.lm2 = lm(Scrap ~ Speed*Line, data=ProdLines)
summary(inter.lm2)

#Plot the fitted regresion lines
#Save estimated coefficients
Coef = inter.lm2$coefficients

#Line 1 production: when Line takes value 1
Scrap1 = Coef[1]+Coef[2]*Speed+Coef[3]*1+Coef[4]*Speed

#Line 2 production: when Line takes value 0 
Scrap2 = Coef[1]+Coef[2]*Speed
  
#Use plot() function
plot(Speed, Scrap1, type='l', col="blue",
     xlab="Production Speed", ylab="Fitted Scrap Production")
lines(Speed, Scrap2, type='l', col="red")

#Add legend with plot() function
legend("topleft",legend=c("Line 1","Line 2"),
       col=c("blue","red"), lty=1, cex=0.5)


