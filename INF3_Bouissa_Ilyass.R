#Bouissa Ilyass
#___________________________ Projet STA401 ___________________________#


setwd("H:/ilyss$/Documents/Cours_2020_2021/semester 4/STAT401/TP")
library(readr)


#------------- Introduction-----------#


# Nous allons comparer les température entre de la fin du XXe siecle et le début du XXIe
# Nous avons une variable Years de type Temporelle qui corrsepond à l'année de la mesure et 
# une variable Mean de type quantitative continues qui correspond au anomalie de temperature en degrés Celsuis 
#c'est a dire l'écart entre la moyenne de temperature et la temperature mesuré.
# Nous allons comparé les 15 dernieres années avant l'année 2000 et les 15 depuis l'année 2000.
# Pour mettre en évidence les effets du réchauffement climatique.



#-------- Analyse descriptive --------#
Temp_RA <- read_csv("Data/Annual_mean_temperature_anomalies.csv")
Temp_RA
class(Source)

Tab <-Temp_RA[,c(2,3)]
Tab



Tab_XXI <- subset(Tab,Year>=2000)
Tab_XX <- subset(Tab,Year<2000)



boxplot(Tab_XX$Mean,Tab_XXI$Mean,names=c("XXe","XXIe"))
#On peut voir une hausse cesicante de la temperature entre les 2 boites à moustache.




hist(Tab_XX$Mean,freq=FALSE,xlim=c(0,0.8),main="Histogramme temperature au XXe")
curve(dnorm(x,mean(Tab_XX$Mean),sd(Tab_XX$Mean)),add = TRUE,col='red')
abline(v=mean(Tab_XX$Mean))

qqnorm(Tab_XX$Mean,main = " Quantile-Quantile de temperature au XXe")
qqline(Tab_XX$Mean, col = 1,lwd=2,lty=2)
abline(h=quantile(Tab_XX$Mean,0.25),v=qnorm(0.25),col="2",lty=1)
abline(h=quantile(Tab_XX$Mean,0.75),v=qnorm(0.75),col="3",lty=1)

legend("topleft", inset=.02, title="quartile XXe",
       c("1er quartile","3e quartile"), fill=c(2,3), cex=0.7)

#HIST DU XXI
hist(Tab_XXI$Mean,freq=FALSE,xlim=c(0.2,1),main="Histogramme temperature au XXIe")
curve(dnorm(x,mean(Tab_XXI$Mean),sd(Tab_XXI$Mean)),add = TRUE,col='red',to=1)
abline(v=mean(Tab_XXI$Mean))

#COMPARAISON DE COURBE GAUSS 
curve(dnorm(x,mean(Tab_XX$Mean),sd(Tab_XX$Mean)),add = FALSE,col='red',ylim=c(0,6),main="comparaison courbe gaussienne",ylab="",xlab="Temperature en °c")
curve(dnorm(x,mean(Tab_XXI$Mean),sd(Tab_XXI$Mean)),add = TRUE,col='blue',to=1,ylab="")
legend("topright", inset=.02, title="Loi normale",
       c("20e siècle","21e siècle"), fill=c('blue','red'), cex=0.7)



qqnorm(Tab_XXI$Mean,main = " Quantile-Quantile de temperature au XXIe")
qqline(Tab_XXI$Mean, col = 2,lwd=2,lty=2)
abline(h=quantile(Tab_XXI$Mean,0.25),v=qnorm(0.25),col="2",lty=1)
abline(h=quantile(Tab_XXI$Mean,0.75),v=qnorm(0.75),col="3",lty=1)

legend("bottomright", inset=.02, title="quartile XXIe",
       c("1er quartile","3e quartile"), fill=c(2,3), cex=0.7)

plot(Tab_XX$Year,Tab_XX$Mean,xlab='Année',ylab='Temperature en °c',main="Variation de temperature au XXe")
regression <- lm(Tab_XX$Mean ~ Tab_XX$Year)$coefficients
abline(a=regression[1] ,b=regression[2] , col = 'red')
lm(Tab_XX$Mean ~ Tab_XX$Year)$coefficients[2]


coef = lm(Tab_XX$Mean ~ c(1:15))$coefficients
text(1988,0.5,paste("Equation de la droite\ny =",round(coef[2],3),"x+",round(coef[1],3)),col="red" )

#La droite d'équation a un coefficient directeur positif sur ca droite de tendance
#on voit donc qu'il y a une hausse depuis le début d'une 20é siecle 


plot(Tab_XXI$Year,Tab_XXI$Mean,xlab='Année',ylab='Temperature en °c',main="Variation de temperature au XXIe",col='blue')
regression <- lm(Tab_XXI$Mean ~ Tab_XXI$Year)$coefficients
abline(a=regression[1],b=regression[2], col = 'blue')

#On peut voir que cette hausse continue toujours au XXIe siècles.
coef = lm(Tab_XXI$Mean ~ c(1:15))$coefficients
text(2005,0.72,paste("Equation de la droite\ny =",round(coef[2],3),"x+",round(coef[1],3)),col="blue")


split.screen(c(1,2))
screen(1)
qqnorm(Tab_XX$Mean,main = " Quantile-Quantile de temperature au XXe")
qqline(Tab_XX$Mean, col = 1,lwd=2,lty=2)
abline(h=quantile(Tab_XX$Mean,0.25),v=qnorm(0.25),col="2",lty=1)
abline(h=quantile(Tab_XX$Mean,0.75),v=qnorm(0.75),col="3",lty=1)

legend("topleft", inset=.02, title="quartile XXe",
       c("1er quartile","3e quartile"), fill=c(2,3), cex=0.7)
screen(2)
qqnorm(Tab_XXI$Mean,main = " Quantile-Quantile de temperature au XXIe")
qqline(Tab_XXI$Mean, col = 2,lwd=2,lty=2)
abline(h=quantile(Tab_XXI$Mean,0.25),v=qnorm(0.25),col="2",lty=1)
abline(h=quantile(Tab_XXI$Mean,0.75),v=qnorm(0.75),col="3",lty=1)

legend("bottomright", inset=.02, title="quartile XXIe",
       c("1er quartile","3e quartile"), fill=c(2,3), cex=0.7)

