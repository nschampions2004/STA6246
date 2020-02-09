# Etching Experiment

ER=c(575,542,530,539,570,565,593,590,579,610,600,651,610,637,629,725,700,715,685,710)
RF=gl(n = 4, k = 5, length = 20, labels = c("160","180","200","220"))
etch_exp=data.frame(ER,RF)
plot(ER ~ RF,data = etch_exp)
boxplot(ER ~ RF, notch= TRUE)

results=aov(ER ~ RF,data = etch_exp)
summary(results)

par(mfrow=c(2,2))
plot(results)




# Export Table to LATEX
library(xtable)
lmtab=xtable(results)
print(lmtab)



# Variance- Box Cox transformation
library(car)
l=powerTransform(ER ~ RF)
#Normality Test
shapiro.test(results$residuals)
#Variance Test
bartlett.test(ER ~ RF)
leveneTest(ER ~ RF) # Robust to departure from normality
fligner.test(ER ~ RF) # nonparametric test based on the ranks


# Example Peak Discharge Data

PdD=c(0.34,0.12,1.23,0.7,1.75,0.12,0.91,2.94,2.14,
      2.36,2.86,4.55,6.31,8.37,9.75,6.09,9.82,7.24,17.15,
      11.82,10.95,17.20,14.35,16.82)
methods=gl(n = 4, k = 6, length = 24, labels = c("a","b","c","d"))

shapiro.test(PdD)
leveneTest(PdD~methods,center = median) 
fligner.test(PdD ~ methods)
PT=powerTransform(PdD ~ methods)
Tranfdata= sqrt(PdD)
hist(Tranfdata)
leveneTest(Tranfdata,methods,center = median)

shapiro.test(Tranfdata)

results=aov(Tranfdata ~ methods)
summary(results)

par(mfrow=c(2,2))
plot(results)



DC =c(118.8 ,	122.6 ,	115.6 ,	113.6 ,	119.5 ,	115.9 ,	115.8 ,	115.1 ,	116.9 ,	115.4 ,	115.6 ,	107.9)
DCMK=c(05.4 ,	101.1 ,	102.7 ,97.1, 	101.9 ,98.9 ,	100.0,99.8,102.6,100.9,104.5,93.5)
MC=c( 	102.1 ,	105.8 ,	99.6 ,	102.7 ,98.8 ,	100.9 ,	102.8 ,98.7 ,94.7 ,97.8 ,99.7 ,98.6)
dd=c(DC,DCMK,MC)
Choc=gl(n = 3, k =12, labels = c("DC","DCMK","MC"))
boxplot(dd~Choc)
shapiro.test(dd)
qqnorm(dd)
leveneTest(dd,Choc,center = median) 
summary(aov(dd~Choc))
TukeyHSD(aov(dd~Choc))



# Multiple Comparaisons
model=aov(ER~RF)
TukeyHSD(model)
pairwise.t.test(ER,RF,p.adjust.method = "bonferroni")
library(PMCMR)
#The control appears as the first level in the group vector RF. 
dunn.test.control(ER,RF)
library(agricolae)
LSD.test(model,"RF",console=TRUE,group = F,alpha = 0.01)
