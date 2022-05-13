##LOADING RELEVANT PACKAGES
library(car)
library(Boruta)
library(Hmisc)
library(moments)
library(rcompanion)
library(fBasics)
library(caret)
library(tidyverse)
library(googlesheets4)

##IMPORTING DATA TO R
tab1<-read_sheet("https://docs.google.com/spreadsheets/d/1q7DHGdog8mj3AkHq6GzS8J-1V7g6CmTmQ5ckmcVEH04/edit#gid=1955189566")
tab1<-rename(tab1,TC=total.capital.employed, PBIT=profit.before.interest.and.tax,TA=total.assets)

##CALCULATING RATIOS
tab2<-tab1%>%
  mutate(CR=CA/CL, EMR=PBIT/SALES, ROI=PBIT/TC, DE= TL/TC, ROTA=EBIT/TA, 
         RCATFA=CA/FA, TATR=SALES/TA, FATR=SALES/FA, CETR=SALES/TC)%>%
  select(Firm:Yr,CR:CETR,Status,Sta)

##FEATURE SELECTION
#Using Boruta 
Boutput<-Boruta(Status~.,data = na.omit(tab2))
names(Boutput)

Bsig<-getSelectedAttributes(Boutput, withTentative = FALSE)
print(Bsig)

B2<-TentativeRoughFix(Boutput)
B2sig<-getSelectedAttributes(B2)
print(B2sig)

Bimp<-attStats(B2)

Bimp2<-Bimp[Bimp$decision != 'Rejected', c('meanImp','decision')]
Bimp3<-head(Bimp2[order(-Bimp2$meanImp),])

write.csv(Bimp3, "t.csv")

plot(Boutput, cex.axis=.7, las = 2, xlab = "", main = "Variable Importance")


##ASSUMPTIONS OF OUR MODEL

#NO MULTICOLINEARITY
#Using vif to test multicolinearity and eliminate variables
modelz<-glm(Status~., data = tab3, family = "binomial")
car::vif(modelz)

modelz1<-glm(Status~. -FATR, data = tab3, family = "binomial")
car::vif(modelz1)

modelz2<-glm(Status~. -FATR -CETR, data = tab3, family = "binomial")
car::vif(modelz2)

modelz3<-glm(Status~. -FATR -TATR -ROI, data = tab3, family = "binomial")
car::vif(modelz3)

modelz4<-glm(Status~. -CETR -TATR -ROI, data = tab3, family = "binomial")
car::vif(modelz4)

modelz5<-glm(Status~. -CETR -TATR -RCATFA, data = tab3, family = "binomial")
car::vif(modelz5)

#NO CORELATION
#Test of correlation among all variables
tab3<-tab2%>%
  select(CR:Status)%>%
  sapply(as.numeric)%>%
  view()

r1<-rcorr(as.matrix(tab3))
r1

#Test of correlation among selected variables
tab4<-tab2%>%
  select(EPS,EMR,ROTA,ROI,FATR,TATR,Status)%>%
  sapply(as.numeric)%>%
  view()

r2<-rcorr(as.matrix(tab4))
r2

write.csv(r2$P,"pval.csv")

##PLOTTING 
#Earnings per Share against time
ggplot(tab2)+
  geom_smooth(aes(Yr,EPS, col=Sta))

#Return on Investment against time
ggplot(tab2)+
  geom_smooth(aes(Yr,ROI, col=Sta))

#Earnings Margin Ratio
ggplot(tab2)+
  geom_smooth(aes(Yr,EMR, col=Sta))

#Return on Total Assets
ggplot(tab2)+
  geom_smooth(aes(Yr,ROTA, col=Sta))

#Total Asset Turnover Ratio
ggplot(tab2)+
  geom_smooth(aes(Yr,TATR, col=Sta))


##FITTING A MODEL
model1<-glm(Status~CR+EMR+ROI+DE+ROTA+FATR+EPS, data = tab2, family = "binomial")
summary(model1)

model2<-glm(Status~CR+EMR+ROI+ROTA+FATR+EPS, data = tab2, family = "binomial")
summary(model2)

model3<-glm(Status~EMR+ROI+ROTA+FATR+EPS, data = tab2, family = "binomial")
summary(model3)

model4<-glm(Status~EMR+ROI+ROTA+EPS, data = tab2, family = "binomial")
summary(model4)

##TESTING GOODNESS OF FIT OF THE MODEL
anova(model1,model2, test = "Chisq")
anova(model3,model2, test = "Chisq")
anova(model3,model4, test = "Chisq")

#Thus model 4 is the best

