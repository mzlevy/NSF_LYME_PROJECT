
#setting the directory
setwd("~/Dropbox/NSF_Lyme_preproposal/data/")
#bringing in the datafile with both vaccinated and sham together and names changed to values R can read
d<-read.csv("OspA_XenodiagnosisData_perMouseIndividual_July2015.csv")
attach(d)
#make a variable for vaccianted--note theres a space after the True
vac<-Vaccine=="True "


#model of the % of infected ticks (not controlling for the raw number infected)
mod1<-glm(Tot_infected~vac, offset=TOT_PROCESSED, family=poisson)
summary(mod1)

#model of the count of infected ticks (not controlling for # captured)
mod2<-glm(Tot_infected~vac, family=poisson)
summary(mod2)

install.packages("pscl")
library("pscl")
m1 <- zeroinfl(Tot_infected ~ vac | vac,
  dist = "negbin", EM = TRUE)
summary(m1)

vacs<- d[vac==1,]
numvacs<- dim(vacs)[1]
shams<- d[vac==0,]
numshams<- dim(shams)[1]
#twenty shams in data set

permutesampsize<-function(N,sims)
	{

	fakeresults<-1:sims*NA
	for(i in 1:sims)
		{	
        sampvac<-sample(numvacs,N,replace=T)
		fakevacs<-vacs[sampvac,]
		sampshams<-sample(numshams,N,replace=T)
		fakeshams<-shams[sampshams,]
		faked<-rbind(fakevacs,fakeshams)
		faked$vac<-faked$Vaccine=="True "
		#faked<-append(/(20,N,replace=T),]
		m<-zeroinfl(Tot_infected ~ vac | vac, data=faked,dist = "negbin", EM = TRUE)
		pcount<-coef(summary(m))$count[2,4]
		pzero<-coef(summary(m))$zero[2,4]
		fakeresults[i]<-pcount
		}
		return(fakeresults)
	}


sims<-200

out40<-permutesampsize(40,sims)	
p40<-sum(out40<.05)/sims
out50<-permutesampsize(50,sims)	
p50<-sum(out50<.05)/sims
out60<-permutesampsize(60,sims)	
p60<-sum(out60<.05)/sims

out100<-permutesampsize(100,sims)	
p100<-sum(out100<.05)/sims

#p100<-.95

permutesampsize<-function(N,sims)
	{

	fakeresults<-1:sims*NA
	for(i in 1:sims)
		{	
        sampvac<-sample(numvacs,N,replace=T)
		fakevacs<-vacs[sampvac,]
		sampshams<-sample(numshams,N,replace=T)
		fakeshams<-shams[sampshams,]
		faked<-rbind(fakevacs,fakeshams)
		faked$vac<-faked$Vaccine=="True "
		#faked<-append(/(20,N,replace=T),]
		m<-glm(Tot_infected~vac, family=poisson)

		pcount<-coef(summary(m))$count[2,4]
		pzero<-coef(summary(m))$zero[2,4]
		fakeresults[i]<-pcount
		}
		return(fakeresults)
	}
out50<-permutesampsize(50,100)	
	
	
	sample(N,20)
zeroinfl(Tot_infected ~ vac | vac,
  dist = "negbin", EM = TRUE)

Call:
glm(formula = Tot_infected ~ vac, family = poisson, offset = TOT_PROCESSED)


Model 1 output
Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-6.6055  -0.8521   1.0297   1.6493   3.6839  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -5.2311     0.1085 -48.230   <2e-16 ***
vacTRUE       0.1078     0.1627   0.662    0.508    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 157.02  on 41  degrees of freedom
Residual deviance: 156.59  on 40  degrees of freedom
AIC: 275.92

Number of Fisher Scoring iterations: 5


#
Pearson residuals:
    Min      1Q  Median      3Q     Max 
-1.6739 -0.9933  0.2954  0.8525  1.8570 

Count model coefficients (negbin with log link):
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)   1.5405     0.1172  13.145   <2e-16 ***
vacTRUE      -0.3019     0.1779  -1.697   0.0897 .  
Log(theta)    3.8446     4.1265   0.932   0.3515    

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)   
(Intercept)  -2.3216     0.8451  -2.747  0.00601 **
vacTRUE       0.1711     1.1787   0.145  0.88461   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 46.7414 
Number of iterations in BFGS optimization: 1 
Log-likelihood: -93.78 on 5 Df