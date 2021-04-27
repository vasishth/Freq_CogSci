gw<-read.table("data/gibsonwu2012data.txt",
               header=TRUE)
## sum-contrast coding of predictor:
gw$so <- ifelse(
  gw$type%in%c("subj-ext"),-1,1)
## subset critical region
gw1<-subset(gw,region=="headnoun")

## frequentist analysis:
library(lme4)
m_lmer<-lmer(rt~so + (1+so||subj)+(1+so||item),gw1)
summary(m_lmer)


library(designr)

## example
## define data frame:
design<-fixed.factor("so", levels=c("s", "o")) +
  random.factor("subj", instances=30)+
  random.factor("item", instances=16)
dat <- design.codes(design)
contrasts(dat$so) <- c(-1, +1)

tvals<-rep(NA,100)

for(i in 1:100){
## generate data:
dat$ysim <- simLMM(formula = ~ 1 + so + (1 + so || subj) + (1+so||item),
                   data = dat,
                   LMM = m_lmer,
#                   Fixef = c(6, 0.02),
#                   VC_sd = list(c(.30,.10),c(.20,.10), 0.5),
#                   CP =  0.3,
                   empirical=FALSE,verbose=FALSE,family="gaussian")

dat$Xn <- ifelse(dat$so=="s",-1,1)
## fit model to simulated data:
m<-lme4::lmer(ysim ~ Xn + (Xn || subj) + (Xn||item), 
              data=dat, control=lmerControl(calc.derivs=FALSE))
## save t-value
tvals[i]<-summary(m)$coefficients[2,3]
}

mean(abs(tvals)>2)

design <- fixed.factor("x", levels=c("-1", "1"), replications=2) +
  random.factor("subj", instances=15)
simdata <- design.codes(design)
simdata$x <- as.numeric(as.character(simdata$x))

simdata

rtsimmat <- matrix(NA,nrow(simdata),nsim)
# We take exp() since we assume response times are log-normally distributed
for (i in 1:nsim) 
  rtsimmat[,i] <- exp(simLMM(formula=~ x + (x | subj), 
                             dat=simdata, 
                             Fixef=c(beta0[i], beta1[i]), 
                             VC_sd=list(c(sigma_u0[i], sigma_u1[i]), sigma[i]), 
                             CP=rho_u[i], empirical=FALSE))


design <- fixed.factor("distance", c("long","short")) + 
       fixed.factor("predictability", c("high","low")) + 
       random.factor("Subject",instances=4) + ## 4x4 = 16 subjects
       random.factor("Item",instances=4) +    ## 4x4=16 items
       random.factor(c("Subject","Item"), groups = c("distance","predictability"))

simdat<-design.codes(design)

unique(simdat$Subject)
unique(simdat$Item)


       