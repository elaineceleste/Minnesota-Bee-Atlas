## Comparison of stem nesting bee nest frequency by MN ecological province

#Nest frequency for each species is in its own column with header "G_species"
#Nest traps were run in 3 years 2016-2018
#Nest frequency is recorded at the level of nest trap. 
#nest trap numbers are all unique. 
#Some places had traps in more than one year.  
#The column "onekm" associates traps within 1km of each other over all years.

#load data
library(readr)
Pnests <- read_csv("~/Pnestspub.csv", 
                   col_types = cols(trap = col_character(), 
                  province = col_factor(levels = c("EBF","PP", "LMF", "TAP"))))

Pnests$province <- factor(Pnests$province)
Pnests$year <- as.factor(Pnests$year)
Pnests$onekm <- as.factor(Pnests$onekm)

#remove Tallgrass Aspen Parkland, only 2 traps.
Pnests <- Pnests[Pnests$province != "TAP", , drop=FALSE] 

# subset without LMF for bees absent from LMF
PnestsPPEBF<-subset(Pnests, 
             subset= province != "LMF", ) 
summary (PnestsPPEBF)
summary(Pnests)

#graph, data familiarization
#example species in this code = M_campanulae
hist(Pnests$M_campanulae) #
aggregate(Pnests$M_campanulae, list(Pnests$province), FUN=sum)

# MODELS
library(glmmTMB)
library(TMB)

#Comparison of overall nest frequency across ecological provinces
 
#means and SD
blockabund=read.csv("ecosp.csv", header=TRUE)

#remove wasps
blockabund=subset(blockabund, select = -c(Iso_mex, Anc_ant, Anc_spin, Euo_for, Sym_can, Sym_cri))

#Means and SDs
blockabund %>%
  group_by(Biome) %>%
  summarise_at(vars(totalabund), list(name = mean))

blockabund %>%
  group_by(Biome) %>%
  summarise(sd_var1 = sd(totalabund, na.rm=TRUE))

#remove TAP
blockabund<-subset(blockabund, 
                   subset= Biome != "Tallgrass Aspen Parkland", )

#negative binomial model 
nbfit1less=glmmTMB(totalabund ~  Biome,
                   blockabund, family="nbinom2")

simulateResiduals(fittedModel = nbfit1less, plot = T) #Check assumptions
library(performance)
check_overdispersion(nbfit1less) 


library(car)
Anova(nbfit1less)
pairs=pairs(emmeans(nbfit1less, "Biome"), adjust = "none") 

ggplot(blockabund, aes(x=as.factor(Biome), y=totalabund)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Biome")



# 1) M_campanulae: no M_campanulae in EBF so use reduced data set.
#Poisson model
poisfitless = glmmTMB(M_campanulae ~  province + (1|onekm)+ (1|year), 
                      PnestsPPEBF, family="poisson")
library(DHARMa)
simulateResiduals(fittedModel = poisfitless, plot = T) #Check assumptions. 
library(performance)
check_overdispersion(poisfitless) 

#Negative Binomial
nbfit1less = glmmTMB(M_campanulae ~  province + (1|onekm)+ (1|year),
                     PnestsPPEBF, family="nbinom2")
simulateResiduals(fittedModel = nbfit1less, plot = T) #Check assumptions

summary(poisfitless) #AIC 
summary(nbfit1less) #AIC 
library(car)
Anova(nbfit1less) #significant difference between any province? 

# 2) H_carinata. all provinces
#Poisson
poisfit2 = glmmTMB(H_carinata ~  province + (1|onekm)+ (1|year),
                   Pnests, family="poisson")
simulateResiduals(fittedModel = poisfit2, plot = T) #assumptions 
check_overdispersion(poisfit2) 

#Negative binomial
nbfit2 = glmmTMB(H_carinata ~  province + (1|onekm)+ (1|year),
                 Pnests, family="nbinom2")
simulateResiduals(fittedModel = nbfit2, plot = T) #assumptions

summary(poisfit2) #AIC 
summary(nbfit2) #AIC

Anova(nbfit2) 
pairs(emmeans(nbfit2, "province"), adjust = "none") 

# 3) O_lignaria
#Poisson
poisfit3 = glmmTMB(O_lignaria ~  province + (1|onekm)+ (1|year),
                   Pnests, family="poisson")
simulateResiduals(fittedModel = poisfit3, plot = T) #assumptions. 

#Negative binomial
nbfit3 = glmmTMB(O_lignaria ~  province + (1|onekm)+ (1|year),
                 Pnests, family="nbinom2")
simulateResiduals(fittedModel = nbfit3, plot = T) #assumptions. 
summary(nbfit3)

Anova(nbfit3)
pairs(emmeans(nbfit3, "province"), adjust = "none") 

# 4) O_pumila. absent from LMF
#Poisson
poisfit4= glmmTMB(O_pumila ~  province + (1|onekm)+ (1|year),
                  PnestsPPEBF, family="poisson")
simulateResiduals(fittedModel = poisfit4, plot = T) #assumptions

#Negative binomial
nbfit4 = glmmTMB(O_pumila ~  province + (1|onekm)+ (1|year),
                 PnestsPPEBF, family="nbinom2")
simulateResiduals(fittedModel = nbfit4, plot = T) #assumptions 

summary(poisfit4) #AIC 
summary(nbfit4) #AIC  

Anova(nbfit4)
pairs(emmeans(nbfit4, "province"), adjust = "none")

# 5) O_tersula
#Poisson
poisfit5 = glmmTMB(O_tersula ~  province + (1|onekm)+ (1|year),
                   Pnests, family="poisson")
simulateResiduals(fittedModel = poisfit5, plot = T) #assumptions 
check_overdispersion(poisfit5) 

#Negative binomial
nbfit5 = glmmTMB(O_tersula ~  province + (1|onekm)+ (1|year),
                 Pnests, family="nbinom2")
simulateResiduals(fittedModel = nbfit5, plot = T) #assumptions

summary(poisfit5) #AIC
summary(nbfit5) #AIC

Anova(nbfit5) 
pairs(emmeans(nbfit5, "province"), adjust = "none")

# 6) M_pugnata
#Poisson
poisfit6 = glmmTMB(M_pugnata ~  province + (1|onekm)+ (1|year),
                   Pnests, family="poisson")
simulateResiduals(fittedModel = poisfit6, plot = T) #assumptions. 
check_overdispersion(poisfit6) 
#Negative binomial
nbfit6 = glmmTMB(M_pugnata ~  province + (1|onekm)+ (1|year),
                 Pnests, family="nbinom2")
simulateResiduals(fittedModel = nbfit6, plot = T) #assumptions

summary(poisfit6)  #aic  
summary(nbfit6) #aic 

Anova(nbfit6)  
pairs(emmeans(nbfit6, "province"), adjust = "none") 

# 7) M_relativa
#Poisson
poisfit7 = glmmTMB(M_relativa ~  province + (1|onekm)+ (1|year),
                   Pnests, family="poisson")
simulateResiduals(fittedModel = poisfit7, plot = T) #assumptions. Dispersion p=.04

#negative binomial
nbfit7 = glmmTMB(M_relativa ~  province + (1|onekm)+ (1|year),
                 Pnests, family="nbinom2")
simulateResiduals(fittedModel = nbfit7, plot = T) #assumptions

summary(nbfit7) 

Anova(nbfit7) 
pairs(emmeans(nbfit7, "province"), adjust = "none") 


sessionInfo()
