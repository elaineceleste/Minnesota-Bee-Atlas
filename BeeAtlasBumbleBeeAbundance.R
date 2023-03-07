###Comparison of overall bumble bee abundance across ecological provinces

require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(pscl)
require(lattice)
require (glmmTMB)
require(car)
require(emmeans)
options(stringsAsFactors = FALSE)


####Abundance per route

### Qty per date on route, limited to routes with 3 surveys per year

qtyperrouteyeareco=read.csv("G:/My Drive/Research/R/data/MNBeeAtlas/qtyperyearrouteeco.csv")

##Plot of mean bumble bee abundances across ecological provinces
require(ggbeeswarm)
p=ggplot(qtyperrouteyeareco, aes(ecoprov, qty), ) + # y: iq
  geom_quasirandom(alpha = 0.3) +
  stat_summary(fun = mean, geom = 'point', size = 3) + # apply mean function (fun = mean) (median or other functions work too)
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0, size = 1) # apply mean_cl_normal function to data
p+xlab("Ecological province")+ ylab("Mean bumble bee abundance +/- SE")+theme_classic()+theme(text = element_text(size = 20))


fm1=glmmTMB(log(qty+1)~ecoprov+(1|routename), data=qtyperrouteyeareco)
summary(fm1)
Anova(fm1)

