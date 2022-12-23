###Comparison of overall bumble bee abundance across ecological provinces

require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(ggpubr)
require(pscl)
require(lme4)
library(lmerTest)
options(stringsAsFactors = FALSE)


####Abundance per route

### Qty per date on route, limited to routes with 3 surveys per year

qtyperrouteyeareco=read.csv("qtyperyearrouteeco.csv")


ggboxplot(qtyperrouteyeareco, x = "ecoprov", y = "qty", 
          order = c("Broadleaf", "Laurentian", "Prairie"),
          ylab = "Average bumble bee abundance per route per year", xlab = "Ecological Province")

require(ggbeeswarm)
p=ggplot(qtyperrouteyeareco, aes(ecoprov, qty), ) + # y: iq
  geom_quasirandom(alpha = 0.3) +
  stat_summary(fun = mean, geom = 'point', size = 3) + # apply mean function (fun = mean) (median or other functions work too)
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0, size = 1) # apply mean_cl_normal function to data
p+xlab("Ecological province")+ ylab("Mean bumble bee abundance +/- SE")+theme_classic()+theme(text = element_text(size = 20))



fm1=lmer(log(qty+1)~ecoprov+(1|year)+(1|routename), data=qtyperrouteyeareco)
summary(fm1)
plot(fm1)
fm1
anova(fm1)


