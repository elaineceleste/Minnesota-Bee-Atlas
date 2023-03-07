#Code for Minnesota Bee Atlas trap nest information and land cover within 250 m with summary by ecological province

require(vegan)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(ggpubr)
require(pscl)

atlassp=read.csv("LU250nestsNoZeroNestSpgr10.csv", header=TRUE)
str(atlassp)


###convert to presence absence
species=select(atlassp, -c(1:15))
species[species > 0]<- 1
str(species)

presabs=select(atlassp, c(1:15))

presabs <- cbind(presabs, species)
str(presabs)




#RDA
species=presabs[,-c(1:16)]
str(species)
head(species)
head(presabs)


env=presabs[,c("Openwater","Developed", "ForestedGrouped", "Grasslands", "Crops", "Wetlands", "EvergreenMixed","DeciduousShrub")]
str(env)
head(env)

envEco=presabs[,c("province")]

a.rda <- rda(species ~ Developed+ ForestedGrouped+Grasslands+Crops, data=env)
a.rda
summary(a.rda)
screeplot(a.rda)

plot(a.rda, display=c("species", "bp"))



a.rda <- rda(species ~ Developed+ EvergreenMixed+DeciduousShrub+Grasslands+Crops+Wetlands, data=env)
a.rda
summary(a.rda)
screeplot(a.rda)
##significance overall
anova.cca(a.rda, step = 1000)
#sig of landuse
anova.cca(a.rda, step = 1000, by="term")
#sig of axis
anova.cca(for.a.rda, step = 1000, by="axis")

sig.a.rda <- rda(species ~ Developed+ EvergreenMixed+DeciduousShrub+Grasslands, data=env)
anova.cca(sig.a.rda, step = 1000)
#sig of landuse
anova.cca(sig.a.rda, step = 1000, by="term")
#sig of axis
plot(sig.a.rda, display=c("species", "bp"))



fwd.sel <- ordiR2step(rda(species ~ 1, data = env), # lower model limit (simple!)
                      scope = formula(a.rda), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # can't surpass the "full" model's R2
                      pstep = 1000,
                      trace = FALSE) # change to TRUE to see the selection process!

fwd.sel$call

###rda(formula = species ~ Grasslands + Developed + DeciduousShrub + EvergreenMixed, data = env)

for.a.rda <- rda(species ~ Developed+ EvergreenMixed++Grasslands, data=env)
summary(for.a.rda)

RsquareAdj(for.a.rda)
                      
plot(a.rda, display=c("species", "bp"))


##significance overall
anova.cca(for.a.rda, step = 1000)
#sig of landuse
anova.cca(for.a.rda, step = 1000, by="term")
#sig of axis
anova.cca(for.a.rda, step = 1000, by="axis")

plot(for.a.rda, display=c("species", "bp"))



###RDA forward select with grouped forest
env=presabs[,c("Openwater","Developed", "ForestedGrouped", "Grasslands", "Crops", "Wetlands", "EvergreenMixed","DeciduousShrub")]
str(env)
head(env)


a.rda <- rda(species ~ Developed+ ForestedGrouped+Grasslands+Crops+Wetlands, data=env)
a.rda
summary(a.rda)
screeplot(a.rda)
##significance overall
anova.cca(a.rda, step = 1000)
#sig of landuse
anova.cca(a.rda, step = 1000, by="term")
#sig of axis
anova.cca(for.a.rda, step = 1000, by="axis")

##took out wetlands crops n.s.
sig.a.rda <- rda(species ~ Developed+ ForestedGrouped+Grasslands, data=env)
anova.cca(sig.a.rda, step = 1000)
#sig of landuse
anova.cca(sig.a.rda, step = 1000, by="term")
#sig of axis
plot(sig.a.rda, display=c("species", "bp"))

summary(sig.a.rda)


fwd.sel <- ordiR2step(rda(species ~ 1, data = env), # lower model limit (simple!)
                      scope = formula(a.rda), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # can't surpass the "full" model's R2
                      pstep = 1000,
                      trace = FALSE) # change to TRUE to see the selection process!

fwd.sel$call

rda(formula = species ~ ForestedGrouped + Grasslands + Developed, 
    data = env)
for.a.rda <- rda(species ~ ForestedGrouped + Grasslands + Developed, data=env)
summary(for.a.rda)

RsquareAdj(for.a.rda)

plot(a.rda, display=c("species", "bp"))


##significance overall
anova.cca(for.a.rda, step = 1000)
#sig of landuse
anova.cca(for.a.rda, step = 1000, by="term")
#sig of axis
anova.cca(for.a.rda, step = 1000, by="axis")

plot(for.a.rda, display=c("species", "bp"))
orditkplot(for.a.rda)
