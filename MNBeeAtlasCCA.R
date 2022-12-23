#Code for Canonical Correspondence Analysis for Minnesota Bee Atlas

require(vegan)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(ggpubr)
require(pscl)

atlasspmatrix=read.csv("atlasspmatrixyear.csv", header=TRUE, row.names=1)
routelanduse=read.csv("routelanduse.csv",header=TRUE, row.names=1)

landuse= subset(routelanduse, select = -c(years,route, ecoprov, lat, long, routename) )
str(landuse)
spmatrix=subset(atlasspmatrix, select = -c(year, route) )
str(spmatrix)

####### Land use
cca <- cca(spmatrix~crops+developed+forest+grassland+water+wetland, data=landuse)
summary(cca)
RsquareAdj(cca)
anova.cca(cca) ###test overal significance
anova.cca(cca, by="axis")   ###test sig of all axes
vif.cca(cca)

scrs <- scores(cca, display = c("landuse", "species"), scaling = 2)
str(scrs, max = 1)

xlim <- with(scrs, range(species[,1]))
ylim <- with(scrs, range(species[,2]))
plot(cca, type = "n", scaling = 1)
orditorp(cca, display = "species", scaling = 0.2,
         col = "black", cex = .6, air=.9, pch="")
text(cca, scaling = 2, display = "bp",cex = 1) 
orditkplot(cca) #####can move labels
orditkplot(cca, display = "species")

###jitter
ordipointlabel(cca,display="species",pch="", scaling=.5) 
text(cca, dis = "bp") 

fit=envfit(cca, landuse,perm=99)
scores(fit, "vectors")
spenvcor(cca.5) 
intersetcor(cca.300)

###jitter
ordipointlabel(cca.300,display="species",pch="", scaling=2) 
text(cca.300, dis = "bp") 



##### Land use wetlands removed due to to covariance with crops
cca <- cca(spmatrix~developed+crops+forest+grassland+water, data=landuse)
summary(cca)
RsquareAdj(cca)
anova.cca(cca) ###test overal significance
anova.cca(cca, by="axis")   ###test sig of all axes
vif.cca(cca)
goodness(cca)

scrs <- scores(cca, display = c("landuse", "species"), scaling = 2)
str(scrs, max = 1)

xlim <- with(scrs, range(species[,1]))
ylim <- with(scrs, range(species[,2]))
plot(cca, type = "n", scaling = 1)
orditorp(cca, display = "species", scaling = 0.2,
         col = "black", cex = .6, air=.9, pch="")
text(cca, scaling = 2, display = "bp",cex = 1) 
orditkplot(cca) #####can move labels
orditkplot(cca, display = "species")

###jitter
ordipointlabel(cca,display="species",pch="", scaling=.5) 
text(cca, dis = "bp") 

fit=envfit(cca, landuse,perm=99)
scores(fit, "vectors")



#### CCA with low inertial species removed according to goodness  less than 5% of inertia for first 2 axes. 

atlasspmatrix=read.csv("atlasspmatrixyearCCA.csv", header=TRUE, row.names=1)
routelanduse= read.csv("routelanduse.csv", header=TRUE, row.names=1)

landuse= subset(routelanduse, select = -c(years,route, ecoprov, lat, long, routename) )
str(landuse)
spmatrix=subset(atlasspmatrix, select = -c(year, route) )
str(spmatrix)


#####wetlands removed due to to covariance with crops and water removed due to correlation <0.4 in CCA1,2,3
cca <- cca(spmatrix~developed+crops+forest+grassland, data=landuse)
summary(cca)
RsquareAdj(cca)
anova.cca(cca) ###test overal significance
anova.cca(cca, by="axis")   ###test sig of all axes
vif.cca(cca)
goodness(cca)

scrs <- scores(cca, display = c("landuse", "species"), scaling = 2)
str(scrs, max = 1)

xlim <- with(scrs, range(species[,1]))
ylim <- with(scrs, range(species[,2]))
plot(cca, type = "n", scaling = 1)
orditorp(cca, display = "species", scaling = 0.2,
         col = "black", cex = .6, air=.9, pch="")
text(cca, scaling = 2, display = "bp",cex = 1) 
orditkplot(cca) #####can move labels
orditkplot(cca, display = "species")

###jitter
ordipointlabel(cca,display="species",pch="", scaling=.5) 
text(cca, dis = "bp") 

fit=envfit(cca, landuse,perm=99)
scores(fit, "vectors")

