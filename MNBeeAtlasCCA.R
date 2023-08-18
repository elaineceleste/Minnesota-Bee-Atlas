#Code for Canonical Correspondence Analysis for Minnesota Bee Atlas

require(vegan)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)
require(ggpubr)
require(pscl)

#abundances of bumble bee species summarized over route and year for all routes with at least 3 surveys per year
atlasspmatrix=read.csv("atlasspmatrixyear.csv", header=TRUE, row.names=1)

###land use with 2 km of all stops into one route land use layer
routelanduse=read.csv("landuseroutefix.csv",header=TRUE, row.names=1)

landuse= subset(routelanduse, select = -c(route, ecoprov, routename) )
str(landuse)
spmatrix=subset(atlasspmatrix, select = -c(year, route) )
str(spmatrix)

####### Land use
cca <- cca(spmatrix~crops+developed+forested+grasslands+openwater+wetlands, data=landuse)
summary(cca)
RsquareAdj(cca)
anova.cca(cca) ###test overal significance
anova.cca(cca, by="axis")   ###test sig of all axes
vif.cca(cca)




#####crops removed due to to covariance with wetlands and water removed due to correlation <0.4 in CCA1,2,3
cca <- cca(spmatrix~developed+wetlands+forested+grasslands, data=landuse)
summary(cca)
RsquareAdj(cca)
anova.cca(cca) ###test overal significance
anova.cca(cca, by="axis")   ###test sig of all axes
vif.cca(cca)
goodness(cca)



#### CCA with low inertial species removed according to goodness  less than 5% of inertia for first 2 axes. 

atlasspmatrix=read.csv("atlasspmatrixyear.csv", header=TRUE, row.names=1)
routelanduse=read.csv("landuseroutefix.csv",header=TRUE, row.names=1)

landuse= subset(routelanduse, select = -c(route, ecoprov, routename) )
str(landuse)
spmatrix=subset(atlasspmatrix, select = -c(year, route, citrinus, insularis, pensylvanicus, rufocinctus) )

str(spmatrix)
cca <- cca(spmatrix~developed+wetlands+forested+grasslands, data=landuse)



plot(cca, choices=c(1,2), display=c("species", "landuse"), scaling="species")
text(cca, scaling = 2, display = "bp",cex = 1) 
orditkplot(cca) #####can move labels


fit=envfit(cca, landuse,perm=99)
scores(fit, "vectors")



