setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioThesis\\Data")

library(plyr)

dat.td<- read.csv("thaw_depth17.csv")
dat.shrub<- read.csv("shrub_all.csv")
dat.sden<-read.csv("shrub_dens.csv")

td.x<- aggregate(dat.td$TD, by= list(dat.td$Site, dat.td$Day.of.year), FUN= "mean")
colnames(td.x)<- c("site", "doy", "td")

#should I add SE to fig? 
td.sd<- aggregate(dat.td$TD, by= list(dat.td$Site, dat.td$Day.of.year), FUN= "sd")
td.l<- aggregate(dat.td$TD, by= list(dat.td$Site, dat.td$Day.of.year), FUN= "length")

td.x$SE<- td.sd$x/sqrt(td.l$x)

#t.tests
  #early season
t.test(dat.td$TD[dat.td$Day.of.year== "169" & dat.td$Site== "ld"],
       dat.td$TD[dat.td$Day.of.year== "171" & dat.td$Site== "hd"])

  #mid season
t.test(dat.td$TD[dat.td$Day.of.year== "198" & dat.td$Site== "ld"],
       dat.td$TD[dat.td$Day.of.year== "198" & dat.td$Site== "hd"])
  #late season
t.test(dat.td$TD[dat.td$Day.of.year== "224" & dat.td$Site== "ld"],
       dat.td$TD[dat.td$Day.of.year== "225" & dat.td$Site== "hd"])
###Linear Regression###
plot(td.x$doy[td.x$site== "hd"], td.x$td[td.x$site== "hd"], pch=19, col= "green",
      ylim= c(0,80), xlim=c(160,230), ylab= "Thaw Depth (cm)", xlab= "Day of Year")
points(td.x$doy[td.x$site=="ld"], td.x$td[td.x$site== "ld"], pch=19, col= "blue")

legend(214.5, 30, c("high density", "low density"), pch= c(19,19), col= c("green", "blue"))

fit.hd<- lm(td.x$td[td.x$site== "hd"]~ td.x$doy[td.x$site== "hd"])
summary(fit.hd)

abline(fit.hd, lwd=1, col= "green")

fit.ld<- lm(td.x$td[td.x$site== "ld"]~ td.x$doy[td.x$site== "ld"])
summary(fit.ld)

abline(fit.ld, lwd=1, col= "blue")

####shrub data###

#basal diameter
#seperated by species
bd.x<-aggregate(dat.shrub$BD, by= list(dat.shrub$Site, dat.shrub$Species), FUN= "mean")
colnames(bd.x)<- c("site", "species", "BD")

bd.sd<-aggregate(dat.shrub$BD, by= list(dat.shrub$Site, dat.shrub$Species), FUN= "sd")
colnames(bd.sd)<- c("site", "species", "BD_sd")
bd.l<-aggregate(dat.shrub$BD, by= list(dat.shrub$Site, dat.shrub$Species), FUN= "length")
colnames(bd.l)<- c("site", "species", "BD_length")

bd.x$SE<- bd.sd$BD_sd/sqrt(bd.l$BD_length)

#betula BD
t.test(dat.shrub$BD[dat.shrub$Site=="LDF2" & dat.shrub$Species== "BN"],
       dat.shrub$BD[dat.shrub$Site=="S23" & dat.shrub$Species== "BN"])
#salix BD
t.test(dat.shrub$BD[dat.shrub$Site=="LDF2" & dat.shrub$Species== "SA"],
       dat.shrub$BD[dat.shrub$Site=="S23" & dat.shrub$Species== "SA"])
# bd between species
t.test(dat.shrub$BD[dat.shrub$Site=="LDF2" & dat.shrub$Species== "BN"],
       dat.shrub$BD[dat.shrub$Site=="LDF2" & dat.shrub$Species== "SA"])
t.test(dat.shrub$BD[dat.shrub$Site=="S23" & dat.shrub$Species== "BN"],
      dat.shrub$BD[dat.shrub$Site=="S23" & dat.shrub$Species== "SA"])
              
       
#bd generally
t.test(dat.shrub$BD[dat.shrub$Site=="LDF2"],
       dat.shrub$BD[dat.shrub$Site== "S23"])

#basal area
dat.shrub$area<-pi*((0.5*dat.shrub$BD)^2)
BA<-aggregate(dat.shrub$area, by = list(dat.shrub$Site, dat.shrub$Species), FUN= "mean")
BA.sd<- aggregate(dat.shrub$area, by = list(dat.shrub$Site, dat.shrub$Species), FUN= "sd")
BA.se<-  aggregate(dat.shrub$area, by = list(dat.shrub$Site, dat.shrub$Species), FUN= "length")

BA$se<- BA.sd$x/ sqrt(BA.se$x)

t.test(dat.shrub$area[dat.shrub$Site=="LDF2" & dat.shrub$Species== "BN"],
       dat.shrub$area[dat.shrub$Site=="S23" & dat.shrub$Species== "BN"])

t.test(dat.shrub$area[dat.shrub$Site=="LDF2" & dat.shrub$Species== "SA"],
       dat.shrub$area[dat.shrub$Site=="S23" & dat.shrub$Species== "SA"])
# ba between species
t.test(dat.shrub$area[dat.shrub$Site=="LDF2" & dat.shrub$Species== "BN"],
       dat.shrub$area[dat.shrub$Site=="LDF2" & dat.shrub$Species== "SA"])
t.test(dat.shrub$area[dat.shrub$Site=="S23" & dat.shrub$Species== "BN"],
       dat.shrub$area[dat.shrub$Site=="S23" & dat.shrub$Species== "SA"])



#shrubdensity
#betula
dens.bn<-aggregate(dat.sden$BN_Dens, by= list(dat.sden$Site), FUN= "mean")
colnames(dens.bn)<- c("Site", "BN_Dens")

dens.bn.sd<-aggregate(dat.sden$BN_Dens, by= list(dat.sden$Site), FUN= "sd")
colnames(dens.bn.sd)<- c("site", "BN_sd")

dens.bn.l<-aggregate(dat.sden$BN_Dens, by= list(dat.sden$Site), FUN= "length")
colnames(dens.bn.l)<- c("site","BN_length")

dens.bn$SE<- dens.bn.sd$BN_sd/sqrt(dens.bn.l$BN_length)

t.test(dat.sden$BN_Dens[dat.sden$Site=="LDF2"], dat.sden$BN_Dens[dat.sden$Site=="S23"])
#salix
dens.sa<-aggregate(dat.sden$SA_Dens, by= list(dat.sden$Site), FUN= "mean")
colnames(dens.sa)<- c("Site", "SA_Dens")

dens.sa.sd<-aggregate(dat.sden$SA_Dens, by= list(dat.sden$Site), FUN= "sd")
colnames(dens.sa.sd)<- c("site", "SA_sd")

dens.sa.l<-aggregate(dat.sden$BN_Dens, by= list(dat.sden$Site), FUN= "length")
colnames(dens.sa.l)<- c("site","SA_length")

dens.sa$SE<- dens.sa.sd$SA_sd/sqrt(dens.sa.l$SA_length)

t.test(dat.sden$SA_Dens[dat.sden$Site=="LDF2"], dat.sden$SA_Dens[dat.sden$Site=="S23"])
#density differences between two species at each site?
#ld
t.test(dat.sden$SA_Dens[dat.sden$Site=="LDF2"], dat.sden$BN_Dens[dat.sden$Site=="LDF2"])
#hd
t.test(dat.sden$SA_Dens[dat.sden$Site=="S23"], dat.sden$BN_Dens[dat.sden$Site=="S23"])

###Biomass
#betula

bmass.bn<-aggregate(dat.sden$BN_Bio, by= list(dat.sden$Site), FUN= "mean")
colnames(bmass.bn)<- c("Site", "BN_Bio")

bmass.bn.sd<-aggregate(dat.sden$BN_Bio, by= list(dat.sden$Site), FUN= "sd")
colnames(bmass.bn.sd)<- c("site", "BN_sd")

bmass.bn.l<-aggregate(dat.sden$BN_Bio, by= list(dat.sden$Site), FUN= "length")
colnames(bmass.bn.l)<- c("site","BN_length")

bmass.bn$SE<- bmass.bn.sd$BN_sd/sqrt(bmass.bn.l$BN_length)

t.test(dat.sden$BN_Bio[dat.sden$Site=="LDF2"], dat.sden$BN_Bio[dat.sden$Site=="S23"])

#salix
bmass.sa<-aggregate(dat.sden$SA_Bio, by= list(dat.sden$Site), FUN= "mean")
colnames(bmass.sa)<- c("Site", "SA_Bio")

bmass.sa.sd<-aggregate(dat.sden$SA_Bio, by= list(dat.sden$Site), FUN= "sd")
colnames(bmass.sa.sd)<- c("site", "SA_sd")

bmass.sa.l<-aggregate(dat.sden$SA_Bio, by= list(dat.sden$Site), FUN= "length")
colnames(bmass.sa.l)<- c("site","SA_length")

bmass.sa$SE<- bmass.sa.sd$SA_sd/sqrt(bmass.sa.l$SA_length)

t.test(dat.sden$SA_Bio[dat.sden$Site=="LDF2"], dat.sden$SA_Bio[dat.sden$Site=="S23"])

#bionass differences between two species at each site?
#ld
t.test(dat.sden$SA_Bio[dat.sden$Site=="LDF2"], dat.sden$BN_Bio[dat.sden$Site=="LDF2"])
#hd
t.test(dat.sden$SA_Bio[dat.sden$Site=="S23"], dat.sden$BN_Bio[dat.sden$Site=="S23"])

