
setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioThesis\\Data")

library(plyr)

datwp_f<-read.csv("rwp17.csv")
datVPD<- read.csv("RH.VP4.csv")
datT<- read.csv("TempC.VP4.csv")

datMet<- join(datVPD, datT, by= c("doy", "year", "hour", "site"), type = "inner")
datMet$RH_fix<- ifelse(datMet$RH.VP4 >= 1, 0.999, datMet$RH.VP4)
datMet$E_sat<- 0.611* exp((17.502*datMet$TempC.VP4)/ (datMet$TempC.VP4 + 240.97))
datMet$D<- datMet$E_sat - (datMet$RH_fix*datMet$E_sat)


datwp_p<- datwp_f[datwp_f$Time_d>=11.5 & datwp_f$Time_d<=15.5,]

datwp_p1<- aggregate(datwp_p$wp,by=list(datwp_p$Species, datwp_p$Site, datwp_p$DOY),
                     FUN= "mean")
datwp_sd<- aggregate(datwp_p$wp,by=list(datwp_p$Species, datwp_p$Site, datwp_p$DOY),
                     FUN= "sd")
datwp_l<- aggregate(datwp_p$wp,by=list(datwp_p$Species, datwp_p$Site, datwp_p$DOY),
                     FUN= "length")
datwp_p1$SE<- datwp_sd$x/sqrt(datwp_l$x)

colnames(datwp_p1)<- c("species", "site", "DOY", "wp", "SE")

colsite<- ifelse(datwp_p1$site=="hd", 19, 17)

species<- ifelse(datwp_p1$species == "larix", "green", 
                 ifelse(datwp_p1$species == "betula", "red", "blue"))
plot(datwp_p1$DOY, datwp_p1$wp, col=species, pch= colsite, xlab= "Day of Year", ylab=
       "Water Potential (-MPa)", ylim= c(0, 1.5), family= "serif")

legend(170, 1.6, c("larix", "betula", "salix", "high density", "low density"),
       pch= c(19, 19, 19, 19, 17), col= c("green", "red", "blue", "black", "black"), bty= "n")
arrows(datwp_p1$DOY, datwp_p1$wp-datwp_p1$SE, datwp_p1$DOY, datwp_p1$wp+datwp_p1$SE, 
       code=0, col= rgb(0/255, 0/255, 0/255, 0.4))

datPj<-datwp_p
colnames(datPj)<- tolower(colnames(datPj))
datPj$hour<- round_any(datPj$time_d, 0.5)

datAll<- join(datPj, datMet, by= c("doy", "year", "hour", "site"), type="left")

plot(datAll$D, datAll$wp)
datX<- aggregate(datAll$wp, by = list(datAll$doy, datAll$site, datAll$species), FUN= "mean")
colnames(datX)<- c("doy", "site", "species", "wp")
datD<- aggregate(datAll$D, by = list(datAll$doy, datAll$site, datAll$species), FUN= "mean")
colnames(datD)<- c("doy", "site", "species", "d")

datDX<- join(datX, datD, by=c("doy", "site", "species"), type= "inner")

#wp vs. vpd plot larix

plot(datDX$d[datDX$site=="hd" & datDX$species=="larix"], datDX$wp[datDX$site=="hd"  &
      datDX$species=="larix"], pch=19, col= "green", ylim=c(0.25,1.25), xlim= c(0,1.5), xlab= "Vapor Pressure Deficit",
     ylab = "Water Potential (-MPa)", family= "serif")
points(datDX$d[datDX$site=="ld"  & datDX$species=="larix"], datDX$wp[datDX$site=="ld"
       & datDX$species=="larix"], pch=19, col= "blue")

#legend(0, 1.195, c( "high density", "low density"),
 #      pch= c(19, 19), col= c("green", "blue", bty= "n"))

#regression of all larix, no site differentiation
fit_l<- lm(datDX$wp[datDX$species== "larix"]~ datDX$d[datDX$species== "larix"])
summary(fit_l)

abline(fit_l, lwd=1, col = "gray")

#regression by site
fit_hl<- lm(datDX$wp[datDX$site=="hd"  & datDX$species=="larix"]~ 
              datDX$d[datDX$site=="hd"  & datDX$species=="larix"])
summary(fit_hl)

fit_ll<- lm(datDX$wp[datDX$site=="ld"  & datDX$species=="larix"]~ 
              datDX$d[datDX$site=="ld"  & datDX$species=="larix"])
summary(fit_ll)

abline(fit_ll, lwd= 1, col= "blue")


#wp vs vpd salix

##only one salix hd value

plot(datDX$d[datDX$site=="hd" & datDX$species=="salix"], datDX$wp[datDX$site=="hd"  &
          datDX$species=="salix"], pch=19, col= "green", ylim=c(0.25,1.25), xlim= c(0,1.5), xlab= "Vapor Pressure Deficit",
     ylab = "Water Potential (-MPa)", family= "serif")
points(datDX$d[datDX$site=="ld"  & datDX$species=="salix"], datDX$wp[datDX$site=="ld"
       & datDX$species=="salix"], pch=19, col= "blue")
#regression of all larix, no site differentiation
fit_l<- lm(datDX$wp[datDX$species== "salix"]~ datDX$d[datDX$species== "salix"])
summary(fit_l)

abline(fit_l, lwd=1, col = "gray")

#legend(0, 1.195, c( "high density", "low density"),
 #      pch= c(19, 19), col= c("green", "blue", bty= "n"))

#individual salix regression
fit_hl<- lm(datDX$wp[datDX$site=="hd"  & datDX$species=="salix"]~ 
              datDX$d[datDX$site=="hd"  & datDX$species=="salix"])
summary(fit_hl)

fit_ll<- lm(datDX$wp[datDX$site=="ld"  & datDX$species=="salix"]~ 
              datDX$d[datDX$site=="ld"  & datDX$species=="salix"])
summary(fit_ll)

abline(fit_ll, lwd= 1, col= "blue")

### wp vs vpd betula

plot(datDX$d[datDX$site=="hd" & datDX$species=="betula"], datDX$wp[datDX$site=="hd"  &
      datDX$species=="betula"], pch=19, col= "green", ylim=c(0.25,1.25), xlim= c(0,1.5), xlab= "Vapor Pressure Deficit",
     ylab = "Water Potential (-MPa)", family= "serif")
points(datDX$d[datDX$site=="ld"  & datDX$species=="betula"], datDX$wp[datDX$site=="ld"
                 & datDX$species=="betula"], pch=19, col= "blue")

legend(0, 1.195, c( "high density", "low density"),
       pch= c(19, 19), col= c("green", "blue", bty= "n"))

#hd linear regression
fit_hl<- lm(datDX$wp[datDX$site=="hd"  & datDX$species=="betula"]~ 
              datDX$d[datDX$site=="hd"  & datDX$species=="betula"])
summary(fit_hl)

#ld linear regression
fit_ll<- lm(datDX$wp[datDX$site=="ld"  & datDX$species=="betula"]~ 
              datDX$d[datDX$site=="ld"  & datDX$species=="betula"])
summary(fit_ll)

abline(fit_ll, lwd= 1, col= "blue")

#regression of all betula, no site differentiation
fit_b<- lm(datDX$wp[datDX$species== "betula"]~ datDX$d[datDX$species== "betula"])
summary(fit_b)

coef(fit_b)


abline(fit_b, lwd=1, col = "gray")
