setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioThesis\\Data")

getwd()

datSLA<- read.csv("SLA_2017.csv")

#####Means#####

sp.mean<- aggregate(datSLA$SLA, by=list(datSLA$Site, datSLA$Species), FUN= "mean")
colnames(sp.mean)<- c("Site", "Species", "SLA")

sp.sd<- aggregate(datSLA$SLA, by=list(datSLA$Site, datSLA$Species), FUN= "sd")
colnames(sp.sd)<- c("Site", "Species", "sd")

sp.l<- aggregate(datSLA$SLA, by=list(datSLA$Site, datSLA$Species), FUN= "length")
colnames(sp.l)<- c("Site", "Species", "Length")

sp.mean$SE<- sp.sd$sd/sqrt(sp.l$Length)

t.test()

####Plotting###
  
#attempt 1
plot(sp.mean$Species, sp.mean$SLA, xlab= "Species", ylab= "SLA")
#attempt 2
barplot(sp.mean$SLA, xlab=species, col=c("darkblue", "green")
        
install.packages("ggplot2")
gplot(c(dat.mean$Site, dat.mean$Species), dat.mean$SLA)
        
#Trying to copy old stuffers
x.c<-c(1,2,4,5, 7, 8)
colors<-c("royalblue3", "darkseagreen", "royalblue3", "darkseagreen", "royalblue3", "darkseagreen")

par(mai=c(1.5,1.5,1.5,1.5))
plot(c(0,1), c(0,1), type="n", xlim=c(0,8), ylim=c(80,180), axes=FALSE, xlab=" ", cex.lab=
       1.25,
     ylab= expression(paste("Specific Leaf Area (cm"^"2","g"^"-1",")")), xaxs="i", yaxs="i")

for(i in 1:dim(sp.mean)[1]){
  polygon(c(x.c[i]-1, x.c[i]-1, x.c[i], x.c[i]),
          c(80, sp.mean$SLA[i],sp.mean$SLA[i], 80 ),col= colors[i])
}

arrows(x.c-0.5, sp.mean$SLA- sp.mean$SE, x.c-0.5, sp.mean$SLA+ sp.mean$SE, code=0)


axis(2, seq(80,160,by=20), las=2)
axis(1, x.c-0.5, c("low", "high", "low", "high"))

mtext("Betula", side=1, line=2, at=1, cex=1.25)
mtext("Larix", side=1, line=2, at=4, cex=1.25)
mtext("Salix", side=1, line=2, at=7, cex=1.25)

####T TESTS ####
t.test(datSLA$SLA[datSLA$Species== "Betula" & datSLA$Site== "High Density"],
       datSLA$SLA[datSLA$Species=="Betula" & datSLA$Site== "Low Density"])






