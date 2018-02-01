setwd("C:\\Users\\Sabrina\\Google Drive\\Colgate\\Senior Year\\BioThesis\\Data")

getwd()

dat.nut<-read.csv("nutrients_consolidated.csv")

###nitrogen###

n.mean<- aggregate(dat.nut$N, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean")
colnames(n.mean)<- c("Site", "Species", "N")

n.sd<- aggregate(dat.nut$N, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd")
colnames(n.sd)<- c("Site", "Species", "sd")

n.l<- aggregate(dat.nut$N, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "length")
colnames(n.l)<- c("Site", "Species", "Length")

n.mean$SE<- n.sd$sd/sqrt(n.l$Length)

t.test(dat.nut$N[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$N[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$N[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$N[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$N[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$N[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])

####carbon###
C.mean<- aggregate(dat.nut$C, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean",
                   na.rm= TRUE)
colnames(C.mean)<- c("Site", "Species", "N")

C.sd<- aggregate(dat.nut$C, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd",
                 na.rm= TRUE)
colnames(C.sd)<- c("Site", "Species", "sd")

C.l<- aggregate(dat.nut$C[!is.na(dat.nut$C)], by=list(dat.nut$Treatment[!is.na(dat.nut$C)],
                        dat.nut$Species[!is.na(dat.nut$C)]), FUN= "length")

colnames(C.l)<- c("Site", "Species", "Length")

C.mean$SE<- C.sd$sd/sqrt(C.l$Length)

t.test(dat.nut$C[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$C[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$C[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$C[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$C[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$C[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])

###C:N####
cn.mean<- aggregate(dat.nut$C_N, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean",
                    na.rm=TRUE)
colnames(cn.mean)<- c("Site", "Species", "CN")

cn.sd<- aggregate(dat.nut$C_N, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd",
                  na.rm=TRUE)
colnames(cn.sd)<- c("Site", "Species", "sd")

cn.l<- aggregate(dat.nut$C_N[!is.na(dat.nut$C_N)],by=list(dat.nut$Treatment[!is.na(dat.nut$C_N)], 
      dat.nut$Species[!is.na(dat.nut$C_N)]), FUN= "length")
colnames(cn.l)<- c("Site", "Species", "Length")

cn.mean$SE<- cn.sd$sd/sqrt(cn.l$Length)

t.test(dat.nut$C_N[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$C_N[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$C_N[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$C_N[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$C_N[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$C_N[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])

###Phosphorus###
p.mean<- aggregate(dat.nut$P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean", na.rm=TRUE)
colnames(p.mean)<- c("Site", "Species", "P")

p.sd<- aggregate(dat.nut$P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd", na.rm=TRUE)
colnames(p.sd)<- c("Site", "Species", "sd")

p.l<- aggregate(dat.nut$P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "length")
colnames(p.l)<- c("Site", "Species", "Length")

p.mean$SE<- p.sd$sd/sqrt(p.l$Length)

t.test(dat.nut$P[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$P[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$P[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$P[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$P[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$P[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])

### N:P ###
np.mean<- aggregate(dat.nut$N_P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean", na.rm=TRUE)
colnames(np.mean)<- c("Site", "Species", "NP")

np.sd<- aggregate(dat.nut$N_P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd", na.rm=TRUE)
colnames(np.sd)<- c("Site", "Species", "sd")

np.l<- aggregate(dat.nut$N_P[!is.na(dat.nut$N_P)],by=list(dat.nut$Treatment[!is.na(dat.nut$N_P)],
        dat.nut$Species[!is.na(dat.nut$N_P)]), FUN= "length")
colnames(np.l)<- c("Site", "Species", "Length")

np.mean$SE<- np.sd$sd/sqrt(np.l$Length)

t.test(dat.nut$N_P[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$N_P[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$N_P[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$N_P[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$N_P[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$N_P[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])


###C:P ###

cp.mean<- aggregate(dat.nut$C_P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean"
                    , na.rm= TRUE)
colnames(cp.mean)<- c("Site", "Species", "CP")

cp.sd<- aggregate(dat.nut$C_P, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd",
                  na.rm= TRUE)
colnames(cp.sd)<- c("Site", "Species", "sd")

cp.l<- aggregate(dat.nut$C_P[!is.na(dat.nut$C_P)], by=list(dat.nut$Treatment[!is.na(dat.nut$C_P)],
         dat.nut$Species[!is.na(dat.nut$C_P)]), FUN= "length")
colnames(cp.l)<- c("Site", "Species", "Length")

cp.mean$SE<- cp.sd$sd/sqrt(cp.l$Length)

t.test(dat.nut$C_P[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$C_P[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$C_P[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$C_P[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$C_P[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$C_P[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])

###LMA###

lma.mean<- aggregate(dat.nut$LMA, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "mean"
)
colnames(lma.mean)<- c("Site", "Species", "LMA")

lma.sd<- aggregate(dat.nut$LMA, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "sd"
                  )
colnames(lma.sd)<- c("Site", "Species", "sd")

lma.l<- aggregate(dat.nut$LMA, by=list(dat.nut$Treatment, dat.nut$Species), FUN= "length"
                 )
colnames(lma.l)<- c("Site", "Species", "Length")

lma.mean$SE<- lma.sd$sd/sqrt(lma.l$Length)

t.sd<- sqrt((((lma.l$Length[lma.mean$Species== "Betula" & lma.mean$Site== "H"]-1)
             *(lma.sd$sd[lma.mean$Species== "Betula" & lma.mean$Site== "H"]^2))+
              ((lma.l$Length[lma.mean$Species== "Betula" & lma.mean$Site== "L"]-1)
               *(lma.sd$sd[lma.mean$Species== "Betula" & lma.mean$Site== "L"]^2)))/
            (lma.l$Length[lma.mean$Species== "Betula" & lma.mean$Site== "H"]+
               lma.l$Length[lma.mean$Species== "Betula" & lma.mean$Site== "L"]-2))
            
t.check<- (lma.mean$LMA[lma.mean$Species== "Betula" & lma.mean$Site== "H"]-
  lma.mean$LMA[lma.mean$Species== "Betula" & lma.mean$Site== "L"])/
  (t.sd*sqrt((1/lma.l$Length[lma.mean$Species== "Betula" & lma.mean$Site== "H"])+
              (1/lma.l$Length[lma.mean$Species== "Betula" & lma.mean$Site== "L"])))
t.check



t.test(dat.nut$LMA[dat.nut$Species== "Betula" & dat.nut$Treatment== "H"], 
       dat.nut$LMA[dat.nut$Species== "Betula" & dat.nut$Treatment== "L"])

t.test(dat.nut$LMA[dat.nut$Species== "Salix" & dat.nut$Treatment== "H"], 
       dat.nut$LMA[dat.nut$Species== "Salix" & dat.nut$Treatment== "L"])

t.test(dat.nut$LMA[dat.nut$Species== "Larix" & dat.nut$Treatment== "H"], 
       dat.nut$LMA[dat.nut$Species== "Larix" & dat.nut$Treatment== "L"])

#stomata density