### Rhododendron siste species box plots
### Figure 2: Range plot selected Environmental Variable 
### Boxplot by species 
tiff(filename="D:/Rhododendron Git/sign different plot.tif", width = 11500,res=500, height=8000,  compression = "lzw" )
par(mfrow=c(3,4), cex=1.5, tck= -0.02)

par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5, .5,0) )
boxplot(pre.df$bio09/10~ pre.df$species, names= F, ylab="Bio09:Mean Temperature of Driest Quarter")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio12~ pre.df$species,names= F, ylab="Bio012: Annual Precipitation")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio13~ pre.df$species, names= F, ylab="Bio13: Precipitation of Wettest Month")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio14~ pre.df$species, names= F, ylab="Bio14: Precipitation of Driest Month")

par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$bio15~ pre.df$species, names= F, ylab="Bio15: Precipitation Seasonality")
par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio16~ pre.df$species, names= F, ylab="Bio16: Precipitation of Wettest Quarter")
par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio17~ pre.df$species, names= F,  ylab="Bio17: Precipitation of Driest Quarter")
par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio18~ pre.df$species, ylab="Bio18: Precipitation of Warmest Quarter")


par(mai=c(0.8,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio19~ pre.df$species, ylab="Bio19: Precipitation of Coldest Quarter")
par(mai=c(0.8,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$ai~ pre.df$species, ylab="Aridity index")
par(mai=c(0.8,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$eq~ pre.df$species, ylab="Ellenberg quotient")

dev.off()




### Supplimentary Range plot for all variables 
### Boxplots

tiff(filename="D:/Rhododendron Git/Temperature bioclims.tif", width = 8000,res=500, height=11000,  compression = "lzw" )
par(mfrow=c(4,3), cex=1.3, tck= -0.02)

par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5, .5,0) )
boxplot(pre.df$bio01/10~ pre.df$species, names= F, ylab="Bio01: Annual Mean Temperature")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5, .5,0) )
boxplot(pre.df$bio02/10~ pre.df$species, names= F, ylab="Bio02: Mean Diurnal Range ")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio03/10~ pre.df$species,names= F, ylab="Bio03: Isothermality")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio04/10~ pre.df$species,names= F, ylab="Bio04: Temperature Seasonality ")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5, .5,0) )
boxplot(pre.df$bio05/10~ pre.df$species, names= F, ylab="Bio05: Max Temperature of Warmest Month")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5, .5,0) )
boxplot(pre.df$bio06/10~ pre.df$species, names= F, ylab="Bio06: Min Temperature of Coldest Month")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio07/10~ pre.df$species, names= F, ylab="Bio07: Temperature Annual Range")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio08/10~ pre.df$species, names= F, ylab="Bio08: Mean Temperature of Wettest Quarter")
par(mai=c(0.2,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio09/10~ pre.df$species,  ylab="Bio09: Mean Temperature of Driest Quarter")
par(mai=c(0.5,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio10/10~ pre.df$species, ylab="Bio10: Mean Temperature of Warmest Quarter")
par(mai=c(0.5,0.8 ,0.2, 0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio11/10~ pre.df$species,  ylab="Bio11: Mean Temperature of Coldest Quarter")

dev.off()


### Supplimentary Precipitation
tiff(filename="D:/Rhododendron Git/Precipitation bioclims.tif", width = 8000,res=500, height=8250,  compression = "lzw" )
par(mfrow=c(3,3), cex=1.3, tck= -0.02)

par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$bio12~ pre.df$species,names= F,  ylab="Bio12: Annual Precipitation")
par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$bio13~ pre.df$species, names= F, ylab="Bio13: Precipitation of Wettest Month")
par(mai=c(0.2,0.8 ,0.1,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio14~ pre.df$species,names= F,  ylab="Bio14: Precipitation of Driest Month")
par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio15~ pre.df$species, names= F, ylab="Bio15: Precipitation Seasonality")
par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio16~ pre.df$species, names= F, ylab="Bio16: Precipitation of Wettest Quarter")
par(mai=c(0.2,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio17~ pre.df$species, ylab="Bio17: Precipitation of Driest Quarter")
par(mai=c(0.6,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio18~ pre.df$species, ylab="Bio18: Precipitation of Warmest Quarter")
par(mai=c(0.6,0.8 ,0,0.1), mgp=c(1.5,.5,0))
boxplot(pre.df$bio19~ pre.df$species, ylab="Bio19: Precipitation of Coldest Quarter")

dev.off()

### Supplimentary Precipitation
tiff(filename="D:/Rhododendron Git/Other variables.tif", width = 8000,res=500, height=8250,  compression = "lzw" )
par(mfrow=c(3,3), cex=1.3, tck= -0.02)

par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$slope~ pre.df$species,names= F,  ylab="Slope")
par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$aspect~ pre.df$species,names= F,  ylab="Aspect")
par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$rri~ pre.df$species,names= F,  ylab="Relative Radiation Index")
par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$abt~ pre.df$species,names= F,  ylab="Annual BioTemperature")
par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$ai~ pre.df$species,names= F,  ylab="Aridity Index")
par(mai=c(.2,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$wi~ pre.df$species,  ylab="Warmth Index")
par(mai=c(.6,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$ci/10~ pre.df$species,  ylab="Coldness Index")
par(mai=c(.6,0.8 ,0.1,0.1), mgp=c(1.5, .5 ,0))
boxplot(pre.df$eq~ pre.df$species,  ylab="Ellenberg quotient")

dev.off()







myY<-pre.df[,4:10]

bpb12<- ggplot(pre.df, aes(x=species, y=bio12)) +
  geom_boxplot()+
  labs(x="Species", y="Bio12: \n Annual Precipitation")

bpb14<- ggplot(pre.df, aes(x=species, y=bio14)) +
  geom_boxplot()+
  labs(x="Species", y="Bio14: \n Precipitation of Driest Month")

bpb15<- ggplot(pre.df, aes(x=species, y=bio15)) +
  geom_boxplot()+
  labs(x="Species", y="Bio15: \n Precipitation Seasonality")

bpb17<- ggplot(pre.df, aes(x=species, y=bio17)) +
  geom_boxplot()+
  labs(x="Species", y="Bio17: \n Precipitation of Driest Quarter")
