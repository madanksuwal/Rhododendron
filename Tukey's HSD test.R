### Post Hoc test
### Tukey's HSD test

pre.df<- read.csv("D:/Rhododendron Git/Rhododendron/Data/Rhodo 3 spp presence points2.csv")
head(pre.df)
names(pre.df)
"rri"     "slope"   "elev"    "aspect"  "abt"    
[28] "ai"      "wi"      "ci"      "eq"   

fit.aov <-aov (pre.df$eq~ pre.df$species)
posthoc<- TukeyHSD(fit.aov)
P1<- (round ((posthoc$`pre.df$species`[1,4]),3))
P2<- (round ((posthoc$`pre.df$species`[2,4]),3))
P3<- (round ((posthoc$`pre.df$species`[2,4]),3))

V<- "eq"
df<-cbind( V, P1, P2, P3)
b.df<-rbind(b.df, df)
b.df

dim(b.df)
c.df<- cbind ( b.df[,1],transform(b.df[,2:4] <=0.05))
c.df
posthoc; plot(posthoc)

a.df<-data.frame()

## boxplot
