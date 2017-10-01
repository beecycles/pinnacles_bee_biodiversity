#########################################################
####### Code for Pinnacles Biodiversity Paper ###########
################# J. Meiners 2017 #######################

setwd("~/Dropbox/Biodiv Paper/Data")

# load data
Prj12 = read.csv("Project12.csv", header = TRUE)
dim(Prj12)
head(Prj12)

Prj56 = read.csv("Project56.csv", header = TRUE)
dim(Prj56)
head(Prj56)

Prj57 = read.csv("Project57.csv", header = TRUE)
dim(Prj57)
head(Prj57)
names(Prj57)

# subset by year
unique(Prj12$Yr0)
Y88 = subset(Prj12, Yr0 == 1988 & Bee == 1)
Y96 = subset(Prj12, Yr0 == 1996 & Bee == 1)
Y97 = subset(Prj12, Yr0 == 1997 & Bee == 1)
Y98 = subset(Prj12, Yr0 == 1998 & Bee == 1)
Y99 = subset(Prj12, Yr0 == 1999 & Bee == 1)

unique(Prj56$Yr0)
Y02 = subset(Prj56, Yr0 == 2002 & Bee == 1)

unique(Prj57$Yr0)
Y11 = subset(Prj57, Yr0 == 2011 & Bee == 1)
Y12 = subset(Prj57, Yr0 == 2012 & Bee == 1)

# Number of collecting days
unique(Y88$JD_date) # 1
unique(Y96$JD_date) # 5
unique(Y97$JD_date) # 50
unique(Y98$JD_date) # 56
unique(Y99$JD_date) # 14
unique(Y02$JD_date) # 10
unique(Y11$JD_date) # 58
unique(Y12$JD_date) # 53

# Number of species
unique(Y88$SpID) # 1
unique(Y96$SpID) # 174
unique(Y97$SpID) # 304
unique(Y98$SpID) # 321
unique(Y99$SpID) # 222
unique(Y02$SpID) # 155
unique(Y11$SpID) # 298
unique(Y12$SpID) # 309

# Specimen totals
sum(Y88$larger) # 1
sum(Y96$larger) # 1362
sum(Y97$larger) # 8077
sum(Y98$larger) # 9382
sum(Y99$larger) # 8234
sum(Y02$larger) # 7255
sum(Y11$larger) # 20351
sum(Y12$larger) # 32438

sum(sum(Y88$M) + sum(Y88$F)) # 1
sum(sum(Y96$M) + sum(Y96$F)) # 1332
sum(sum(Y97$M) + sum(Y97$F)) # 7931
sum(sum(Y98$M) + sum(Y98$F)) # 8899
sum(sum(Y99$M) + sum(Y99$F)) # 7082
sum(sum(Y02$M) + sum(Y02$F)) # 6534
sum(sum(Y11$M) + sum(Y11$F)) # 18536
sum(sum(Y12$M) + sum(Y12$F)) # 27719

## Figure 4: Dominance Graphs
library(ggplot2)

dominance = read.csv("Dominance_Genera.csv", header = TRUE, row.names = 1)

10^(seq(from=-1, to=2, by=1))

10^0.5


dominance=dominance[order(dominance$Sum),]
j = 0.1
toggle=c(0, j, -2*j, -j, 0, j, 2*j, -j, 1.5*j, -j, j, 0, -j, 0.5*j, 0, -1.2*j,
         -0.5*j, -j, 0, j, 0, 1.2*j, 0.5*j, 2*j, 0, j, -j, -0.5*j, -j, -j, 0, j,
         -0.2*j, 0, 0, 0, j, 0, -0.2*j, 0, -0.2*j, 0, 0, 0, 0, 0, 0, 0)
#jitter = c(toggle, rep(0, times = c(48-length(toggle))))

quartz(width = 11, height = 7)
#plot.new()
par(mar=c(5,5,2,2)+0.1)
plot(log10(dominance$Sum), log10(dominance$StDv), xlab = expression(Total~Abundance~(log[10]~scaled)), ylab = expression(Standard~Deviation~of~Sample~Abundances~(log[10]~scaled)), pch=19, col=gray(level=0.20, alpha=0.5), axes=F, xlim=c(0,4.25), cex=0.7)
axis(side = 2, at=seq(from=-1, to=2, by=1), labels = 10^(seq(from=-1, to=2, by=1)))
axis(side = 1, at=seq(from=0, to=4, by=1), labels = 10^(seq(from=0, to=4, by=1)))
text(log10(dominance$Sum), c(log10(dominance$StDv)+toggle), row.names(dominance), cex=0.75, pos=4, col="black")
box(which="plot")



#### Figure 6: Study comparison (Data from Table S1)

# load data
compare = read.csv("Study_comparison.csv", header = TRUE)
dim(compare)
head(compare)
compare$Area = as.integer(compare$Area)

library(ggplot2)

quartz(width=10, height=6)
ggplot(compare, aes(Genera, Species)) +
  geom_point(aes(size=Area)) +
  geom_text(aes(label=Study),hjust=0, vjust=0)

quartz(width=6, height=6)
ggplot(compare, aes(Area, Species)) +
  geom_point(size=0.9) +
  geom_text(aes(label=Study), size=3, hjust=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0), vjust=c(0,0,0,0,0,1,0,0,-1,1,0,1,0,1,0,0,0,0)) +
  xlim(-100, 8000)

quartz(width=6, height=6)
ggplot(compare, aes(log10(Area), log10(Species))) +
  geom_point(size=0.9) +
  geom_text(aes(label=Study), size=3, hjust=0, vjust=c(0,0,0,0,0,1,0,0,-1,1,0,1,0,1,0,0,0,0)) +
  xlim(0.5, 4.8) +
  stat_smooth()

tiff(filename = "SpeciesArea.tiff" , units = "in", compression = "lzw", res = 150, width = 6, height = 6)
dev.off()

