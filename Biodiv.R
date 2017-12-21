##########################################################
####### Code for Pinnacles Biodiversity Paper ############
################# J. Meiners 2017 ########################

setwd("~/Dropbox/Data")
library(dplyr)
library(plyr)

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

setwd("~/Dropbox/Biodiv Paper/Pinnacles_Bee_Biodiversity")
## Figure 2: Barplot comparisons across years
####### Fig 2a
## Calculate how many years each species was collected
Species_years = read.csv("Species_list.csv", header = TRUE)
Species_years = subset(Species_years, Num_years != "NA", select = c("Family", "Species", "Num_years"))
## Group species by family and number of years recorded
fam_years = Species_years %>%
  dplyr::group_by(Family, Num_years) %>%
  dplyr::summarise(count = n())
## Calculate number of species per family
fam_num = Species_years %>%
  dplyr::group_by(Family) %>%
  dplyr::summarise(species_per_family = n())
## Coerce into crostab
library(reshape2)
spyears <- dcast(fam_years, Family~Num_years, value.var="count")
library(tidyverse)
spyears = spyears %>% remove_rownames %>% column_to_rownames(var="Family")
spyears[is.na(spyears)] <- 0

quartz(width = 9, height = 6)
par(mar=c(5,4.5,5.5,4)) 
###### Side by side barplot #######
library(RColorBrewer)
spyears = as.matrix(spyears)
names_leg=c("Andrenidae (N = 97)", "Apidae (N = 122)" , "Colletidae (N = 21)", "Halictidae (N = 60)", "Megachilidae (N = 148)", "Melittidae (N = 2)")
barplot(spyears, beside = TRUE, col=brewer.pal(n = 6, name = "Set3"))
mtext(side = 2, line = 2.75, text = "Number of species", font = 2)
mtext(side = 1, line = 3.75, text = "Number of years a species was present", font = 2)
legend("top", names_leg, pch=15, col=brewer.pal(n = 6, name = "Set3"), bty="n", title = "Bee Families")

######## Fig 2b
Prop_specyear <- read.csv("SpeciesinFamily.csv", header = TRUE, row.names = 1)
years = c("All96", "New97", "New98", "New99", "New02", "New11", "New12")

prop_years = Prop_specyear[,years]/rowSums(Prop_specyear[,years])
totals = rowSums(Prop_specyear[,years])

quartz(width = 9, height = 6)
par(mar=c(5,4.5,5.5,4)) 
color.vec = brewer.pal(n = 7, name = "YlGnBu")
barplot(t(prop_years), las=1, col=color.vec,
             names=c("Andrenidae \n (N = 107)", "Apidae \n (N = 121)" , "Colletidae \n (N = 22)", "Halictidae \n (N = 76)", "Megachilidae \n (N = 151)", "Melittidae \n (N = 2)"))
mtext(side = 2, line = 2.75, text = "Proportion of total species collected", font = 2)
mtext(side = 1, line = 3.75, text = "Bee Family \n (N = total species collected)", font = 2)

years_leg = c("1996", "1997", "1998", "1999", "2002", "2011", "2012")
legend(5, 1.25, legend=years_leg[6:7], bty = "n",  xpd=NA, ncol=1, pch=22, pt.bg=color.vec[6:7], pt.cex=2.5, inset=c(-0.15) ,text.font = 1, title = "Current Collection Years")
legend(3.5,1.25, legend=years_leg[5], bty = "n", xpd=NA, ncol=1, pch=22, pt.bg=color.vec[5], pt.cex=2.5, inset=c(-0.15) ,text.font = 1, title = "Bowl Study")
legend(0.3,1.25, legend=years_leg[1:4], bty = "n", xpd=NA, ncol=2, pch=22, pt.bg=color.vec[1:4], pt.cex=2.5, inset=c(-0.15) ,text.font = 1, title = "Early Museum Collection Years")


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



#### Figure 6: Study comparison (Data from Table S1) ## Clean this up to only what is necessary for fig.

surveys = read.csv("Study_comparison.csv", header = TRUE) # may need to add 'stringsAsFactors = FALSE' if not importing correctly
names(surveys)
dim(surveys)

pinn_col = rep("black", times=length(surveys[,1]))
pinn_col[which(surveys$Study=="Pinnacles National Park, CA")]="red"

plot(surveys$Species ~ surveys$Area, las=1, pch=19, col = pinn_col)
lin_mod = lm(surveys$Species ~ surveys$Area)
abline(lin_mod)
text("topright", paste("R2=",round(summary(lin_mod)$ r.squared, digits=3), sep=""))
text(topright, paste("p-value=",round(summary(lin_mod)$ coefficients[[2,4]], digits=3), sep=""))
mtext("Linear model", line = 1, font=2)

plot(bee$Species ~ log(bee$Area), las=1, pch=19, col = pinn_col)
log_mod = lm(bee$Species ~ log(bee$Area))
abline(log_mod)
text(x=2.5, y=600, paste("R2=",round(summary(log_mod)$ r.squared, digits=3), sep=""))
text(x=2.5, y=550, paste("p-value=",round(summary(log_mod)$ coefficients[[2,4]], digits=3), sep=""))
mtext("Logarithmic model", line = 1, font=2)

plot(log(surveys$Species) ~ log(surveys$Area), las=1, pch=19, col = pinn_col)
power_mod = lm(log(surveys$Species) ~ log(surveys$Area))
abline(power_mod)
text(x=2.5, y=6.25, paste("R2=",round(summary(power_mod)$ r.squared, digits=3), sep=""))
text(x=2.5, y=6.1, paste("p-value=",round(summary(power_mod)$ coefficients[[2,4]], digits=3), sep=""))
mtext("power-law model", line = 1, font=2)

##Look for largest residual
plot(residuals(power_mod) ~ log(surveys$Area), las=1, pch=19, ylim=c(-max(abs(residuals(power_mod))),  max(abs(residuals(power_mod))) ), col = pinn_col)
abline(h=0, col="gray")
abline(h = c(-residuals(power_mod)[which(surveys$Study=="Pinnacles National Park, CA")], residuals(power_mod)[which(surveys$Study=="Pinnacles National Park, CA")]), lty=2, col="darkred")

##Convert to a percentage of predicted. 
percent_deviation_A = (surveys$Species - exp(predict(power_mod)))/exp(predict(power_mod))*100
sort_order = rev(order(percent_deviation_A))
percent_deviation = percent_deviation_A[sort_order]
bar_col = rep("gray20", times = length(surveys[,1]))
bar_col[which(percent_deviation>0)]="gray80"

beta = power_mod$coefficients
area_range = seq(from = min(surveys$Area), to = max(surveys$Area), length.out=300)
pred_species = exp(beta[1]+beta[2]*log(area_range))

quartz(width=11, height=6)
par(mfrow=c(1,2))
par(mar=c(5.5, 5, 2, 2)+0.1)
surveys_col = rep("gray20", times = length(surveys[,1]))
surveys_col[which(percent_deviation_A>0)]="gray80"
plot(surveys$Species ~ surveys$Area, las=1, pch=21, bg = surveys_col, ylab="Number of species", xlab="Area (sq. km.)")
points(x = area_range, y = pred_species, type='l', lwd=2)
points(x = area_range[which(surveys$Study=="Pinnacles National Park, CA")],
       y = pred_species[which(surveys$Study=="Pinnacles National Park, CA")],
       pch=21, cex=3)

par(mar=c(15, 6, 2, 2)+0.1)
barplot(height = percent_deviation, las=2, names.arg = surveys$Study[sort_order], yaxs = 'r', ylab = "Observed relative to predicted\nspecies per area (%)", col = bar_col)
abline(h=0)
box(which="plot")
legend("topright", legend = c("> predicted", "< predicted"), pch=22, pt.bg = c("gray80", "gray20"), pt.cex=2, inset=0.05, bty='n')
axis(side = 1, at = seq(from = 0.75, by = 1.2, length.out=length(surveys[,1])), labels=FALSE)

par(mfrow=c(1,2))
par(mar=c(5.5, 5, 2, 2)+0.1)
surveys_col = rep("gray20", times = length(surveys[,1]))
surveys_col[which(percent_deviation_A>0)]="gray80"
plot(log(surveys$Species) ~ log(surveys$Area), las=1, pch=21, bg = surveys_col, ylab="log_e(Number of species)", xlab="log_e(Area (sq. km.))")
points(x = log(surveys$Area), y = predict(power_mod), type='l', lwd=2)
points(x = log(surveys$Area)[which(surveys$Study=="Pinnacles National Park, CA")],
       y = log(surveys$Species)[which(surveys$Study=="Pinnacles National Park, CA")],
       pch=21, cex=3)

par(mar=c(15, 6, 2, 2)+0.1)
barplot(height = percent_deviation, las=2, names.arg = surveys$Study[sort_order], yaxs = 'r', ylab = "Observed relative to predicted\nspecies per area (%)", col = bar_col)
abline(h=0)
box(which="plot")
legend("topright", legend = c("> predicted", "< predicted"), pch=22, pt.bg = c("gray80", "gray20"), pt.cex=2, inset=0.05, bty='n')
axis(side = 1, at = seq(from = 0.75, by = 1.2, length.out=length(surveys[,1])), labels=FALSE)


