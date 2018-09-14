##########################################################
####### Code for Pinnacles Biodiversity Paper ############
################# J. Meiners 2017 ########################

setwd("~/Dropbox/ResearchProjects/Biodiv Paper/Pinnacles_Bee_Biodiversity")
library(dplyr)
library(plyr)
library(RColorBrewer)
library(tidyverse)
library(reshape2)
library(ggplot2)

## Figure 1: Map made in ArcGIS using the file "loc_days_species.csv"

## Tables 1 & 2: Summary statistics of species and specimen numbers per year of study were calculated in excel using the file "Species_list.csv" (shared in Supplementary material)

## Figure 2a: Barplot comparisons across years
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
spyears <- dcast(fam_years, Family~Num_years, value.var="count")
spyears = spyears %>% remove_rownames %>% column_to_rownames(var="Family")
spyears[is.na(spyears)] <- 0

# par(mfrow=c(2,1))
# quartz(width = 4.5, height = 6)
quartz(width = 9, height = 5)
# par(mar=c(5,4.5,5.5,4)) 
###### Side by side barplot #######
tiff(filename = "Biodiv_Fig2a.tiff", units = "in", compression = "lzw", res = 300, width = 9, height = 5)
spyears = as.matrix(spyears)
names_leg=c("Andrenidae (N = 97)", "Apidae (N = 122)" , "Colletidae (N = 21)", "Halictidae (N = 60)", "Megachilidae (N = 148)", "Melittidae (N = 2)")
barplot(spyears, beside = TRUE, col=brewer.pal(n = 6, name = "Set3"))
mtext(side = 2, line = 2.75, text = "Number of species", font = 2)
mtext(side = 1, line = 3.75, text = "Number of years a species was present", font = 2)
legend("top", names_leg, pch=15, cex = 0.9, col=brewer.pal(n = 6, name = "Set3"), bty="n", title = "Bee Families")
dev.off() # run this line after figure code to finish saving out figure to file


## Figure 2b: Family accumulation over years
Prop_specyear <- read.csv("SpeciesinFamily_450.csv", header = TRUE, row.names = 1)
years = c("All96", "New97", "New98", "New99", "New02", "New11", "New12")

prop_years = Prop_specyear[,years]/rowSums(Prop_specyear[,years])
totals = rowSums(Prop_specyear[,years])

quartz(width = 9, height = 5)
# par(mar=c(5,4.5,5.5,4)) 
tiff(filename = "Biodiv_Fig2b.tiff", units = "in", compression = "lzw", res = 300, width = 9, height = 5)
color.vec = brewer.pal(n = 7, name = "YlGnBu")
barplot(t(prop_years), las=1, col=color.vec,
             names=c("Andrenidae \n (N = 97)", "Apidae \n (N = 122)" , "Colletidae \n (N = 21)", "Halictidae \n (N = 60)", "Megachilidae \n (N = 148)", "Melittidae \n (N = 2)"))
mtext(side = 2, line = 2.75, text = "Proportion of total species collected", font = 2)
mtext(side = 1, line = 3.75, text = "Bee Family \n (N = total species collected)", font = 2)

years_leg = c("1996", "1997", "1998", "1999", "2002", "2011", "2012")
legend(5, 1.25, legend=years_leg[6:7], bty = "n",  xpd=NA, ncol=1, pch=22, pt.bg=color.vec[6:7], pt.cex=2.5, inset=c(-0.15) ,text.font = 1, title = "Current Collection Years")
legend(3.5,1.25, legend=years_leg[5], bty = "n", xpd=NA, ncol=1, pch=22, pt.bg=color.vec[5], pt.cex=2.5, inset=c(-0.15) ,text.font = 1, title = "Bowl Study")
legend(0.3,1.25, legend=years_leg[1:4], bty = "n", xpd=NA, ncol=2, pch=22, pt.bg=color.vec[1:4], pt.cex=2.5, inset=c(-0.15) ,text.font = 1, title = "Early Museum Collection Years")
dev.off() # run this line after figure code to finish saving out figure to file


## Figure 3: Habitat type rarefaction curve
samples = read.csv("samples.csv", header = TRUE, row.names = 1)
## Species accumulation curve
quartz(height = 6, width = 10)
## observed curve
accumcurve = specaccum(samples, method = "rarefaction")
plot(accumcurve, ci.type="poly", col = "black", lwd=2, ci.lty=0, ci.col="grey", ylab = "Number of species", xlab = "Number of samples")
## expected curve for comparison
accumcurve.random = specaccum(samples, "random")
plot(accumcurve.random, col="blue", add=TRUE)

## run to save figure as tiff file
tiff(filename = "Biodiv_Fig3.tiff", units = "in", compression = "lzw", res = 300, width = 10, height = 6)
# (run code script here)
dev.off() # then run this line after figure code to finish saving out figure to file

## Table 3: full data for these rank abundance summaries is shared in Supplementary material


## Figure 4: Map made in ARCGis using data from file "Study_comparison.csv"


#### Figure 5: Study comparison (Data from Table S1)

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


