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
library(extrafont)
library(vegan)
# font_import() # this takes a while so only run it if making figures to export with Arial font
loadfonts() # Adds fonts for PDF graphics only
library(showtext)
## add the Arial font
font_add("Arial", regular = "arial.ttf",
         bold = "arialbd.ttf", italic = "ariali.ttf", bolditalic = "arialbi.ttf")

## Figure 1: Map made in ArcGIS using the file "loc_days_species.csv"

## Tables 1 & 2: Summary statistics of species and specimen numbers per year of study were calculated in excel using the file "Species_list.csv" (shared in Supplementary material)

## Figure 2a prep: Barplot comparisons across years
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
spyears = as.matrix(spyears)
names_leg=c("Andrenidae (N = 97)", "Apidae (N = 122)" , "Colletidae (N = 21)", "Halictidae (N = 60)", "Megachilidae (N = 148)", "Melittidae (N = 2)")

# Fig 2b prep
Prop_specyear <- read.csv("SpeciesinFamily_450.csv", header = TRUE, row.names = 1)
years = c("All96", "New97", "New98", "New99", "New02", "New11", "New12")
prop_years = Prop_specyear[,years]/rowSums(Prop_specyear[,years])
totals = rowSums(Prop_specyear[,years])
color.vec = brewer.pal(n = 7, name = "YlGnBu")

# Figure 2 plotting (a and b)

###### Side by side barplot #######
# bitmap("Fig2.tiff", height = 12, width = 17, units = "cm", type = "tifflzw", res = 300)
loadfonts(device = "postscript")
postscript("Fig2.eps", width = 12, height = 18, horizontal = FALSE, onefile = FALSE, paper = "special", colormodel = "cmyk", family = "Arial")
par(mfrow=c(2,1), omi=c(1, 0.8, 0.2, 0))

# Fig 2a: Species per year in each family
par(mai=c(1.8, 0.5, 0.5, 0.2))
barplot(spyears, beside = TRUE, col=brewer.pal(n = 6, name = "Set3"))
mtext(side = 2, line = 2.75, text = "Number of species", font = 2, cex = 2)
mtext(side = 1, line = 3.75, text = "Number of years a species was present", font = 2, cex = 2)
legend("top", names_leg, pch=15, cex = 1.3, col=brewer.pal(n = 6, name = "Set3"), title = "Bee Families")
mtext("a", side = 1, line = 4, at = -2, cex = 3.5, font = 2)

## Figure 2b: Family accumulation over years
par(mai=c(0.5, 0.5, 1.8, 0.2))
barplot(t(prop_years), las=1, col=color.vec, names=c("Andrenidae \n (N = 97)", "Apidae \n (N = 122)" , "Colletidae \n (N = 21)", "Halictidae \n (N = 60)", "Megachilidae \n (N = 148)", "Melittidae \n (N = 2)"))
mtext(side = 2, line = 2.75, text = "Proportion of total species collected", font = 2, cex = 2)
mtext(side = 1, line = 3.75, text = "Bee Family (N = total species collected)", font = 2, cex = 2)
years_leg = c("1996", "1997", "1998", "1999", "2002", "2011", "2012")
legend(4.35, 1.16, legend=years_leg[6:7], xpd=NA, ncol=2, pch=22, pt.bg=color.vec[6:7], pt.cex=3, text.font = 1, title = "Recent Collection Years")
legend(3.6,1.16, legend=years_leg[5], xpd=NA, ncol=1, pch=22, pt.bg=color.vec[5], pt.cex=3,  text.font = 1, title = "Bowl Study")
legend(1.5,1.16, legend=years_leg[1:4], xpd=NA, ncol=4, pch=22, pt.bg=color.vec[1:4], pt.cex=3,  text.font = 1, title = "Early Museum Collection Years")
mtext("b", side = 1, line = 4, at = -0.2, cex = 3.5, font = 2)

dev.off() # run this line after figure code to finish saving out figure to file


## Figure 3: Habitat type rarefaction curve
samples = read.csv("samples.csv", header = TRUE, row.names = 1)
## Species accumulation curve
## observed curve
accumcurve = specaccum(samples, method = "rarefaction")
quartz(height = 6, width = 10)
## Fig save for journal specs
postscript("Fig3.eps", width = 10, height = 6, horizontal = FALSE, onefile = FALSE, paper = "special", colormodel = "cmyk", family = "Arial")
par(mai=c(1, 1, 0.8, 0.2))
plot(accumcurve, ci.type="poly", col = "black", lwd=2, ci.lty=0, ci.col="grey", ylab = "Number of bee species found", xlab = "Number of plot samples in recent survey", cex = 0.8, cex.lab = 1.3, font.lab =2)
## expected curve for comparison
accumcurve.random = specaccum(samples, "random")
plot(accumcurve.random, col="blue", add=TRUE)

dev.off()

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

plot(surveys$Species ~ log(surveys$Area), las=1, pch=19, col = pinn_col)
log_mod = lm(surveys$Species ~ log(surveys$Area))
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

quartz(width=10, height=6)
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

#### Fig save for journal specs
postscript("Fig5.eps", width = 10, height = 5.5, horizontal = FALSE, onefile = FALSE, paper = "special", colormodel = "cmyk", family = "Arial")

par(mfrow=c(1,2))
par(mar=c(7, 5, 3, 2)+0.1)
surveys_col = rep("gray20", times = length(surveys[,1]))
surveys_col[which(percent_deviation_A>0)]="gray80"
plot(log(surveys$Species) ~ log(surveys$Area), las=1, pch=21, bg = surveys_col, ylab="ln (Number of bee species)", xlab="ln (Area surveyed (sq.km.))", cex.lab = 1.2, font.lab = 2)
points(x = log(surveys$Area), y = predict(power_mod), type='l', lwd=2)
points(x = log(surveys$Area)[which(surveys$Study=="Pinnacles National Park, CA")],
       y = log(surveys$Species)[which(surveys$Study=="Pinnacles National Park, CA")],
       pch=21, cex=3, col = "red")
mtext("a", side = 1, line = 5, at = 1.5, cex = 2, font = 2)

par(mar=c(12, 6, 3, 2)+0.1)
barplot(height = percent_deviation, las=2, names.arg = surveys$Study[sort_order], cex.names = 0.75, yaxs = 'r', ylab = "Observed relative to predicted\nspecies per area (%)", col = bar_col, cex.lab = 1.2, font.lab = 2, border = c("red", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black"))
abline(h=0)
box(which="plot")
legend("topright", legend = c("> predicted", "< predicted"), pch=22, pt.bg = c("gray80", "gray20"), pt.cex=2, inset=0.05, bty='n')
axis(side = 1, at = seq(from = 0.75, by = 1.2, length.out=length(surveys[,1])), labels=FALSE)
mtext("b", side = 1, line = 10, at = -3, cex = 2, font = 2)

dev.off()


#### S1 Fig
# load data on floral richness per plot sample
flor_rich = read.csv("floral_richness.csv", header = TRUE)
dim(flor_rich)

quartz(width = 10, height = 5.5)
par(mfrow=c(1,2), omi=c(0.2, 0.3, 0, 0))
## Linear Regression of FlorDiv and bee Richness
rich_rich = lm(flor_rich$beeRichness ~ flor_rich$floralRichness)
summary(rich_rich)
plot(beeRichness ~ floralRichness, data=flor_rich, col="black", pch=19, cex = 0.8, xlab= "Number of floral taxa", ylab= "Number of bee species", las=1, cex.lab = 1.2, font.lab = 2)
#abline(diversity)
text(x = c(12,12), y = c(62,56), labels=c("r2 = 0.37", "p < 0.001"))
# Build power model of Floral Richnes ~ Bee Richness
rich_rich_powerlog = lm(log(beeRichness) ~ log(floralRichness), data=flor_rich)
summary(rich_rich_powerlog)
floral_range = seq(from=min(na.omit(flor_rich$floralRichness)), to=max(na.omit(flor_rich$floralRichness)), by=1)
floral_range
betas = coefficients(rich_rich_powerlog)
betas
# Plot a regression line showing log relationship plotted along non-transformed data above
divmod_predict = exp(betas[1]+betas[2]*log(floral_range))
points(divmod_predict ~ floral_range, type = "l")
mtext("a", side = 1, line = 3, at = -1, cex = 2, font = 2)

###### Linear Regression of FlorDiv and bee sqrtAbundance
rich_abun = lm(flor_rich$sqrtAbun ~ flor_rich$floralRichness)
summary(rich_abun)
plot(sqrtAbun ~ floralRichness, data=flor_rich, col="black", pch=19, cex = 0.8, xlab= "Number of floral taxa", ylab= "sqrt (Bee abundance)", las=1, cex.lab = 1.2, font.lab = 2)
#abline(diversity)
text(x = c(12,12), y = c(34,31), labels=c("r2 = 0.16", "p < 0.001"))
# Build power model of Floral Richnes ~ Bee Abundance
rich_abun_powerlog = lm(log(sqrtAbun) ~ log(floralRichness), data=flor_rich)
summary(rich_abun_powerlog)
floral_range = seq(from=min(na.omit(flor_rich$floralRichness)), to=max(na.omit(flor_rich$floralRichness)), by=1)
floral_range
betas = coefficients(rich_abun_powerlog)
betas
# Plot a regression line showing log relationship plotted along non-transformed data above
divmod_predict = exp(betas[1]+betas[2]*log(floral_range))
points(divmod_predict ~ floral_range, type = "l")
mtext("b", side = 1, line = 3, at = -1, cex = 2, font = 2)

# format figure for journal
postscript("S1Fig.eps", width = 10, height = 5.5, horizontal = FALSE, onefile = FALSE, paper = "special", colormodel = "cmyk", family = "Arial")
dev.off()
