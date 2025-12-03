setwd("/Users/ashantimahoney/Desktop/Fall 2025/Policy Data")

source("Code/Functions/prepModelData.R")
source("Code/Functions/installPackageNotFound.R")
source("Code/Functions/formatSig.R")
source("Code/Functions/allocateProportionalCounts.R")

install.packages("data.table")
install.packages("plm")
install.packages("reshape2")

library("data.table")
library("plm")
library("reshape2")

## Filtering data to just Southern Cali and LA
library(readr)
hosp_zip3_quarter_1983_2009 <- read_csv("Data/hosp_zip3_quarter_1983_2009.csv")
hosp.data <- subset(hosp_zip3_quarter_1983_2009, (prop.la>0 | prop.socal > 0) & (year%in%1995:1999))

# JL Disease 
dta.agg.jl.alt <- prepModelData(dta = hosp.data, fbi.name = "jl.codes.mdc6", control.name = "control.jl.codes", years = 1993:2009, control.grp = TRUE, filter = FALSE)
# JL Spec
jl.lm.log <- lm(logCount ~ -1 + I(prop.la*as.numeric(year>=1998)) + I(prop.la*as.numeric(year>=1998)*disease.type) + as.factor(year.qtr) + as.factor(geo.type.pk), data=dta.agg.jl.alt)
# DDD 
ddd.lm.log <- lm(logCount ~ -1 + I(prop.la*as.numeric(year>=1998)) +  I(prop.la*as.numeric(year>=1998)*disease.type) + as.factor(year.qtr.type) + as.factor(geo.type.pk), data=dta.agg.jl.alt)

###
la.mat <- aggregate(prop.la~geo, data = dta.agg.jl.alt, FUN = max)

pred.data <- expand.grid(la = c(0,1), disease.type = c(0,1), year = sort(unique(hosp.data$year)))

ddd.model.data <- data.frame(coeff.names = names(ddd.lm.log$coefficients), coefficients = ddd.lm.log$coefficients, stringsAsFactors = FALSE)

jl.model.data <- data.frame(coeff.names = names(jl.lm.log$coefficients), coefficients = jl.lm.log$coefficients, stringsAsFactors = FALSE)

ddd.model.data$zip3 <- as.numeric(ifelse(grepl("geo.type.pk", ddd.model.data$coeff.names), substring(ddd.model.data$coeff.names, 23, nchar(ddd.model.data$coeff.names)-2), ""))
ddd.model.data$prop.la <- la.mat[match(ddd.model.data$zip3, la.mat$geo), "prop.la"]
ddd.model.data$prop.la[is.na(ddd.model.data$prop.la)] <- 1

jl.model.data$zip3 <- as.numeric(ifelse(grepl("geo.type.pk", jl.model.data$coeff.names), substring(jl.model.data$coeff.names, 23, nchar(jl.model.data$coeff.names)-2), ""))
jl.model.data$prop.la <- la.mat[match(jl.model.data$zip3, la.mat$geo), "prop.la"]
jl.model.data$prop.la[is.na(jl.model.data$prop.la)] <- 1

ddd.year.coeff <- ddd.model.data$coeff.names[grepl("year.qtr.type", ddd.model.data$coeff.names)]
jl.year.coeff <- jl.model.data$coeff.names[grepl("year.qtr", jl.model.data$coeff.names)]
zip.coeff <- ddd.model.data$coeff.names[grepl("geo.type.pk", ddd.model.data$coeff.names)]

## Bulk of code (loop) TESTING PART 2 

for(i in 1:nrow(pred.data)){ if(pred.data$la[i]==1){ prop.factor <- ddd.model.data$prop.la[ddd.model.data$coeff.names%in%zip.coeff] }else{ prop.factor <- 1-ddd.model.data$prop.la[ddd.model.data$coeff.names%in%zip.coeff]}
  
 b1 <- pred.data$la[i] * as.numeric(pred.data$year[i] >= 1998)
 b2 <- pred.data$la[i] * as.numeric(pred.data$year[i] >= 1998) * pred.data$disease.type[i]
  
year.ddd.filter <- grepl(pred.data$year[i], ddd.year.coeff)
year.ddd.vec <- as.numeric(year.ddd.filter & substring(ddd.year.coeff, nchar(ddd.year.coeff), nchar(ddd.year.coeff))==pred.data$disease.type[i] )*0.25
year.jl.filter <- grepl(pred.data$year[i], jl.year.coeff)
year.jl.vec <- as.numeric(year.jl.filter)*0.25
  
zip.vec <- as.numeric(substring(zip.coeff, nchar(zip.coeff), nchar(zip.coeff))==pred.data$disease.type[i])*prop.factor
  
if(pred.data$disease.type[i] == 0 & pred.data$la[i]){ zip.vec <- zip.vec/(sum(zip.vec)+1) }else{ zip.vec <- zip.vec/sum(zip.vec) }
  
pred.ddd.vec <- c(b1,b2,year.ddd.vec,zip.vec)
pred.jl.vec <- c(b1,b2,year.jl.vec,zip.vec)
  
this.ddd.pred <- sum(pred.ddd.vec*ddd.model.data$coefficients, na.rm = TRUE)
dta.agg.jl.alt$p.ddd.log[dta.agg.jl.alt$geo.type.pk=="902-1" & #dta.agg.jl.alt$year.qtr.type=="1992-1-1"]
dta.agg.jl.alt$year.qtr.type=="1993-1-1"]
this.jl.pred <- sum(pred.jl.vec*jl.model.data$coefficients, na.rm = TRUE)
  
pred.data$ddd.pred[i] <- this.ddd.pred
pred.data$jl.pred[i] <- this.jl.pred
}

## 
#LA
these.vars <- c("logCount")
log.fbi.yearly <- allocateProportionalCounts(this.data = dta.agg.jl.alt, prop.var = "prop.la", vars = these.vars)

#SOCAL 
these.vars <- c("logCount")
counts.soc <- allocateProportionalCounts(this.data = dta.agg.jl.alt, prop.var = "prop.socal", vars = these.vars)

## PLOT
#Parameters
par(mar=c(3,3,2,1), mgp=c(1.5,0.5,0), tcl=-0.3)
m <- rbind(c(3,1,2), c(6,4,5))
layout(m, widths = c(0.2, 0.4, 0.4), heights = c(1,1,1))

#Colors for plot (Pink and Green)
ca.color <- rgb(1,0.75,0.8,0.7)
ca.color2 <- rgb(1,0.4,0.6,0.9)
la.color <- rgb(0,0.33,0.17,0.6)
la.color2 <- rgb(0,0.33,0.17,0.8)
la.lwd <- 0.5
ca.lwd <- 2.5

#Observation window (pre-treatment - post-treatment)
all.years <- 1995:1999
pre.years <- 1995:1997 
post.years <- 1998:1999 

### FOODBORNE PLOT WITH JL PREDICTED VALUES AGAINST WHAT IS ACTUALLY OBSERVED ###
plot(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], 
     log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], 
     type = "n",
     lty = 5,
     lwd = 2,
     ylim = c(1.2, 1.82),
     col = ca.color,
     ylab = "log(Hospitalizations)", xlab = "Year",
     main = "Foodborne Disorders")

## Plot actual values
lines(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], 
      log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1],lty=5,lwd=2,col=ca.color2)
lines(log.fbi.yearly$year[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==1], 
      log.fbi.yearly$logCount[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==1], 
      type = "l", lty = 5, col = la.color, lwd = 1)
abline(v=1997.5,col=rgb(0,0,0,0.6),lwd=1)
text(1998, 1.75, "LA enacts\ngrading",cex=0.8)
text(x = 1999, y = 1.30, label = "LA", cex = 0.9, col = "BLACK")
text(x = 1998.5, y = 1.5, label = "Southern CA", cex = 0.9, col = "BLACK")
text(1995.5, 1.65, "Predicted")
text(1995.4, 1.78, "Actual")

## Plot JL predicted values
lines(pred.data$year[pred.data$la==1 & pred.data$disease.type==1], 
      pred.data$jl.pred[pred.data$la==1 & pred.data$disease.type==1], type = "l", lty = 1, col = la.color,lwd=0.5)
lines(pred.data$year[pred.data$la==0 & pred.data$disease.type==1], 
      pred.data$jl.pred[pred.data$la==0 & pred.data$disease.type==1], type = "l", lty = 1, col = ca.color2,lwd=ca.lwd)

### NON-FOODBORNE PLOT WITH JL PREDICTED VALUES AGAINST WHAT IS ACTUALLY OBSERVED ###
plot(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], 
     type = "n",
     lty = 5,
     lwd = 2,
     ylim = c(6.4, 7.08),
     col = ca.color,
     ylab = "log(Hospitalizations)", xlab = "Year",
     main = "Digestive Disorders")

## Plot actual values 
lines(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0],lty = 5, lwd = 2, col=ca.color2)
lines(log.fbi.yearly$year[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==0], 
      log.fbi.yearly$logCount[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==0], 
      type = "l", lty = 5, col = la.color, lwd = 1)
abline(v=1997.5,col=rgb(0,0,0,0.6),lwd=1)

# JL PREDICTED VALUES 
lines(pred.data$year[pred.data$la==1 & pred.data$disease.type==0], 
      pred.data$jl.pred[pred.data$la==1 & pred.data$disease.type==0], type = "l", lty = 1, col = la.color,lwd=0.5)
lines(pred.data$year[pred.data$la==0 & pred.data$disease.type==0], 
      pred.data$jl.pred[pred.data$la==0 & pred.data$disease.type==0], type = "l", lty = 1, col = ca.color2,lwd=ca.lwd)

## AFTER INCLUDING INTERACTION --- DDD ###
#Plot foodborne
plot(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], 
     type = "n",
     lty = 5,
     lwd = 2,
     ylim = c(1.2, 1.82),
     col = ca.color,
     ylab = "log(Hospitalizations)", xlab = "Year",
     main = "Foodborne Disorders")
abline(v=1997.5,col=rgb(0,0,0,0.6),lwd=1)

## Actual values
lines(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==1], 
      lty = 5,
      lwd = 1,
      col = ca.color)
lines(log.fbi.yearly$year[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==1], 
      log.fbi.yearly$logCount[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==1], 
      type = "l", lty = 5, col = la.color, lwd = 1)

#Plot DDD predicted values
lines(pred.data$year[pred.data$la==1 & pred.data$disease.type==1], 
      pred.data$ddd.pred[pred.data$la==1 & pred.data$disease.type==1], type = "l", lty = 1, col = la.color,lwd=0.5)
lines(pred.data$year[pred.data$la==0 & pred.data$disease.type==1], 
      pred.data$ddd.pred[pred.data$la==0 & pred.data$disease.type==1], type = "l", lty = 1, col = ca.color,lwd=1)

#Plot non-foodborne
plot(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], 
     type = "n",
     lty = 5,
     lwd = 2,
     ylim = c(6.4, 7.08),
     col = ca.color,
     ylab = "log(Hospitalizations)", xlab = "Year",
     main = "Digestive Disorders")
abline(v=1997.5,col=rgb(0,0,0,0.6),lwd=1)
lines(log.fbi.yearly$year[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], log.fbi.yearly$logCount[log.fbi.yearly$la==0 & log.fbi.yearly$disease.type==0], 
      lty = 5,
      lwd = 1,
      col = ca.color)
lines(log.fbi.yearly$year[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==0], 
      log.fbi.yearly$logCount[log.fbi.yearly$la==1 & log.fbi.yearly$disease.type==0], 
      type = "l", lty = 5, col = la.color, lwd = 1)

#Plot DDD predicted values
lines(pred.data$year[pred.data$la==1 & pred.data$disease.type==0], 
      pred.data$ddd.pred[pred.data$la==1 & pred.data$disease.type==0], type = "l", lty = 1, col = la.color,lwd=1)
lines(pred.data$year[pred.data$la==0 & pred.data$disease.type==0], 
      pred.data$ddd.pred[pred.data$la==0 & pred.data$disease.type==0], type = "l", lty = 1, col = ca.color,lwd=1)




