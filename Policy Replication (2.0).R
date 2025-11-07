setwd("/Users/ashantimahoney/Desktop/Fall 2025/Policy Data")
library(readr)
hosp_zip3_quarter_1983_2009 <- read_csv("Data/hosp_zip3_quarter_1983_2009.csv")

source("Code/Functions/prepModelData.R")
source("Code/Functions/installPackageNotFound.R")
source("Code/Functions/formatSig.R")
source("Code/Functions/calculateFRatio.R")

install.packages("data.table")
install.packages("plm")
install.packages("stargazer")
install.packages("lmtest")
install.packages("reshape2")

library("data.table")
library("plm")
library("stargazer")
library("lmtest")
library("reshape2")

#To fix error from stargazer on output 
detach("package:stargazer",unload=T)
remove.packages("stargazer")
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
untar("stargazer_5.2.3.tar.gz")
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
install.packages("stargazer", repos = NULL, type="source")

library("stargazer")


#Subset of the data. We want both windows (Jin and Leslie Window, and expanded window) + we only want California and Southern California zipcode

obs.window.list <- list(c(1995:1999), c(1993:2009))
this.model.data <- subset(hosp_zip3_quarter_1983_2009, prop.socal>0 | prop.la>0)

for(obs.window in obs.window.list){
  dta.agg.jl <- prepModelData( dta = this.model.data, fbi.name = "jl.codes.mdc6", control.name = "control.jl.codes", years = obs.window, control.grp = TRUE, filter = FALSE)
  
  #Treatment Variables (mandatory and voluntary)
  
  dta.agg.jl$treatment.m <- dta.agg.jl$disease.type * dta.agg.jl$m
  
  dta.agg.jl$treatment.v <- dta.agg.jl$disease.type * dta.agg.jl$v
  
  
  #Table specifications (focusing on only JL and DDD)
  n.obs <- nrow (dta.agg.jl)
  did.data <- subset(dta.agg.jl, disease.type == 1)
  
  names(did.data)[which(names(did.data)=="m")] <- "treatment.m"
  names(did.data)[which(names(did.data)=="v")] <- "treatment.v"
  
  # Jl Specification 
  
  jl.model <- plm(logCount ~ treatment.m + treatment.v + m + v , index = c("geo.type.pk", "year.qtr"), model = "within", effect = "twoways" , data= dta.agg.jl)
  
  adjusted.se <- coeftest(jl.model, vcov=vcovHC(jl.model, type="HC0", cluster= "group"))
  
  jl.lm <- lm(logCount ~ treatment.m + treatment.v + m + v + factor(year.qtr) + factor (geo.type.pk), data = dta.agg.jl)
  
  r2 <- formatSig(summary(jl.lm)$r.squared, 2)
  
  # Triple Difference in Difference Model (including interaction term)
  
  jl.model.triple <- plm(logCount ~ treatment.m + treatment.v + I(as.numeric(year>=1998) * disease.type) + m + v , index = c("geo.type.pk" , "year.qtr"), model = "within", effect = "twoways" , data = dta.agg.jl)
  
  adjusted.se.triple <- coeftest(jl.model.triple, vcov=vcovHC(jl.model.triple, type= "HC0" , cluster= "group"))
  
  triple.lm <- lm(logCount ~ treatment.m + treatment.v + I(as.numeric(year>=1998) * disease.type) + m + v + I(as.numeric(year>=1998) * disease.type) + factor(year.qtr) + factor(geo.type.pk), data = dta.agg.jl)
  
  r2.triple <- formatSig(summary(triple.lm)$r.squared, 2) 
  
  assign(paste0("adjusted.se.", min(obs.window), ".", max(obs.window), ".", "so.cal"), adjusted.se)
  assign(paste0("jl.lm.", min(obs.window), ".", max(obs.window), ".", "so.cal"), jl.lm)
  assign(paste0("r2.", min(obs.window), ".", max(obs.window), ".", "so.cal"), r2)
  
   assign(paste0("adjusted.se.triple.", min(obs.window), ".", max(obs.window), ".", "so.cal"), adjusted.se.triple)
  assign(paste0("triple.lm.", min(obs.window), ".", max(obs.window), ".", "so.cal"), triple.lm)
  assign(paste0("r2.triple.", min(obs.window), ".", max(obs.window), ".", "so.cal"), r2.triple)
  
}
# F 
f1 <- calculateFRatio (triple.lm.1995.1999.so.cal, jl.lm.1995.1999.so.cal)
f2 <- calculateFRatio(triple.lm.1993.2009.so.cal, jl.lm.1993.2009.so.cal)

#Table 
cov.label <- c("Foodborne $\\times$ LA Mandatory", "Foodborne $\\times$ LA Voluntary", "Foodborne $\\times$ South Cali", "LA Mandatory Disclosure", "LA Voluntary Disclosure")

latex_output <- stargazer(jl.lm.1995.1999.so.cal, triple.lm.1995.1999.so.cal, jl.lm.1993.2009.so.cal, triple.lm.1993.2009.so.cal,
                          coef = list(round(jl.lm.1995.1999.so.cal$coefficients, 2),
                                      round(triple.lm.1995.1999.so.cal$coefficients,2),
                                      round(jl.lm.1993.2009.so.cal$coefficients,2),
                                      round(triple.lm.1993.2009.so.cal$coefficients,2)),
                          se = list(adjusted.se.1995.1999.so.cal[,2],
                                    adjusted.se.triple.1995.1999.so.cal[,2],
                                    adjusted.se.1993.2009.so.cal[,2],
                                    adjusted.se.triple.1993.2009.so.cal[,2]),
                          p = list(adjusted.se.1995.1999.so.cal[,4],
                                   adjusted.se.triple.1995.1999.so.cal[,4],
                                   adjusted.se.1993.2009.so.cal[,4],
                                   adjusted.se.triple.1993.2009.so.cal[,4]),
                          covariate.labels = cov.label,
                          column.separate = c(1,1,1,1),
                          digits = 2,
                          dep.var.labels = "",
                          dep.var.caption = "",
                          notes.append = FALSE, 
                          column.sep.width = "1pt",
                          no.space = TRUE,
                          type = "text",
                          omit = c("year.qtr", "geo.type.pk"), 
                          omit.stat = c("rsq", "adj.rsq", "f"),
                          notes.align = "l",
                          add.lines = list (c("R2",
                                              r2.1995.1999.so.cal,
                                              r2.triple.1995.1999.so.cal,
                                              r2.1993.2009.so.cal,
                                              r2.triple.1993.2009.so.cal),
                                            c("F-Stat","",paste0(formatSig(f1[1], 2),
                                                                 
                                                                 ifelse(f1[2]<0.01, "***",
                                                                        ifelse(f1[2]<0.05, "**",
                                                                               ifelse(f1[2]<0.10, "")))),
                                              "", paste0(formatSig(f2[1],2), 
                                                         
                                                         ifelse(f2[2] < 0.01, "***",
                                                                ifelse(f2[2] < 0.05, "**",
                                                                       ifelse(f2[2] < 0.10, "*","" )))))))
                                                                       
















