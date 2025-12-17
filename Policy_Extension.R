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
install.packages("sandwich")
install.packages("car")

library("data.table")
library("plm")
library("stargazer")
library("lmtest")
library("reshape2")
library("sandwich")
library("car")

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

#Subset of the data

obs.window.list <- list(c(1995:1999), c(1993:2009))
this.model.data <- subset(hosp_zip3_quarter_1983_2009, prop.socal>0 | prop.la>0)
results <- list()

for(obs.window in obs.window.list){
  dta.agg.jl <- prepModelData( dta = this.model.data, fbi.name = "jl.codes.mdc6", control.name = "control.jl.codes", years = obs.window, control.grp = TRUE, filter = FALSE)
  
  #Treatment Variables (mandatory and voluntary)
  
  dta.agg.jl$treatment.m <- dta.agg.jl$disease.type * dta.agg.jl$m
  
  dta.agg.jl$treatment.v <- dta.agg.jl$disease.type * dta.agg.jl$v
  
  # interaction term
  dta.agg.jl$post_policy_interaction <- as.numeric(dta.agg.jl$year >= 1998) * dta.agg.jl$disease.type
  ###
  triple.lm <- lm(logCount ~ treatment.m + treatment.v + post_policy_interaction + m + v + factor(year.qtr) + factor(geo.type.pk), data = dta.agg.jl)
  
  adjusted.se.triple <- coeftest(triple.lm, vcov = vcovHC(triple.lm, type = "HC0", cluster = "group"))
  
  r2.triple <- formatSig(summary(triple.lm)$r.squared, 2)
  
  results[[paste0("window_", min(obs.window), "_", max(obs.window))]] <- list(triple.lm = triple.lm, adjusted.se.triple = adjusted.se.triple, r2.triple = r2.triple )
}


######### F Test Joint Significance Test on Interaction term, LA mandatory + Voluntary and other terms from table (each window)

f_test_results <- data.frame(
  Window = character(),
  F_stat = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for(win in names(results)){
  res <- results[[win]]
   f_real <- linearHypothesis(
   res$triple.lm,
   c("treatment.m = 0", "treatment.v = 0", "post_policy_interaction = 0"),
   vcov = vcovHC(res$triple.lm, type = "HC0", cluster = "group")
  )
   f_test_results <- rbind(
   f_test_results,
   data.frame(Window = win, F_stat = f_real$F[2], p_value = f_real$`Pr(>F)`[2])
  )
}

print(f_test_results)
  
  
  