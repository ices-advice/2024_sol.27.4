# utilities.R - DESC
# 2024_sol.27.4_benchmark_assessment/utilities.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(compResidual)

# osa_ss3 {{{

osa_ss3 <- function(x, plot=TRUE){ # x is the output list from SS_output in r4ss
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  res <- list()
  
  ## Dirichlet Multinomial OSA residuals
  for (k in unique(x$agedbase$Fleet)){
    
    theta <- subset(x$Age_Comp_Fit_Summary, Fleet==k)$val1
    
    tmp <- subset(x$agedbase, Fleet==k) # Obs = observed prop at age, Exp = predicted prop at age
    #tmp$obsN <- tmp$Obs*tmp$DM_effN
    tmp$obsN <- tmp$Obs*tmp$Nsamp_DM
    tmp$alpha <- tmp$Exp*theta*tmp$Nsamp_adj
    
    tmp2 <- reshape(tmp[,c("Yr", "Bin", "obsN", "alpha")], idvar="Bin", timevar="Yr", direction="wide")
    
    DM_obs <- as.matrix(tmp2[,grep("obsN",colnames(tmp2))]) # obs should be rounded 
    DM_alpha <- as.matrix(tmp2[,grep("alpha",colnames(tmp2))]) # pred cannot be 0
    
    osa_res <- resDirM(round(DM_obs), DM_alpha)
    dimnames(osa_res) <- list(tmp2$Bin[-nrow(DM_obs)], unique(tmp$Yr))
    if (length(as.numeric(colnames(osa_res))[1]:as.numeric(colnames(osa_res))[ncol(osa_res)])!=ncol(osa_res)) {
      missing_years <- (as.numeric(colnames(osa_res))[1]:as.numeric(colnames(osa_res))[ncol(osa_res)])[as.numeric(colnames(osa_res))[1]:as.numeric(colnames(osa_res))[ncol(osa_res)] %!in% as.numeric(colnames(osa_res))]
      tmp3 <- matrix(nrow=nrow(osa_res), ncol=length(missing_years), dimnames=list(rownames(osa_res), missing_years))
      osa_res <- cbind(osa_res,tmp3)[,as.character(as.numeric(colnames(osa_res))[1]:as.numeric(colnames(osa_res))[ncol(osa_res)])]
      class(osa_res) <- "cres"
    }
    if (plot) plot(osa_res, main=paste0("Fleet ", k))
    
    res[[length(res)+1]] <- osa_res
  }
  res
}
# }}}

# msy_ranges {{{

msy_ranges <- function(obj, msy) {
  
  msy_lowupp <- obj %>% select(SPRloop:Tot_Catch) %>% 
    mutate(dif = Tot_Catch - msy*.95)
  
  fs <- sort(obj[["F_report"]])
    
  # lower bound
  xlow <- msy_lowupp %>% filter(F_report < ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
  posl <- which(fs == xlow$F_report)
  posl <- c(posl, ifelse( xlow$dif < 0, posl+1, posl-1))
  flow <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posl])), 
                   data.frame(Tot_Catch = msy*.95))[[1]]
  
  # upper bound
  xupp <- msy_lowupp %>% filter(F_report > ssFmsy) %>% filter(abs(dif) == min(abs(dif)))
  posu <- which(fs == xupp$F_report)
  posu <- c(posu, ifelse( xupp$dif < 0, posu+1, posu-1))
  fupp <- predict( lm( F_report ~ Tot_Catch, data = msy_lowupp %>% filter(F_report %in% fs[posu])), 
                   data.frame(Tot_Catch = msy*.95))[[1]]
  
  return(c(flow=flow, fupp=fupp)) 
}
# }}}

# FixedBevHolt {{{

fixedBevholt <- function(ab, ssb) {
  log(4 * params$h * params$r0 * ssb /(params$B0 *(1 - params$h) + ssb * (5 * params$h - 1)))
}

# }}}

# fixedSegreg {{{
fixedSegreg <- function(ab, ssb) {
  log(ifelse(ssb >= blim, ab$a * blim, ab$a * ssb))
}
# }}}

# boundedSegreg {{{

boundedSegreg <- function(ab, ssb) {
  ab$b <- ab$b + Blim
  Segreg (ab, ssb)
}
# }}}

# extractMetrics {{{
extractMetrics <- function(out) {

  flq <- FLQuant(dimnames=list(age='all', year=seq(out$startyr, out$endyr)),
    units="t")

  ca <- data.table(out$catch)[Yr >= 1957, .(Yr, Obs, Fleet_Name)]

  lan <- flq %=% ca[Fleet_Name == 'Fleet']$Obs
  dis <- flq %=% ca[Fleet_Name == 'Discards']$Obs

  tot <- lan + dis
  
  dis[, ac(1957:2001)] <- NA
  can <- ubind(lan, dis, tot) 
  dimnames(can)$unit <- c("Landings", "Discards", "Catch")

  res <- FLQuants(Landings=lan, Discards=dis, Catch=can,
    Rec=extractRec(out), SSB=extractSSB(out), F=extractFbar(out))

  return(res)
}
# }}}

# extractMetricCIs {{{
extractMetricCIs <- function(out) {

  SSB=extractSSBci(out)
  Rec=extractRecci(out)
  Fbar=extractFbaci(out)

  res <- FLQuants(SSB=SSB, Rec=Rec, F=Fbar)

  return(res)
}
# }}}

# 
library(icesAdvice)

foo <- function(x) {
  ifelse(x < 10, icesRound(x), sprintf("%i", round(x)))
}

library(ggpubr)

plotTable <- function(x, rows=NULL, ...) {

  dat <- as.data.frame(x, drop=TRUE)

  dat$data <- foo(dat$data)

  ggtexttable(t(dat), rows=rows, ...)
}

plotCatchcurve <- function(x) {

  dat <- as.data.frame(x, cohort=TRUE)
  yrs <- unique(dat$year)

  ggplot(dat, aes(x=ISOdate(year, 1, 1), y=log(data),
    group=factor(age), color=factor(age))) +
    geom_line(aes(group=factor(cohort)), color="black", alpha=0.4) +
    geom_point(colour='white', size=5) + 
    geom_text(aes(label=age), fontface = "bold") +
    xlab("Cohort") + ylab("") +
    theme(legend.position="none")
}
