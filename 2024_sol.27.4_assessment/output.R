# output.R - DESC
# /home/mosqu003/Active/MEETING_WGNSSK_2024-LWT/2024_sol.27.4_assessment/output.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# TODO: RENAME stk to run, CONSTRUCT input stk

library(ss3om)

source("utilities.R")

# --- ss3 {{{

path <- file.path('model', 'ss3')

# LOAD output
out <- SS_output(path, covar=TRUE)

# COMPUTE OSA
osa <- osa_ss3(out, plot=FALSE)

# retros
retroModels <- SSgetoutput(dirvec=file.path(path, "retrospectives",
  paste("retro",0:-5,sep="")))

retroSummary <- SSsummarize(retroModels)

# FLR

# FLStock 
stk <- readFLSss3(path, range=c(minfbar=2, maxfbar=6))

# BUG: CORRECT

dat <- SS_readdat('model/ss3/sol274.dat')
waa <- data.table(SS_readwtatage('model/ss3/wtatage.ss_new'))

# landings
landings.n(stk) <- c(t(data.table(dat$agecomp)[FltSvy == 1, seq(10, 20), with=FALSE]))

landings.wt(stk) <- c(t(waa[Fleet == 1 & Yr %in% seq(1957, 2023),
  seq(7, 17), with=FALSE]))

landings(stk) <- data.table(dat$catch)[fleet == 1 & year %in% 1957:2023, catch]
landings(stk) / computeLandings(stk)

# discards.n
discards.n(stk)[, ac(2002:2023)] <- c(t(data.table(dat$agecomp)[FltSvy == 4, seq(10, 20), with=FALSE]))

discards.wt(stk) <- c(t(waa[Fleet == 4 & Yr %in% seq(1957, 2023),
  seq(7, 17), with=FALSE]))

discards(stk)[, ac(2002:2023)] <- data.table(dat$catch)[fleet == 4 & year %in% 2002:2023, catch]
discards(stk) / computeDiscards(stk)

catch(stk) <- computeCatch(stk, 'all')


# TODO: LOAD estimated

# discards.n
discards.n(stk)[, ac(2002:2023)] <- c(t(DT(out$discard_at_age)[Yr %in% seq(2002, 2023) & Type == 'dead', seq(12, 22), with=FALSE]))

# Stock metrics
mets <- extractMetrics(out)
metsci <- extractMetricCIs(out)

# CHECK

mets$Landings / computeLandings(stk)
mets$Discards / computeDiscards(stk)

# FLPar refpts
rps <- readFLRPss3(path)

# FLSR
srr <- readFLSRss3(path)

# RETRO
retroStocks <- lapply(setNames(file.path(path, "retrospectives",
  paste("retro",0:-5,sep="")), paste("retro",0:-5,sep="")),  readFLSss3)

retroStocks <- FLStocks(Map(function(x, y)
  window(x, end=y), x=retroStocks, y=seq(2022, 2017)))

retroMetrics <- lapply(retroModels, extractMetrics)

# JITTER

jitters <- SSsummarize(SSgetoutput(dirvec = paste0(path, '_jitter'),
  keyvec = 1:50, getcovar = FALSE))

# -- mcmc

mcout <- data.table(SSgetMCMC('model/ss3_mcmc/'))

flq <- FLQuant(dimnames=list(age="all", year=seq(1957, 2023), iter=seq(500)))

ssb <- flq %=% c(t(mcout[500:999, paste0("SSB_", seq(1957, 2023)), with=FALSE]))
units(ssb) <- "t"

fbar <- flq %=% c(t(mcout[500:999, paste0("F_", seq(1957, 2023)), with=FALSE]))
units(fbar) <- "f"

rec <- flq %=% c(t(mcout[500:999, paste0("Recr_", seq(1957, 2023)), with=FALSE]))
units(rec) <- "1000"

mcmets <- FLQuants(Rec=rec, SSB=ssb, Catch=mets$Catch, F=fbar)

recdevs <- exp(flq %=% c(t(mcout[500:999, paste0("Main_RecrDev_", seq(1957, 2023)), with=FALSE])))


# --

sov <- fread("boot/initial/data/intercatch/StockOverview.txt")

sov[`Catch Cat.` == 'Landings', .(Catch=sum(`Catch. kg`)), by=Fleets]



# SAVE
save(out, stk, mets, metsci, srr, rps, retroStocks, retroMetrics, retroSummary, 
  osa, mcmets, recdevs, file="output/output.rda", compress='xz')
