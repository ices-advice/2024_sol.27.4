# model.R - Run forecast, using Mohn's rho-corrected N and Ciy = uptake * TA
# 2022_sol.27.4_forecast/model.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir('model')

library(FLCore)
library(FLasher)

set.seed(697)

# LOAD SS3 results and refpts
load('data/output.rda')
load('data/refpts_WKBFLAT1_2024.rda')

run <- stk

# --- SETUP

# TAC advice current year (2024)
tac <- FLQuant(3675, dimnames=list(age='all', year=2024), units="tonnes")

# data, intermediate, advice and final YEARS

dy <- dims(run)$maxyear
iy <- dy + 1
ay <- iy + 1
fy <- ay + 1

# GEOMEAN recruitment but last 3 years
rec0gm <- exp(mean(log(window(stock.n(run)["0",], end=-3))))

# GENERATE targets from refpts
targets <- expand(as(refpts, 'FLQuant'), year=ay)


# --- SETUP future

# 3 years, 3 year mean wts/selex, 3 year mean discards
# TODO: CHOICE of years
fut <- stf(run, nyears=3, wts.nyears=3, fbar.nyears=5, disc.nyears=3)

# SET geomean SRR
gmsrr <- predictModel(model=rec~a, params=FLPar(c(rec0gm), units="thousands",
  dimnames=list(params="a", year=seq(iy, length=3), iter=1)))


# --- INTERMEDIATE year assumption

# C 2024 = tac_2024 
fut <- fwd(fut, sr=gmsrr, catch=tac)

# UPDATE 2024
fut <- fwd(fut, sr=gmsrr, fbar=targets["Fmsy",])


# --- PROJECT catch options

# DEFINE catch options
catch_options <- list(

  # advice
  advice=FLQuants(fbar=targets["Fmsy",] *
    min((ssb(fut)[, ac(ay)] / refpts$Btrigger), 1)),

  # FMSY
  Fmsy=FLQuants(fbar=targets["Fmsy",]),

  # lowFMSY
  lFmsy=FLQuants(fbar=targets["lFmsy",]),

  # uppFMSY
  uFmsy=FLQuants(fbar=targets["uFmsy",]),

  # lowFMSYadvice
  lFmsyadvice=FLQuants(fbar=targets["lFmsy",] *
    min((ssb(fut)[, ac(ay)] / refpts$Btrigger), 1)),

  # F0
  F0=FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=ay))),

  # Fpa
  Fpa=FLQuants(fbar=targets["Fpa",]),

  # TODO: F05noAR
#  F05noAR=FLQuants(fbar=targets["F05noAR",]),

  # Flim
  Flim=FLQuants(fbar=targets["Flim",]),

  # Bpa
  Bpa=FLQuants(ssb_flash=targets["Bpa",]),

  # Blim
  Blim=FLQuants(ssb_flash=targets["Blim",]),

  # MSYBtrigger
  MSYBtrigger=FLQuants(ssb_flash=targets["Btrigger",]),

  # F iy
  lastF=FLQuants(fbar=expand(fbar(fut)[, ac(iy)], year=ay)),
                 
  # Fmp 0.20
  Fmp=FLQuants(fbar=FLQuant(0.20, dimnames=list(age='all', year=ay))),

  # TAC iy
  rotac=FLQuants(catch=expand(tac, year=ay))
)

# C0, fy + 1
F0 <- FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=fy)))

# CONVERT to fwdControl: intermediate year + catch option + F0

fctls <- lapply(catch_options, function(x)
  as(FLQuants(c(x, F0)), "fwdControl")
)

# RUN!
runs <- FLStocks(lapply(fctls, function(x) fwd(fut, sr=gmsrr, control=x)))

# COMPARE
Map(compare, runs, fctls)

# CHECK advice ay
catch(runs$advice)[, ac(iy:ay)]
ssb(runs$advice)[, ac(iy:fy)]


# --- PROJECT F levels

flevels <- seq(0, 0.50, by=0.01)

# BUILD control with targets as iters
control <- as(FLQuants(
  fbar=append(FLQuant(flevels,
    dimnames=list(age='all', year=ay, iter=seq(flevels))), F0[[1]])),
  "fwdControl")

# RUN
fruns <- divide(fwd(fut, sr=gmsrr, control=control), names=flevels)

# SAVE
save(runs, fruns, fctls, rec0gm, tac, file="model/model.rda", compress="xz")
