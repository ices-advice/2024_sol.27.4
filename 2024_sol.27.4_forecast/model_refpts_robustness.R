# model_refpts.R - DESC
# 2024_sol.27.4_benchmark_assessment/model_refpts.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLRef)

library(progressr)
handlers(global=TRUE)

library(doFuture)

# Windows
#plan(multisession, workers=2)

# Linux, OSX
plan(multicore, workers=14)

# DIMS
its <- 500
fy <- 2082

# LOAD
load('data/refpts_WKBFLAT1_2024.rda')
load('data/output.rda')

# DEFINE metrics
metrics <- list(SB = ssb, F = fbar, C = landings, TC = catch, Rec = rec)

# DEFINE statistics
stats <- list(
  meanFmsy= list(~yearMedians(F/FMSY), name="F/Fmsy",
    desc="Average annual F/Fmsy"),
  meanBmsy = list(~yearMedians(SB/SBMSY), name="B/Bmsy",
    desc="Average annual B/Bmsy"),
  meanCmsy = list(~yearMedians(C/MSY), name="Catch/MSY",
    desc="Average Catch/MSY over years"),
  aavC = list(~yearMedians(iav(C)), name="AAV",
    desc="Average annual variation in catches"),
  riskBlim = list(~apply(iterMeans((SB/Blim) < 1),1,max), name="P3(B<Blim)",
    desc="Probability that SSB < Blim"),
  risk10SB0 = list(~apply(iterMeans((SB/(SB0 * 0.10)) < 1), 1, mean),
    name="P(B<SB0.10)", desc="Probability that SSB < 10% SB0"),
  P80BMSY = list(~apply(iterMeans((SB/(SBMSY * 0.8)) > 1), 1, max),
    name="B>80Bmsy", desc="Probability that SSB > 80% x Bmsy"),
  meanSBMSY = list(~yearMedians(SB/SBMSY), name="SSB/SSB[MSY]",
    desc="Average annual SSB/SSBmsy"),
  meanFMSY = list(~yearMedians(F/FMSY), name="F/F[MSY]",
    desc="Average annual F/Fmsy")
)


# -- OM conditioning

# COERCE FLSR as bevholt(a,b)
nsr <- ab(fmle(as.FLSR(stk, model='bevholtSV'),
  fixed=list(s=params(srr)$s, v=params(srr)$v,
  spr0=params(srr)$v/params(srr)$R0)))

# FIT brps
brp <- brp(FLBRP(stk, sr=nsr))

# EXTRACT brefpts
brps <- remap(refpts(brp), R0=c('virgin', 'rec'), MSY=c('msy', 'yield'))

# ADD Blim (eqsim)
brps$Blim <- refpts$Blim

# CREATE FLom
om <- FLom(stock=stk, sr=srr, refpts=brps)

# EXTEND to future
om <- propagate(fwdWindow(om, end=fy), its)

# ADD SRR deviances
sigmaR <- out$parameters["SR_sigmaR", "Value"]
deviances(om) <- rlnormar1(its, sdlog=sigmaR, rho=0, years=seq(2022, fy))

# F and SSB deviances for shortcut and STF
sdevs <- shortcut_devs(om, Fcv=0.212, Fphi=0.423, SSBcv=0.10)


# -- MP setup

# SETUP standard ICES advice rule with eqsim
arule <- mpCtrl(list(

  # (est)imation method: shortcut.sa + SSB deviances
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=sdevs$SSB)),

  # hcr: hockeystick (fbar ~ ssb | lim, trigger, target, min)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=52352, target=0.157,
    min=0, metric="ssb", output="fbar")),

  # (i)mplementation (sys)tem: tac.is (C ~ F) + F deviances
  # rec as GM ignoring last 2 years
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=-2, fmin=0, Fdevs=sdevs$F))
  ))

# TEST run
advice <- mp(om, ctrl=arule, args=list(iy=2022))

plot(om, advice)

performance(advice, statistics=stats['riskBlim'],
  years=list(long=2052:fy))

# GET candidate values for Btrigger & Ftarget
frps <- lapply(seq(10, 45, by=5), function(x) {
  Fbrp(computeFbrp(stk, sr=nsr, proxy="bx", x=x, blim=0.10))
})

# CREATE combinations: FBx ~ Bx * c(0.60, 0.80, 1)
opts <- list(target=rep(unlist(lapply(frps, '[', 1)), each=3),
  trigger=unlist(lapply(seq(0.10, 0.45, by=0.05), function(x)
  (c(refpts(om)$B0) * x) * c(0.60, 0.8, 1))))

# ADD MSY options
opts$target <- c(opts$target, rep(c(refpts(om)$FMSY), 3))
opts$trigger <- c(opts$trigger, c(refpts(om)$BMSY) * c(0.6, 0.8, 1))

nms <- c(paste0(rep(paste0("FB", seq(10, 45, by=5)), each=3),
  rep(c(".6", ".8", "1"), 8)), c("FMSY.6", "FMSY.8", "FMSY1"))

# TODO: ADD FMSY ~ BMSY * c(0.6, 0.8, 1)

# RUN for all options on 'hcr' control element
system.time(
  plans <- mps(om, ctrl=arule, args=list(iy=2022), hcr=opts, names=nms)
)

plot(om, plans)

# -- COMPUTE performance

performance(plans) <- performance(plans, statistics=stats,
  years=list(all=2023:fy, long=2052:fy))

# TODO: MERGE performance + mp factors (trigger, target)

hps <- rbindlist(lapply(plans, function(x)
  as.data.frame(args(control(x)$hcr)[c("trigger", "target")])), idcol='mp')

performance(plans) <- merge(performance(plans), hps, by='mp')

# TODO: table()
#                         statistic
# year
#   mp (trigger, target)     XXX

# dcast(performance(plans)[year == 'long', .(mp, statistic, data)],
#  fun.aggregate=mean, mp ~ statistic, value.var='data')[order(riskBlim)]

# SAVE
save(om, advice, plans, file="model/plans.rda", compress="xz")
