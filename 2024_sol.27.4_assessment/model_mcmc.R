# model_mcmc.R - DESC
# /home/mosqu003/Active/MEETING_WGNSSK_2024-LWT/2024_sol.27.4_assessment/model_mcmc.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(doFuture)
plan(multicore, workers=2)


# --- McMC

path <- file.path('model', 'ss3_mcmc')
mkdir(path)

# PREPARE
cp("boot/initial/model/ss3/*", path)

# FIT
r4ss::run(path, exe="ss_3.30.22.1", show_in_console=TRUE)

# McMC

r4ss::run(path, exe="ss_3.30.22.1", extras = "-mcmc 1e5 -mcsave 1e2",
  show_in_console=TRUE, skipfinished=FALSE)

r4ss::run(path, exe="ss_3.30.22.1", extras = "-mceval", show_in_console=TRUE,
  skipfinished=FALSE)

# GENERATE output
mcmc.df <- SSgetMCMC(dir=path, writecsv = T,
  keystrings = c("NatM", "R0", "steep", "Q_extraSD"),
  nuisancestrings = c("Objective_function", "SSB_", "InitAge", "RecrDev"))

# SAVE
save(mcmc.df, path, file="model/model_mcmc.rda", compress="xz")


# --- NUTS

library(adnuts)

path <- file.path('model', 'ss3_nuts')
mkdir(path)

# PREPARE
cp("boot/initial/model/ss3/*", path)

# FIT
r4ss::run(path, exe="ss_3.30.22.1", show_in_console=TRUE)

# SET starter.ss to use pars

stf <- readLines(file.path(path, "starter.ss"))
linen <- grep("# 0=use init values in control file; 1=use ss.par", stf)
stf[linen] <- paste0("1 # 0=use init values in control file; 1=use ss.par")
write(stf, file.path(path, "starter.ss"))

# GET hbf
r4ss::run(path, exe="ss_3.30.22.1", extras="-nox -hbf 1 -iprint 200 -mcmc 15",
  show_in_console=TRUE, skipfinished=FALSE)

# SETTINGS adnuts

iters <- 500
chains <- 3
burnin <- 0.25
thin <- 10

iter <- ((iters * thin) * (1 + burnin)) / chains
seeds <- round(runif(chains, 122, 869))

# ADD link to executable
system(paste0("ln -s ~/Bin/ss_3.30.22.1 ", file.path(path), "/ss_3.30.22.1"))

# CALL adnuts
system.time(
mcout <- sample_nuts(model="ss_3.30.22.1", path="model/ss3_nuts",
  iter=iter, warmup=iter * 0.10, duration=30,
  seeds=seeds, chains=chains, cores=chains,
  mceval=TRUE, control=list(metric="mle", adapt_delta=0.9, max_treedepth=10, 
  refresh=1))
)

mcsamps <- extract_samples(mcout)

# SAVE
save(mcout, mcsamps, file="model/adnuts.rda", compress="xz")
