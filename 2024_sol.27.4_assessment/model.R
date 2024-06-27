# model.R - DESC
# 2024_sol.27.4_assessment/model.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(TAF)

library(ss3om)
library(ss3diags)

source("utilities.R")

# --- RUN

path <- file.path('model', 'ss3')
mkdir(path)

# PREPARE
cp("boot/initial/model/ss3/*", path)

# FIT
run(path, exe="ss_3.30.22.1", show_in_console=TRUE)

# RUN retro
retro(path, exe="ss_3.30.22.1", show_in_console=TRUE)

# - DIAGNOSTICS

# All parameters are identifiable
adnuts::check_identifiable("ss3", path="model/ss3")

# RUN jitter

mkdir(paste0(path, "_jitter"))
cp(paste0(path, "/*"), paste0(path, "_jitter"))

library(doFuture)
plan(multicore, workers = 3)

jitters <- jitter(dir=paste0(path, "_jitter"), Njitter=50,
  jitter_fraction=0.10, exe="~/Bin/ss_3.30.22.1")

# SAVE
save(jitters, path, file='model/model.rda', compress='xz')

