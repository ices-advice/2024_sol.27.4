# data.R - DESC
# /home/mosqu003/Active/MEETING_WGNSSK_2024-LWT/2024_sol.27.4_assessment/data.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("data")

# OFFICIAL catches

official <- fread('boot/initial/data/official.dat')

# SAVE
save(official, file='data/data.rda', compress='xz')
