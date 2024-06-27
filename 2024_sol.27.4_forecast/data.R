# data.R - Preprocess data, write TAF data tables
# 2020_sol.27.4_forecast/data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# INPUT: bootstrap/initial/data
# OUPUT: data/data.RData (FLR), data/*.csv (TAF tables)


library(icesTAF)

mkdir("data")

cp("boot/data/*", "data/")
