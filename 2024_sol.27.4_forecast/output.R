# output.R - DESC
# 2022_sol.27.4_forecast/output.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("output")

library(ss3om)
library(icesAdvice)
library(data.table)

source("utilities.R")

# LOAD assessment and forecast results

load('data/refpts_WKBFLAT1_2024.rda')
load('data/output.rda')
load("model/model.rda")

# MODEL, ADVICE and FORECAST year

dy <- dims(stk)$maxyear
iy <- dy + 1
ay <- iy + 1
fy <- ay + 1

# --- Advice sheet

mkdir("output/tables")

# - SAG graphs

source("output_sag.R")

# UPLOAD 
options(icesSAG.use_token = TRUE)
key <- icesSAG::uploadStock(info, fishdata)

# CHECK SAG
findAssessmentKey('sol.27.4', year=iy, published = TRUE)

#  - TABLE 1, Intermediate year assumptions

tab1 <- interimTable(runs$Fmsy)

save_as_docx(autofit(flextable(tab1)),
  path="output/tables/intermediate_assumptions.docx")

save_as_image(x = autofit(flextable(tab1)),
  path = "report/table_intyear.png")

# - TABLE 2, Annual catch scenarios

tab2 <- catchOptionsTable(runs, advice=tac,
  ages=c(2,6), discards.ages=c(1,3))

catchops <- flextableCatchOptions(tab2)

save_as_docx(fontsize(catchops, part='all', size=10),
  path="output/tables/catch_options.docx")

save_as_image(x = catchops,
  path = "report/table_catch.png")

save(catchops, file="output/forecast_tables.rda", compress="xz")

# - TABLE 9, Summary of the assessment

tab9 <- data.table(fishdata[, c("Year",
  "Low_Recruitment", "Recruitment",  "High_Recruitment",
  "Low_StockSize", "StockSize", "High_StockSize",
  "Landings", "Discards", 
  "Low_FishingPressure", "FishingPressure", "High_FishingPressure")])

# MERGE observed and estimnated discard
tab9[Year %in% 1957:2001, Discards:=fishdata[fishdata$Year %in% 1957:2001,]$CustomSeries1]

tab9[, Year:=as.character(Year)]

# ROUND columns
fcols <- c("FishingPressure", "High_FishingPressure", "Low_FishingPressure")
tab9[ , (fcols) := lapply(.SD, icesRound), .SDcols = fcols]

nfcols <- colnames(tab9)[!colnames(tab9) %in% fcols][-1]
tab9[ , (nfcols) := lapply(.SD, round), .SDcols = nfcols]

save_as_docx(fontsize(flextable(tab9), part='all', size=10),
  path="output/tables/model_outputs.docx")

# - TABLE corrections

fqs <- model.frame(FLQuants(
  'AAP~2022~'=stock.n(runs[[1]])[, '2022'],
  'AAP~2023~'=survivors(runs[[1]][, '2022'], rec=rec1gm)[,'2023'],
  'Corrected~2023~'=stock.n(runs[[1]])[,'2023'],
  'Forecast(%TAC)~2024~'=stock.n(runs[[1]])[,'2024']))

ssbrow <- c(ssb(runs[[1]])[, ac(2022)],
  quantSums(survivors(runs[[1]][, '2022'], rec=rec1gm)[,'2023'] *
  stock.wt(runs[[1]])[,'2023'] * mat(runs[[1]])[,'2023']),
  ssb(runs[[1]])[, ac(2023:2024)])

tab0 <- rbind(round(model.frame(fqs)[, c(1, 7:10)]),
  c('SSB', round(ssbrow)))

tab0 <- autofit(flextable(tab0))
tab0 <- vline(tab0, j = c(1,2,4), border = NULL, part = "all")
save_as_docx(fontsize(tab0, part='all', size=10),
  path="output/tables/correction.docx")

save_as_image(fontsize(tab0, part='all', size=10),
  path="report/correction.png")


# --- FRUNS

tabfruns <- lapply(fruns, function(x) metrics(window(x, start=dy, end=fy),
  list(Fbar=fbar, Landings=landings, Discards=discards, Catch=catch, SSB=ssb)))

names(tabfruns) <- paste0("Fmult(", fy, ")=", names(tabfruns))

tabfruns <- lapply(tabfruns, as.data.frame, drop=TRUE)

tab <- rbindlist(tabfruns, idcol="Rationale")
tab <- dcast(tab, Rationale ~ qname + year, value.var = "data")

# years
tab[ ,`:=`(Fbar_2022 = NULL, Landings_2022 = NULL, Discards_2022 = NULL, Catch_2022 = NULL)]

setcolorder(tab, c(1,2,4,6,8,10,3,5,7,9,11,12))

tab[, SSBchange_2024:= (SSB_2025 - SSB_2024) / SSB_2024 * 100]

fwrite(tab, file="output/mutliF_options_sol.27.4_2020.csv")


# SAVE for next year
# save(run, runs, stock, fit, refpts, file="output/aap.rda", compress="xz")


# --- WGMIXFISH
# TODO: RECONSTRUCT catch.n
sol.27.4_WGNSSK_2023 <- FLStocks(input=stock + fit, estimated=fit + stock)

save(sol.27.4_WGNSSK_2023, file="output/sol.27.4_2023_WGMIXFISH.rda",
  compress="xz")
