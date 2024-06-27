# output_sag.R - DESC
# 2020_sol.27.4_forecast/output_sag.R

# Copyright Iago MOSQUEIRA (WMR), 2023
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesSAG)
library(FLasher)

# https://standardgraphs.ices.dk/manage/ViewGraphsAndTables.aspx?key=14120

# --- SAG

# FLStock w/o updated landings/discards.n/wt
sagrun <- stk

# INFO {{{
info <- stockInfo(StockCode="sol.27.4", AssessmentYear=iy,
  Purpose = "Advice", StockCategory=1, ContactPerson="iago.mosqueira@wur.nl",
  
  # A: Age based (like VPA, SCA, SAM) 
  ModelType="A", ModelName="SS3",
  
  # rec
  RecruitmentDescription="Recruitment", RecruitmentAge=0,
  # NE3: Number of individuals in thousands (x1000) (fisheries) 
  RecruitmentUnits="NE3",
  
  # ssb
  StockSizeDescription="Spawning Stock Biomass",
  StockSizeUnits="t",
  
  # catch
  CatchesLandingsUnits="t",
  # F
  FishingPressureDescription="F",
  FishingPressureUnits="",
  # CIs
  ConfidenceIntervalDefinition="95%",

  # REFPTS

  FMSY=c(refpts$Fmsy),
  MSYBtrigger=c(refpts$Btrigger),
  
  Blim=c(refpts$Blim),
  Flim=c(refpts$Flim),
  
  Bpa=c(refpts$Bpa),
  Fpa=c(refpts$Fpa),
  
  # MAP
  FMGT=c(refpts$Fmsy),
  FMGT_lower=c(refpts$lFmsy),
  FMGT_upper=c(refpts$uFmsy),
  BMGT=c(refpts$Btrigger)
)
# }}}

# DEBUG
info$StockCategory <- 1

# DATA
fishdata <- stockFishdata(seq(1957, iy))

# LAST year
n <- length(fishdata$Year)
# YEARS 1:n
i <- seq(n - 1)

# Recruitment
fishdata$Recruitment[i] <- c(mean(metsci$Rec))
fishdata$Recruitment[n] <- c(iter(rec(fut)[, ac(iy)], 1))
fishdata$Low_Recruitment[i] <- c(lowq(metsci$Rec))
fishdata$High_Recruitment[i] <- c(uppq(metsci$Rec))

# TBiomass
fishdata$TBiomass[i] <- c(stock(stk))

# StockSize: ssb(fit)
fishdata$StockSize[i] <- c(mean(metsci$SSB))
fishdata$Low_StockSize[i] <- c(lowq(metsci$SSB))
fishdata$High_StockSize[i] <- c(uppq(metsci$SSB))

# StockSize iy: sagfut
fishdata$StockSize[n] <- c(ssb(fut)[, ac(iy)])

# Landings (obs)
fishdata$Landings[i] <- c(mets$Landings)

# Discards (model + obs)
# Observed discards
fishdata$Discards[i] <- c(mets$Discards)

# Catches (obs)
fishdata$Catches[i] <- c(mets$Catch[,,'Catch'])

# FishingPressure
fishdata$FishingPressure[i] <- c(mean(metsci$F))
fishdata$Low_FishingPressure[i] <- c(lowq(metsci$F))
fishdata$High_FishingPressure[i] <- c(uppq(metsci$F))
# TODO: fishdata$FishingPressure_Landings[i] <- c(Fwanted(fit))
# TODO: fishdata$FishingPressure_Discards[i] <- c(Funwanted(fit))

# XML

xmlfile <- createSAGxml(info, fishdata)

# SAVE to file

capture.output(cat(xmlfile, quote=FALSE), file="output/sol_27_4.xml")
