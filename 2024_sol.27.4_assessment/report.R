# report.R - Prepare plots and tables for report
# 2024_sol.27.4_benchmark_assessment/report.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(TAF)
mkdir("report")

library(ggplotFL)
library(patchwork)
library(ggrepel)

source("utilities.R")

# -- data {{{

load("data/data.rda")

stats[, other:=as.integer(other)]

taf.png("official_landings.png")
ggplot(melt(official[, .(year, BE, DK, FR, DE, NL, UK, other)], id.vars="year",
  variable.name="country", value.name="data", verbose=FALSE),
  aes(x=year, y=data)) +
  geom_col(aes(fill=country)) + ylab("Landings (t)") + xlab("") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.title = element_blank())
dev.off()

# Catches and TACs
taf.png("official_catch.png")
ggplot(melt(official[, .(year, official, ices, tac)], id.vars="year",
  variable.name="category", value.name="data", verbose=FALSE), aes(x=year, y=data)) +
  geom_line(aes(colour=category), size=1) +
  ylab("Catch (t)") + xlab("") +
  guides(colour=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.title = element_blank()) +
  ylim(c(0,NA))
dev.off()


load("output/output.rda")

taf.png("data_catch.png")
plot(mets$Catch) + ylab("Yield (t)") +
  theme(legend.position='bottom') +
  labs(fill='', colour='')
dev.off()
  
taf.png("data_catchnts.png")
plot(catch.n(stk)[-1]) + ylab("Catch (t)")
dev.off()
 
# Time-series of catch at age

taf.png("data_catchnb.png")
ggplot(catch.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Catch (", units(catch.n(stock)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# Time-series of landings at age

taf.png("data_landingsn.png")
ggplot(landings.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Landings (", units(catch.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# Time-series of discards at age

taf.png("data_discardsn.png")
ggplot(window(discards.n(stk), start=2000),
  aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Discards (", units(discards.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# Time-series of discard proportion at age

taf.png("data_discardsp.png")
ggplot(window(discards.n(stk), start=2002) /
  window(catch.n(stk), start=2002),
  aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data), fill=data), shape=21, na.rm=TRUE) +
  scale_size_continuous(range = c(0.1, 8), name="p") +
  scale_fill_gradient(low = "white", high = "gray50", name="p", guide = "legend") +
  ylab(paste0("Proportion discarded")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# Discards ratio

taf.png("data_discardratio.png")
ggplot(window(discards.n(stock) / catch.n(stock), start=2000),
  aes(x=year, y=data, colour=factor(age))) + geom_line() +
  geom_label_repel(data=as.data.frame((discards.n(stock) / catch.n(stock))[, ac(dy)]),
    aes(label=age), colour="black") +
  guides(colour=FALSE) + ylab("Discard ratio (discards/catch)") +
  xlab("") + ylab("")
dev.off()

# Time series of catch by cohort

taf.png("data_catchcoh.png")
ggplot(as.data.frame(FLCohort(catch.n(stock))), aes(x=cohort, y=data, group=age)) +
  geom_line(aes(colour=factor(age))) +
  xlab("Cohort") + ylab("Catch (thousands)") +
  theme(legend.position="none")
dev.off()

taf.png("data_catchcoh2.png")
ggplot(as.data.frame(FLCohort(catch.n(stock))), aes(x=cohort, y=data, group=age)) +
  geom_line(aes(colour=factor(age))) +
  facet_grid(age~., scales="free") +
  xlab("") + ylab("Catch (thousands)") +
  theme(legend.position="none")
dev.off()




# WAA

taf.png("data_stockwt.png")
ggplot(stock.wt(stk), aes(x=year, y=data * 1000, group=age, colour=factor(age))) +
  geom_line() + ylab("Weight-at-age (g)") + xlab("") +
  geom_smooth(se=FALSE, linewidth=0.5, alpha=0.2) +
  geom_text_repel(data=as.data.frame(stock.wt(stk)[, ac(1957)]),
    aes(x=year - 1, label=age), colour="black") +
  geom_text_repel(data=as.data.frame(stock.wt(stk)[, ac(2023)]),
    aes(x=year + 1, label=age), colour="black") +
  theme(legend.position="none") +
  geom_vline(xintercept=2014, alpha=0.5) +
  ylim(0, 800) +
  ggtitle("Stock weight-at-age (Q2)")
dev.off()

taf.png("data_catchwt.png")
ggplot(catch.wt(stk), aes(x=year, y=data * 1000, group=age, colour=factor(age))) +
  geom_line() + ylab("Weight-at-age (g)") + xlab("") +
  geom_smooth(se=FALSE, linewidth=0.5, alpha=0.2) +
  geom_text_repel(data=as.data.frame(catch.wt(stk)[, ac(1957)]),
    aes(x=year - 1, label=age), colour="black") +
  geom_text_repel(data=as.data.frame(catch.wt(stk)[, ac(2023)]),
    aes(x=year + 1, label=age), colour="black") +
  theme(legend.position="none") +
  geom_vline(xintercept=2014, alpha=0.5) +
  ylim(0, 800) +
  ggtitle("Catch weight-at-age")
dev.off()

taf.png("data_landingswt.png")
ggplot(landings.wt(stk), aes(x=year, y=data * 1000, group=age,
  colour=factor(age))) +
  geom_line() + ylab("Weight-at-age (g)") + xlab("") +
  geom_smooth(se=FALSE, linewidth=0.5, alpha=0.2) +
  geom_text_repel(data=as.data.frame(landings.wt(stk)[, ac(1957)]),
    aes(x=year - 1, label=age), colour="black") +
  geom_text_repel(data=as.data.frame(landings.wt(stk)[, ac(2023)]),
    aes(x=year + 1, label=age), colour="black") +
  theme(legend.position="none") +
  geom_vline(xintercept=2014, alpha=0.5) +
  ylim(0, 800) +
  ggtitle("Landings weight-at-age")
dev.off()

taf.png("data_discardswt.png")
ggplot(discards.wt(stk), aes(x=year, y=data * 1000, group=age,
  colour=factor(age))) +
  geom_line() + ylab("Weight-at-age (g)") + xlab("") +
#  geom_smooth(se=FALSE, linewidth=0.5, alpha=0.2) +
  geom_text_repel(data=as.data.frame(discards.wt(stk)[, ac(1957)]),
    aes(x=year - 1, label=age), colour="black") +
  geom_text_repel(data=as.data.frame(discards.wt(stk)[, ac(2023)]),
    aes(x=year + 1, label=age), colour="black") +
  theme(legend.position="none") +
  geom_vline(xintercept=2014, alpha=0.5)
dev.off()

# Catch COHCORR
taf.png("data_catchn_corr.png")
cohcorrplot(catch.n(stk)[-1])
dev.off()


dat <- melt(data.table(out$wtatage), measure.vars=ac(seq(0, 10)),
  value.name="data", variable.name="age")

ggplot(dat, aes(x=Yr, y=data, colour=age)) +
  geom_line() +
  facet_wrap(~Fleet)

# indices
# load("data/indices.rda")

# }}}

# -- output {{{

load("output/output.rda")

load('boot/initial/data/refpts_WKBFLAT1_2024.rda')

library(r4ss)
library(ss3diags)

mkdir("report/ss3")

SS_plots(out, uncertainty=T, png=T, forecastplot=TRUE, fitrange = TRUE, 
  parrows=5, parcols=4, showdev=FALSE, html = FALSE,
  printfolder = "../../report/ss3")

# OSA
taf.png("osa_fleet.png")
plot(osa[[1]], 1, main='Fleet')
dev.off()

taf.png("osa_bts.png")
plot(osa[[2]], 1, main='BTS')
dev.off()

taf.png("osa_coast.png")
plot(osa[[3]], 1, main='Coast')
dev.off()

taf.png("osa_discards.png")
plot(osa[[4]], 1, main='Discards')
dev.off()

# RMSE
SSplotJABBAres(out, subplots="cpue", print=TRUE,
  plotdir="report/", filenameprefix = "cpue_")

SSplotJABBAres(out, subplots="age", print=TRUE,
  plotdir="report/", filenameprefix = "age_")

# RETRO
# TODO: ADD Rec
taf.png("retro.png")
sspar(mfrow=c(3,1), plot.cex = 0.65)
SSplotRetro(retroSummary, add=T, subplots="SSB", forecastrho=FALSE)
SSplotRetro(retroSummary, add=T, subplots="F", forecastrho=FALSE)
SSplotRetro(retroSummary, add=T, subplots="Rec", forecastrho=FALSE)
dev.off()

mkdir("report/retro")
SSplotComparisons(retroSummary, endyrvec=retroSummary$endyrs + 0:-5,
  legendlabels=paste("Data",0:-5,"years"), png = TRUE,
  plotdir=file.path("report", "retro"), uncertainty = T)


# JITTERS

# XVAL
SSplotHCxval(retroSummary, print=TRUE, plotdir="report/")

# RUNS TEST
SSplotRunstest(out, print=TRUE, plotdir="report/")

SSplotRunstest(out, subplots="age", print=TRUE, plotdir="report/")

# stock
taf.png("stock.png")
plot(mets[c("Catch", "Rec", "SSB", "F")]) +
  ylim(0, NA)
dev.off()

taf.png("stock_mcmc.png")
plot(mcmets) +
  ylim(0, NA)
dev.off()


# proportion SSB at age

fill <- c("#21313E","#214C57","#1F6969","#2A8674","#4AA377","#78BF73",
  "#AFD96C","#EFEE69")
msb <- (stock.n(stk) * stock.wt(stk) * mat(stk))[-c(1,2,3),]

taf.png("model_propssb.png")
ggplot(msb %/% quantSums(msb), aes(x=year, y=data, fill=as.factor(age))) +
  geom_bar(stat="identity", width=1) + scale_fill_manual(values=fill) +
  xlab("") + ylab("Proportion of SSB") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="right") +
  guides(fill = guide_legend(nrow = 8))
dev.off()

fill <- viridis::viridis(11)
msb <- (stock.n(stk) * stock.wt(stk))

taf.png("model_propbiom.png")
ggplot(msb %/% quantSums(msb), aes(x=year, y=data, fill=as.factor(age))) +
  geom_bar(stat="identity", width=1) + scale_fill_manual(values=fill) +
  xlab("") + ylab("Proportion of SSB") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title=element_blank(), legend.position="right") +
  guides(fill = guide_legend(nrow = 8))
dev.off()

# --- 

dat <- r4ss::SS_readdat('model/ss3/sol274.dat')

head(out$catage)



# TSB and SSB

taf.png("ssbtsb.png")
ggplot(metrics(stk, list(TSB=tsb, SSB=ssb)), aes(x=year, y=data, colour=qname)) +
  geom_line(size=1) + xlab("") + ylab("Biomass (t)") +
  theme(legend.title=element_blank())
dev.off()

taf.png("vulbiom.png")
ggplot(metrics(stk, TSB=tsb, SSB=ssb, VB=vb),
  aes(x=year, y=data, group=qname, colour=qname)) +
  geom_line() + xlab("") + ylab("Biomass (t)") +
  theme(legend.title=element_blank())
dev.off()

# Fs

taf.png("model_fatage.png")
ggplot(harvest(stk), aes(x=year, y=data, colour=factor(age))) +
  geom_line() + facet_wrap(~age) + xlab("") + ylab("F") +
  theme(legend.position="none")
dev.off()

# Productivity

plot(log(rec(run)[,-1] / ssb(run)[,-dims(run)$year])) +
  geom_point(size=3, colour='white') +
  geom_point(size=2, shape=1) +
  ylab("log(recruits / SSB)")


dat <- data.frame(year=1990:2023,
  cv=data.table(out$derived_quants)[108:141, StdDev / Value])

# REC CVs
taf.png("model_reccv.png")
ggplot(dat, aes(x=year, y=cv)) +
  geom_point() +
  geom_line() +
  xlab("") + ylab("CV(R)") + ylim(0,NA)
dev.off()

# F vs. reference points
taf.png("refpts_fbaref.png")
plot(mets$F) +
  geom_hline(yintercept=c(refpts$Flim), color="red", linewidth=0.25)+
    geom_text(x=2022+1, y=c(refpts$Flim + 0.01), label=expression(F[lim])) +
    geom_hline(yintercept=c(refpts$Fpa), linetype=3) +
    geom_text(x=2022+1, y=c(refpts$Fpa + 0.01), label=expression(F[PA])) +
  geom_hline(yintercept=c(refpts$Fmsy), linetype=2) +
    geom_text(x=2022+1, y=c(refpts$Fmsy - 0.01), label=expression(F[MSY])) +
  ylim(c(0,NA)) + ylab("F (ages 2-6)")
dev.off()

# SSB vs. reference points
taf.png("refpts_ssbref.png")
plot(mets$SSB) +
  geom_hline(yintercept=c(refpts$Bpa), color="red", linewidth=0.25) +
    geom_text(x=1957, y=c(refpts$Bpa - 3500), label=expression(B[PA]), hjust="inward") +
  geom_hline(yintercept=c(refpts$Btrigger), linetype=3) +
    geom_text(x=1957, y=c(refpts$Btrigger + 3500), 
      label=expression(MSYB[trigger]), hjust="inward") +
  geom_hline(yintercept=c(refpts$Blim), linetype=2) +
    geom_text(x=1957, y=c(refpts$Blim + 3500), label=expression(B[lim]),
      hjust="inward") +
  ylim(c(0,NA)) + ylab("SSB (tonnes)")
dev.off()

# SRR
taf.png("srr.png")
plot(srr)
dev.off()

# SELEX
taf.png("catch_selex.png")
ggplot(catch.sel(stk)[, ac(2012:2023)],
  aes(x=age, y=data)) +
  facet_wrap(~year) +
  geom_line(aes(colour=factor(year))) +
  theme(legend.position='none') +
  ylab("Selectivity") + xlab("Age")
dev.off()

# - COMPARE benchmark

load("boot/initial/data/benchmark/reference.rda")

ref_mets <- extractMetrics(reference$out)

catch(reference$stk)[] <- ref_mets$Catch[,,'Catch']

taf.png("wgvsbench.png")
plot(FLStocks(Benchmark=reference$stk, WGNSSK=stk))
dev.off()

# - COMPARE AAP

aap <- mget(load('boot/initial/data/aap.rda'))

# BUG:
catch(stk)[] <- mets$Catch[,,'Catch']

taf.png("ss3vsaap.png")
plot(FLStocks(AAP=aap$run, SS3=stk),
  metrics=list('Rec[age==1]'=function(x) stock.n(x)['1',],
  SSB=ssb, Catch=catch, F=fbar))
dev.off()

# INDICES

# }}}

# RENDER

render("presentation.Rmd", output_dir="report", output_file="sol.27.4_assessment-WGNSSK.pdf")

# render("report.Rmd", output_dir="report", output_file="WD_sol.27.4_refpts-WKBFLAT1.pdf")
