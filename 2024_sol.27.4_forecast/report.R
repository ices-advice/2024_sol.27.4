# report.R - DESC
# /report.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(icesTAF)

library(ss3om)
library(ggplotFL)
library(patchwork)

dimnames(refpts)$params[1] <- "Btrigger / Bpa"

# MODEL, ADVICE and FORECAST year

dy <- dims(run)$maxyear
ay <- dy + 1
fy <- ay + 1

# GRAPHICAL elements

box <- annotate("rect", xmin = ay - 0.5, xmax = fy + 1, ymin=-Inf, ymax=Inf,
  fill="lightgrey", alpha = .2)

vline <- geom_vline(xintercept=dy, linetype=1, colour="#464A54", alpha=0.3)

# SELEX

catch.sel(stk)

taf.png("catchsel_periods.png")
ggplot(FLQuants(Y3=yearMeans(catch.sel(stk)[, ac(2021:2023)]),
  Y5=yearMeans(catch.sel(stk)[, ac(2019:2023)])),
  aes(x=age, y=data, colour=qname)) +
  geom_line()
dev.off()

asel <- DT(out$ageselex)[Yr %in% seq(1957, 2026) & Factor == 'Asel',]

asel <- melt(asel, id.vars=c('Fleet', 'Yr'), measure.vars=ac(seq(0, 10)),
  value.name='data', variable.name="age")

taf.png("selex_ss3.png")
ggplot(asel[Fleet %in% c(1,4) & Yr > 2015],
  aes(x=as.numeric(age), y=data, colour=factor(Fleet))) +
  geom_line() + facet_wrap(~Yr) +
  ylab("Selectivity") + xlab("")
dev.off()


inp <- FLQuants(catch.sel=catch.sel(fut)[, ac(2019:2025)],
  landings.sel=landings.sel(fut)[, ac(2019:2025)],
  discards.sel=discards.sel(fut)[, ac(2019:2025)])

taf.png("model_selex.png")
ggplot(inp, aes(x=age, y=data, colour=qname)) +
  geom_line() + facet_wrap(~year) + xlab("age") + ylab("Selectivity")
dev.off()

# FIT

# BASIS

# FWD run

taf.png("model_fwd.png")
(plot(window(ssb(runs$advice), end=fy + 1), probs=probs) +
  geom_flpar(data=refpts[c(1, 3)], x=1961, colour=c("black", "red"),
    linetype=c(3,1)) +
  vline + ylab("SSB (t)") + ylim(c(0, NA))) /
(plot(window(fbar(runs$advice), end=fy), probs=probs) + vline +
  geom_flpar(data=refpts[c(2, 5, 6)], x=1961, colour=c("black", "red", "black"),
    linetype=c(1,1, 3)) +
    vline + ylab("F (2-6)") + ylim(c(0, NA)))
dev.off()

plot(window(catch(runs$advice), end=ay), probs=probs) +
  vline + ylab("Catch (t)") + ylim(c(0, NA))

# PRESENTATION

cp("bootstrap/report/images", "report/")

# render('presentation.md', output_file = 'report/sol.27.4_forecast-WGNSSK2024.pdf')
