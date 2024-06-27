---
title: "Stock assessment North sea sole (sol.27.4)"
subtitle: "ICES WGNSSK. 16-25 April 2024"
author: Iago Mosqueira <iago.mosqueira@wur.nl>
institute: Wageningen Marine Research (WMR), IJmuiden, The Netherlands.
fontsize: 10pt
output:
  wmrkdown::wur:
    slide_level: 1
header-includes:
  - \newcommand{\fig}[2][0.70]{\centering\includegraphics[width=#1\textwidth]{report/#2}}
custom: report/images/beamtrawl.jpg
tags: [sol.27.4 FWD]
---

# SS3 2024 assessment

\fig[0.48]{refpts_fbaref.png}
\fig[0.48]{refpts_ssbref.png}

# Selectivity

\fig[0.48]{selex_ss3.png}
\fig[0.48]{catchsel_periods.png}
\fig[0.48]{model_selex.png}

# Forecast assumptions and settings

- Natural mortality as in assessment.
- Maturity: knife-edge at age 3.
- F and M before spawning: 0
- Weights at age catch & stock: 3 year average.
- Selectivity at age: 5 year average.
- Discards ratio at age: 3 year average.

# Intermediate year assumptions

- TAC 2024: 3,675 t
- Fsq = 0.079, C = 5,367 t, 46% overshoot
- Q1 2024 catches NL fleet 27% TAC, BE 17% TAC.
- Catch 2024: 3,675 t
- Recruitment (age 0): Geometric mean 1957-2023 = 1,477,423

\fig[0.40]{table_intyear.png}

# Reference points

\small

| $B_{trigger}$ | $F_{MSY}$  | $B_{lim}$  | $B_{PA}$ | $F_{lim}$ | $F_{PA}$   | lower $F_{MSY}$ | upper $F_{MSY}$ |
|----------|-------|-------|-------|------|-------|-------|-------|
| 52532    | 0.157 | 37804 | 52532 | 0.26 | 0.157 | 0.144 | 0.157 |

# Stock projection

\fig[0.85]{model_fwd.png}

# Catch options

\fig[1]{table_catch.png}

- F 2023 = Fmsy * (SSB 2023 / Btrigger) = 0.207 * 0.90 = 0.188
