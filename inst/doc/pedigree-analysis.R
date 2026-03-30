## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(visPedigree)
library(data.table)

data(deep_ped, package = "visPedigree")
data(big_family_size_ped, package = "visPedigree")
data(small_ped, package = "visPedigree")
data(inbred_ped, package = "visPedigree")

tp_deep <- tidyped(deep_ped)
tp_small <- tidyped(small_ped)
tp_inbred <- tidyped(inbred_ped)

## ----pedstats-overview--------------------------------------------------------
stats_deep <- pedstats(tp_deep)

stats_deep$summary
tail(stats_deep$ecg)
stats_deep$gen_intervals

# Visualize ancestral depth (Equivalent Complete Generations)
plot(stats_deep, type = "ecg", metric = "ECG")

## ----pedecg-example-----------------------------------------------------------
ecg_deep <- pedecg(tp_deep)

ecg_deep[order(-ECG)][1:10]

## ----pedgenint-basic----------------------------------------------------------
tp_time <- tidyped(big_family_size_ped)

genint_year <- pedgenint(tp_time, timevar = "Year", unit = "year")
genint_year

# Visualize generation intervals
# Note: we can also use stats <- pedstats(tp_time, timevar = "Year") followed by plot(stats)
plot(genint_year)

## ----pedgenint-cycle----------------------------------------------------------
genint_cycle <- pedgenint(tp_time, timevar = "Year", unit = "year", cycle = 1.2)

genint_cycle[Pathway %in% c("SS", "SD", "DS", "DD", "Average")]

## ----pedsubpop-example--------------------------------------------------------
ped_demo <- data.table(
  Ind = c("A", "B", "C", "D", "E", "F", "G", "H"),
  Sire = c(NA, NA, "A", NA, NA, "E", NA, NA),
  Dam  = c(NA, NA, "B", NA, NA, NA, NA, NA),
  Sex = c("male", "female", "male", "female", "male", "female", "male", "female"),
  Batch = c("L1", "L1", "L1", "L1", "L2", "L2", "L3", "L3")
)

tp_demo <- tidyped(ped_demo)

pedsubpop(tp_demo)
pedsubpop(tp_demo, by = "Batch")

## ----pediv-setup--------------------------------------------------------------
ref_pop <- tp_deep[Gen >= max(Gen) - 1, Ind]
length(ref_pop)

## ----pediv-analysis-----------------------------------------------------------
div_res <- pediv(tp_deep, reference = ref_pop, top = 10, seed = 42L)

div_res$summary
div_res$ancestors

## ----pediv-genediv------------------------------------------------------------
# GeneDiv is in the summary alongside MeanCoan
div_res$summary[, .(fg, MeanCoan, GeneDiv)]

## ----pediv-shannon------------------------------------------------------------
# Shannon metrics are included in pediv() output
div_res$summary[, .(NFounder, feH, fe, NAncestor, faH, fa)]

# The ratio feH/fe reveals long-tail founder diversity
div_res$summary[, .(rho_founder = feH / fe, rho_ancestor = faH / fa)]

## ----pedcontrib-shannon-------------------------------------------------------
# pedcontrib() provides the same metrics via its summary
contrib_res <- pedcontrib(tp_deep, reference = ref_pop, mode = "both")
contrib_res$summary[c("n_founder", "f_e_H", "f_e", "n_ancestor", "f_a_H", "f_a")]

## ----pedhalflife-analysis-----------------------------------------------------
hl <- pedhalflife(tp_deep, timevar = "Gen")
print(hl)

## ----pedhalflife-timeseries---------------------------------------------------
hl$timeseries

## ----pedhalflife-subset-------------------------------------------------------
hl_recent <- pedhalflife(tp_deep, timevar = "Gen",
                         at = tail(sort(unique(tp_deep$Gen)), 4))
print(hl_recent)

## ----pedhalflife-plot, fig.width=6, fig.height=4------------------------------
plot(hl, type = "log")

## ----pedhalflife-plot-raw, fig.width=6, fig.height=4--------------------------
plot(hl, type = "raw")

## ----pedrel-example-----------------------------------------------------------
tp_small$BirthYear <- 2010 + tp_small$Gen

rel_by_gen <- pedrel(tp_small, by = "Gen")
rel_by_gen

## ----pedrel-reference---------------------------------------------------------
ref_ids <- c("Z1", "Z2", "X", "Y")

pedrel(tp_small, by = "Gen", reference = ref_ids)

## ----pedrel-coancestry--------------------------------------------------------
coan_by_gen <- pedrel(tp_small, by = "Gen", scale = "coancestry")
coan_by_gen

## ----inbreed-trend------------------------------------------------------------
tp_inbred_f <- inbreed(tp_inbred)

f_trend <- tp_inbred_f[, .(MeanF = mean(f, na.rm = TRUE)), by = Gen]
f_trend

## ----pedfclass-example--------------------------------------------------------
pedfclass(tp_inbred)

## ----pedfclass-breaks---------------------------------------------------------
pedfclass(tp_inbred, breaks = c(0.03125, 0.0625, 0.125, 0.25))

## ----pedancestry-example------------------------------------------------------
ped_line <- data.table(
  Ind = c("A", "B", "C", "D", "E", "F", "G"),
  Sire = c(NA, NA, NA, NA, "A", "C", "E"),
  Dam  = c(NA, NA, NA, NA, "B", "D", "F"),
  Sex = c("male", "female", "male", "female", "male", "female", "male"),
  Line = c("Line1", "Line1", "Line2", "Line2", NA, NA, NA)
)

tp_line <- tidyped(ped_line)

anc <- pedancestry(tp_line, foundervar = "Line")
anc

## ----pedpartial-example-------------------------------------------------------
partial_f <- pedpartial(tp_inbred, ancestors = c("A", "B"))
partial_f

