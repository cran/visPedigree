## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 6,
  dpi = 96,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(visPedigree)
library(data.table)

data(simple_ped, package = "visPedigree")

## ----tidy-once----------------------------------------------------------------
tp_master <- tidyped(simple_ped)

class(tp_master)
is_tidyped(tp_master)
pedmeta(tp_master)

## ----fast-trace---------------------------------------------------------------
tp_up <- tidyped(tp_master, cand = "J5X804", trace = "up", tracegen = 2)
tp_down <- tidyped(tp_master, cand = "J0Z990", trace = "down")

has_candidates(tp_up)
tp_up[, .(Ind, Sire, Dam, Cand)]

## ----fast-trace-pattern, eval = FALSE-----------------------------------------
# # expensive once
# # tp_master <- tidyped(raw_ped)
# 
# # cheap many times
# # tp_a <- tidyped(tp_master, cand = ids_a, trace = "up")
# # tp_b <- tidyped(tp_master, cand = ids_b, trace = "all", tracegen = 3)
# # tp_c <- tidyped(tp_master, cand = ids_c, trace = "down")

## ----dt-modify----------------------------------------------------------------
tp_work <- copy(tp_master)
tp_work[, phenotype := seq_len(.N)]

class(tp_work)
head(tp_work[, .(Ind, phenotype)])

## ----incomplete-subset--------------------------------------------------------
ped_year <- data.table(
  Ind = c("A", "B", "C", "D"),
  Sire = c(NA, NA, "A", "C"),
  Dam = c(NA, NA, "B", "B"),
  Year = c(2000, 2000, 2005, 2006)
)

tp_year <- tidyped(ped_year)
sub_dt <- tp_year[Year > 2005]

class(sub_dt)
sub_dt

## ----incomplete-subset-error, error = TRUE------------------------------------
try({
inbreed(sub_dt)
})

## ----explicit-tracing---------------------------------------------------------
valid_sub_tp <- tidyped(tp_year, cand = "D", trace = "up")

class(valid_sub_tp)
valid_sub_tp[, .(Ind, Sire, Dam, Cand)]

## ----explicit-analysis--------------------------------------------------------
inbreed(valid_sub_tp)[Ind == "D", .(Ind, f)]

## ----split-vs-summary---------------------------------------------------------
sub_tps <- splitped(tp_master)
length(sub_tps)
class(sub_tps[[1]])

pedsubpop(tp_master)

## ----accessors----------------------------------------------------------------
tp_f <- inbreed(tp_master)

is_tidyped(tp_f)
has_inbreeding(tp_f)
has_candidates(tp_f)
pedmeta(tp_f)

## ----best-practice, eval = FALSE----------------------------------------------
# # 1. build one validated master object
# # tp_master <- tidyped(raw_ped)
# 
# # 2. add analysis-specific columns in place
# # tp_master[, phenotype := pheno_vector]
# # tp_master[, cohort := year_vector]
# 
# # 3. extract valid candidate sub-pedigrees explicitly
# # tp_sel <- tidyped(tp_master, cand = selected_ids, trace = "up", tracegen = 3)
# 
# # 4. run downstream analysis on either the full master or traced sub-pedigree
# # pedstats(tp_master)
# # pedmat(tp_sel)
# # inbreed(tp_sel)
# # visped(tp_sel)
# 
# # 5. split only when disconnected components really matter
# # comps <- splitped(tp_master)

