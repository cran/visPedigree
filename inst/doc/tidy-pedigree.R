## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 6.5,
  dpi = 96,
  out.width = "100%"
)
library(visPedigree)

## ----gettingdataset,eval=FALSE------------------------------------------------
# data(package="visPedigree")

## ----simpleped----------------------------------------------------------------
head(simple_ped)
tail(simple_ped)
# The number of individuals in the pedigree dataset
nrow(simple_ped)
# Individual records with missing parents
simple_ped[Sire %in% c("0", "*", "NA", NA) |
             Dam %in% c("0", "*", "NA", NA)]

## ----error=TRUE---------------------------------------------------------------
try({
x <- data.table::copy(simple_ped)
x[ID == "J2F588", Sire := "J0Z167"]
y <- tidyped(x)
})

## ----tidyped------------------------------------------------------------------
tidy_simple_ped <- tidyped(simple_ped)
head(tidy_simple_ped)
tail(tidy_simple_ped)
nrow(tidy_simple_ped)

## -----------------------------------------------------------------------------
tidy_simple_ped_no_gen_num <-
  tidyped(simple_ped, addgen = FALSE, addnum = FALSE)
    head(tidy_simple_ped_no_gen_num)

## ----loop_detection, error=TRUE-----------------------------------------------
try({
# loop_ped contains cycles (e.g., V -> T -> R -> P -> M -> V)
# Attempting to tidy it will result in an error
try(tidyped(loop_ped))
})

## ----writeped,eval=FALSE------------------------------------------------------
# saved_ped <- data.table::copy(tidy_simple_ped)
# saved_ped[is.na(Sire), Sire := "0"]
# saved_ped[is.na(Dam), Dam := "0"]
# data.table::fwrite(
#   x = saved_ped,
#   file = tempfile(fileext = ".csv"),
#   sep = ",",
#   quote = FALSE
# )

## -----------------------------------------------------------------------------
tidy_simple_ped_J5X804_ancestors <-
  tidyped(ped = tidy_simple_ped_no_gen_num, cand = "J5X804")
  tail(tidy_simple_ped_J5X804_ancestors)

## -----------------------------------------------------------------------------
tidy_simple_ped_J5X804_ancestors_2 <-
  tidyped(ped = tidy_simple_ped_no_gen_num,
  cand = "J5X804",
  tracegen = 2)
  print(tidy_simple_ped_J5X804_ancestors_2)

## -----------------------------------------------------------------------------
tidy_simple_ped_J0Z990_offspring <-
  tidyped(ped = tidy_simple_ped_no_gen_num, cand = "J0Z990", trace = "down")
  print(tidy_simple_ped_J0Z990_offspring)

## ----intped-------------------------------------------------------------------
tidy_simple_ped_with_int <-
  tidyped(ped = tidy_simple_ped_no_gen_num, addnum = TRUE)
head(tidy_simple_ped_with_int)

## ----inbreed------------------------------------------------------------------
# Create a simple inbred pedigree
library(data.table)
test_ped <- data.table(
  Ind = c("A", "B", "C", "D", "E"),
  Sire = c(NA, NA, "A", "C", "C"),
  Dam = c(NA, NA, "B", "B", "D"),
  Sex = c("male", "female", "male", "female", "male")
)
# Option 1: Calculate during tidying
tidy_test <- tidyped(test_ped, inbreed = TRUE)
head(tidy_test)

# Option 2: Calculate after tidying
tidy_test <- inbreed(tidyped(test_ped))

## ----genmethod----------------------------------------------------------------
# Default behavior (Top-Down): J2Y434 is at Gen 3
tidy_top <- tidyped(simple_ped, genmethod = "top")
tidy_top[Ind == "J2Y434"]

# Bottom-Up behavior: J2Y434 is at Gen 6
tidy_bottom <- tidyped(simple_ped, genmethod = "bottom")
tidy_bottom[Ind == "J2Y434"]

## ----summary------------------------------------------------------------------
# Summarize the tidied pedigree
summary(tidy_simple_ped)

## ----splitped-----------------------------------------------------------------
# Split the pedigree into components
sub_pedigrees <- splitped(tidy_simple_ped)

# View summary of the split result
summary(sub_pedigrees)

# Access a specific sub-pedigree
# first_sub <- sub_pedigrees[[1]]

