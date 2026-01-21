## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(visPedigree)

## ----smallped, fig.width=6.5, fig.height=6.5, fig.show="hold"-----------------
tidy_small_ped <-
  tidyped(ped = small_ped,
          cand = c("Y", "Z1", "Z2"))
visped(tidy_small_ped, compact = TRUE, file = tempfile(fileext = ".pdf"))

## ----vissimpleped,  fig.width=6.5, fig.height=6.5, fig.show="hold"------------
tidy_simple_ped <- tidyped(simple_ped)
visped(tidy_simple_ped)

## -----------------------------------------------------------------------------
suppressMessages(visped(tidy_simple_ped, showgraph = FALSE, file = tempfile(fileext = ".pdf")))

## ----highlight1, fig.width=6.5, fig.height=6.5--------------------------------
visped(tidyped(small_ped), highlight = c("Y", "Z1"))

## ----highlight_trace, fig.width=6.5, fig.height=6.5---------------------------
# Highlight individual "Y" and all its ancestors and descendants
visped(tidyped(small_ped), highlight = "Y", trace = TRUE)

## ----highlight2, fig.width=6.5, fig.height=6.5--------------------------------
visped(tidyped(small_ped), 
       highlight = list(ids = c("Y", "Z1"), 
                        frame.color = "#4caf50", 
                        color = "#81c784"))

## ----showinbreed, fig.width=6.5, fig.height=6.5-------------------------------
library(data.table)
test_ped <- data.table(
  Ind = c("A", "B", "C", "D", "E"),
  Sire = c(NA, NA, "A", "C", "C"),
  Dam = c(NA, NA, "B", "B", "D"),
  Sex = c("male", "female", "male", "female", "male")
)
tidy_test_ped_inbreed <- tidyped(test_ped, inbreed = TRUE)
visped(tidy_test_ped_inbreed, showf = TRUE)

## ----deepped, eval=FALSE------------------------------------------------------
# cand_J11_labels <- deep_ped[(substr(Ind, 1, 3) == "K11"), Ind]
# visped(tidyped(deep_ped, cand = cand_J11_labels, tracegen = 3))

## ----reduceped1, fig.width=6.5, fig.height=6.5--------------------------------
cand_J11_labels <- deep_ped[(substr(Ind,1,3) == "K11"),Ind]
visped(
  tidyped(
    deep_ped,
    cand = cand_J11_labels,
    trace = "up",
    tracegen = 3
  ),
  compact = TRUE,
  showgraph = TRUE,
  file = tempfile(fileext = ".pdf")
)

## ----reduceped2, fig.width=6.5, fig.height=6.5--------------------------------
visped(
  tidyped(
    deep_ped,
    cand = cand_J11_labels,
    trace = "up",
    tracegen = 3
  ),
  compact = TRUE,
  cex = 0.83,
  showgraph = FALSE,
  file = tempfile(fileext = ".pdf")
)

## ----reduceped3, fig.width=6.5, fig.height=6.5--------------------------------
suppressMessages(visped(
  tidyped(
    deep_ped,
    cand = cand_J11_labels, 
    tracegen = 3),
  compact = TRUE,
  outline = TRUE,
  showgraph = TRUE,
  file = tempfile(fileext = ".pdf")
))

## ----pedofoneind, fig.width=6.5, fig.height=6.5-------------------------------
suppressWarnings(J110550G_ped <-
                   tidyped(deep_ped, cand = "K110550H"))
suppressMessages(visped(J110550G_ped, showgraph = TRUE, file = tempfile(fileext = ".pdf")))

## ----optiMate, fig.width=6.5, fig.height=6.5----------------------------------
cand_2007_G8_labels <-
  big_family_size_ped[(Year == 2007) & (substr(Ind, 1, 2) == "G8"), Ind]
suppressWarnings(
  cand_2007_G8_tidy_ped_ancestor_2 <-
    tidyped(
      big_family_size_ped,
      cand = cand_2007_G8_labels,
      trace = "up",
      tracegen = 2
    )
)
sire_label <-
  unique(cand_2007_G8_tidy_ped_ancestor_2[Ind %in% cand_2007_G8_labels,
                                          Sire])
dam_label <-
  unique(cand_2007_G8_tidy_ped_ancestor_2[Ind %in% cand_2007_G8_labels,
                                          Dam])
sire_dam_label <- unique(c(sire_label, dam_label))
sire_dam_label <- sire_dam_label[!is.na(sire_dam_label)]
sire_dam_ped <-
  cand_2007_G8_tidy_ped_ancestor_2[Ind %in% sire_dam_label]
sire_dam_ped <-
  sire_dam_ped[, FamilyID := paste(Sire, Dam, sep = "")]
family_size <- sire_dam_ped[, .N, by = c("FamilyID")]
fullsib_family_label <- unique(sire_dam_ped$FamilyID)
suppressMessages(
  visped(
    cand_2007_G8_tidy_ped_ancestor_2,
    compact = TRUE,
    outline = TRUE,
    showgraph = TRUE
  )
)

