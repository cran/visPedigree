## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 6.5,
  dpi = 300,
  out.width = "100%"
)
library(visPedigree)

## ----smallped, fig.show="hold"------------------------------------------------
tidy_small_ped <-
  tidyped(ped = small_ped,
          cand = c("Y", "Z1", "Z2"))
visped(tidy_small_ped, 
        compact = TRUE, 
        cex=0.5, 
        symbolsize=10, 
        file = tempfile(fileext = ".pdf"))

## ----symbol-schemes, fig.show="hold"------------------------------------------
visped(tidy_small_ped, 
      compact = TRUE,
      cex=0.5, 
      symbolsize=10)
visped(tidy_small_ped, 
        compact = TRUE, 
        shapeby = "role",
        cex=0.5, 
        symbolsize=10)

## ----vissimpleped, fig.show="hold"--------------------------------------------
tidy_simple_ped <- tidyped(simple_ped)
visped(tidy_simple_ped, cex=0.3, symbolsize=10)

## ----vissimpleped_genlab, fig.show="hold"-------------------------------------
visped(tidy_simple_ped, cex = 0.3, symbolsize = 10, genlab = TRUE)

## ----vissimpleped_custom_genlab, fig.show="hold"------------------------------
generation_labels <- paste0("Gen_", sort(unique(tidy_simple_ped$Gen)))
visped(tidy_simple_ped, cex = 0.3, symbolsize = 10, genlab = generation_labels, genlabcex=0.6)

## ----vissimpleped_genlabcex, fig.show="hold"----------------------------------
# cex controls individual label size; genlabcex controls generation label size
visped(tidy_simple_ped, cex = 0.3, symbolsize = 15, genlab = TRUE, genlabcex = 0.8)

## -----------------------------------------------------------------------------
suppressMessages(visped(tidy_simple_ped, showgraph = FALSE, file = tempfile(fileext = ".pdf")))

## -----------------------------------------------------------------------------
suppressMessages(visped(tidy_simple_ped, showgraph = FALSE, file = tempfile(fileext = ".svg")))

## ----custom-labels, fig.show="hold"-------------------------------------------
tidy_simple_ped[, ShortLabel := paste0("N", seq_len(.N))]
visped(tidy_simple_ped, labelvar = "ShortLabel", cex = 0.3, symbolsize = 10)

# A row-aligned vector is also accepted
visped(tidy_simple_ped, labelvar = rep("1x", nrow(tidy_simple_ped)),
       cex = 0.3, symbolsize = 10)

## ----highlight1---------------------------------------------------------------
visped(tidyped(small_ped), highlight = c("Y", "Z1"))

## ----highlight_trace----------------------------------------------------------
# Highlight individual "Y" and all its ancestors and descendants
visped(tidyped(small_ped), highlight = "Y", trace = TRUE)

## ----highlight2---------------------------------------------------------------
visped(tidyped(small_ped), 
       highlight = list(ids = c("Y", "Z1"), 
                        frame.color = "#4caf50", 
                        color = "#81c784"))

## ----showinbreed--------------------------------------------------------------
library(data.table)
test_ped <- data.table(
  Ind = c("A", "B", "C", "D", "E"),
  Sire = c(NA, NA, "A", "C", "C"),
  Dam = c(NA, NA, "B", "B", "D"),
  Sex = c("male", "female", "male", "female", "male")
)
tidy_test_ped_inbreed <- tidyped(test_ped, inbreed = TRUE)
visped(tidy_test_ped_inbreed, showf = TRUE)

## ----plant-selfing-draw-------------------------------------------------------
library(data.table)

# P1 × P2 → F1;  F1 selfs → F2_1, F2_2;  F2_1 selfs → F3
plant_ped <- data.table(
  Ind  = c("P1", "P2", "F1", "F2_1", "F2_2", "F3"),
  Sire = c(NA,   NA,   "P1", "F1",   "F1",   "F2_1"),
  Dam  = c(NA,   NA,   "P2", "F1",   "F1",   "F2_1")
)

tp_plant <- tidyped(plant_ped, selfing = TRUE, inbreed = TRUE)

visped(tp_plant, compact = TRUE, showf = TRUE, cex=0.5, symbolsize=10)

## ----deepped, eval=FALSE------------------------------------------------------
# cand_J11_labels <- deep_ped[(substr(Ind, 1, 3) == "K11"), Ind]
# visped(
#   tidyped(deep_ped,
#     cand = cand_J11_labels,
#     tracegen = 3),
#   cex=0.08,
#   symbolsize=5.5
#   )

## ----reduceped1---------------------------------------------------------------
cand_J11_labels <- deep_ped[(substr(Ind,1,3) == "K11"),Ind]
visped(
  tidyped(
    deep_ped,
    cand = cand_J11_labels,
    trace = "up",
    tracegen = 3
  ),
  cex=0.08, 
  symbolsize=5.5,
  compact = TRUE,
  showgraph = TRUE,
  file = tempfile(fileext = ".pdf")
)

## ----reduceped2---------------------------------------------------------------
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

## ----reduceped3---------------------------------------------------------------
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

## ----pedofoneind--------------------------------------------------------------
suppressWarnings(
  K110550H_ped <- tidyped(deep_ped, cand = "K110550H")
)
if(interactive()) {
  visped(K110550H_ped, compact = TRUE)
}

## ----optiMate-----------------------------------------------------------------
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

