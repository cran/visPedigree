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
library(Matrix)

## ----basic_calc---------------------------------------------------------------
# Load example pedigree and tidy it
data(small_ped)
tped <- tidyped(small_ped)

# Calculate Additive Relationship Matrix (A)
mat_A <- pedmat(tped, method = "A")

# Calculate Dominance Relationship Matrix (D)
mat_D <- pedmat(tped, method = "D")

# Calculate inbreeding coefficients (f)
vec_f <- pedmat(tped, method = "f")

## ----sparse_check-------------------------------------------------------------
class(mat_A)

## ----matrix_summary-----------------------------------------------------------
tail(summary(mat_A),10)

## ----query--------------------------------------------------------------------
# Query relationship between Z1 and Z2
query_relationship(mat_A, "Z1", "Z2")

# Query multiple pairs
query_relationship(mat_A, c("Z1", "A"), c("Z2", "B"))

## ----compact_calc-------------------------------------------------------------
# Calculate compacted A matrix
mat_compact <- pedmat(tped, method = "A", compact = TRUE)

# The result is a 'pedmat' object containing the compacted matrix
print(mat_compact[11:20,11:20])

## ----expand-------------------------------------------------------------------
# Expand to full 28x28 matrix
mat_full <- expand_pedmat(mat_compact)
dim(mat_full)

# Query still works the same way
query_relationship(mat_compact, "Z1", "Z2")

## ----heatmap, fig.width=6, fig.height=6---------------------------------------
# Heatmap of the A matrix (with default clustering reorder)
vismat(mat_A, labelcex = 0.5)

## ----heatmap_compact, fig.width=6, fig.height=6-------------------------------
# Compact matrix: expanded automatically (message printed)
vismat(mat_compact,labelcex=0.5)

## ----heatmap_no_reorder, fig.width=6, fig.height=6----------------------------
vismat(mat_A, reorder = FALSE, labelcex = 0.5)

## ----heatmap_ids, fig.width=5, fig.height=5-----------------------------------
target_ids <- rownames(as.matrix(mat_A))[1:8]
vismat(mat_A, ids = target_ids,
       main = "Relationship Heatmap — First 8 Individuals")

## ----heatmap_group, fig.width=6, fig.height=6---------------------------------
# Mean relationship between generations
vismat(mat_A, ped = tped, by = "Gen",
       main = "Mean Relationship Between Generations")

## ----heatmap_family, fig.width=6, fig.height=6--------------------------------
# Mean relationship between full-sib families
# (founders without a family assignment are excluded automatically)
vismat(mat_A, ped = tped, by = "Family",
       main = "Mean Relationship Between Full-Sib Families")

## ----histogram, fig.width=6, fig.height=4-------------------------------------
# Distribution of relationship coefficients
vismat(mat_A, type = "histogram")

## ----large_ped_tip, fig.width=9, fig.height=8---------------------------------
data(big_family_size_ped)

tp_big <- tidyped(big_family_size_ped)
last_gen <- max(tp_big$Gen, na.rm = TRUE)

# Compute the compact A matrix for the entire pedigree
mat_big_compact <- pedmat(tp_big, method = "A", compact = TRUE)

# Focus on all individuals in the last generation that belong to a family
ids_last_gen <- tp_big[Gen == last_gen & !is.na(Family), Ind]

# vismat() aggregates directly from the compact matrix — no expansion needed
vismat(
       mat_big_compact,
       ped = tp_big,
       ids = ids_last_gen,
       by = "Family",
       labelcex = 0.3,
       main = paste("Mean Relationship Between All Families in Generation", last_gen)
)

