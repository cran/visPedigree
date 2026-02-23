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
summary(mat_A)

## ----query--------------------------------------------------------------------
# Query relationship between Z1 and Z2
query_relationship(mat_A, "Z1", "Z2")

# Query multiple pairs
query_relationship(mat_A, c("Z1", "A"), c("Z2", "B"))

## ----compact_calc-------------------------------------------------------------
# Calculate compacted A matrix
mat_compact <- pedmat(tped, method = "A", compact = TRUE)

# The result is a 'pedmat' object containing the compacted matrix
print(mat_compact)

## ----expand-------------------------------------------------------------------
# Expand to full 28x28 matrix
mat_full <- expand_pedmat(mat_compact)
dim(mat_full)

# Query still works the same way
query_relationship(mat_compact, "Z1", "Z2")

## ----heatmap, fig.width=6, fig.height=6---------------------------------------
# Heatmap of the A matrix
vismat(mat_A)

## ----heatmap_group, fig.width=6, fig.height=6---------------------------------
# Mean relationship between generations
vismat(mat_A, ped = tped, grouping = "Gen")

## ----histogram, fig.width=6, fig.height=4-------------------------------------
# Distribution of relationship coefficients
vismat(mat_A, type = "histogram")

