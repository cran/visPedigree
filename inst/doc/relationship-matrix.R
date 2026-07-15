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

## ----pedprod_Ax---------------------------------------------------------------
# Load example pedigree and tidy it
data(small_ped)
tped <- tidyped(small_ped)

# Equal contributions from two candidates; other individuals have zero weight
weights <- c(Z1 = 0.5, Z2 = 0.5)

# Relationship of every pedigree individual to the weighted candidate group
Ax <- pedprod(tped, weights)
head(Ax)

# Average additive relationship c' A c and average coancestry
mean_relationship <- sum(weights * Ax[names(weights)])
mean_coancestry <- mean_relationship / 2
c(mean_relationship = mean_relationship, mean_coancestry = mean_coancestry)

## ----pedprod_named_unnamed----------------------------------------------------
# Named: only listed individuals get non-zero values
named_x <- c(Z1 = 0.3, Z2 = 0.4, A = 0.3)
pedprod(tped, named_x)[1:6]  # B, C, etc. are automatically zero

# Unnamed: must match the pedigree size
unnamed_x <- rep(1, nrow(tped))
length(pedprod(tped, unnamed_x))

## ----pedprod_Ainv-------------------------------------------------------------
# Ainv * vector
x <- rnorm(nrow(tped))
Ainv_x <- pedprod(tped, x, method = "Ainv")
head(Ainv_x)

# Verify against explicit computation (small pedigree only)
A <- pedmat(tped, method = "A", sparse = FALSE)
Ainv <- pedmat(tped, method = "Ainv", sparse = FALSE)
all.equal(
  unname(Ainv_x),
  unname(drop(Ainv %*% x)),
  tolerance = 1e-12
)

## ----pedprod_Ainv_identity----------------------------------------------------
# Ainv * (A * x) should recover x (to machine precision)
A_x <- pedprod(tped, x, method = "A")
Ainv_Ax <- pedprod(tped, A_x, method = "Ainv")
all.equal(unname(Ainv_Ax), unname(x), tolerance = 1e-12)

## ----pedprod_AX---------------------------------------------------------------
# Three ways to weight the SAME two candidates (Z1 and Z2 are full sibs)
schemes <- cbind(
  Equal      = c(Z1 = 0.5, Z2 = 0.5),
  Z1_only    = c(Z1 = 1.0, Z2 = 0.0),
  Weighted   = c(Z1 = 0.7, Z2 = 0.3)
)
AX <- pedprod(tped, schemes)

# The schemes diverge at the candidates themselves (and their descendants);
# a shared ancestor is equally related to any weighting of two full sibs.
AX[c("Z1", "Z2"), ]

# Expected average relationship within each contributing group, 0.5 * c'A c.
# Under random mating this equals the mean inbreeding of the resulting progeny.
group_coancestry <- 0.5 * colSums(schemes * AX[rownames(schemes), ])
round(group_coancestry, 5)

## ----pedprod_ocs--------------------------------------------------------------
# Weighted candidate contributions
candidates <- setNames(c(0.25, 0.25, 0.25, 0.25), c("Z1", "Z2", "A", "B"))
Ac <- pedprod(tped, candidates)

# Average coancestry of the selected group: 0.5 * c' A c
c_accepted <- sum(candidates * Ac[names(candidates)]) / 2
c_accepted

## ----pedprod_blup-------------------------------------------------------------
# Simulated breeding values as a matrix right-hand side
set.seed(20260704)
Z_design <- cbind(
  trait1 = rnorm(nrow(tped)),
  trait2 = rnorm(nrow(tped))
)
rownames(Z_design) <- tped$Ind

# Ainv * Z in one traversal — no Ainv ever stored
Ainv_Z <- pedprod(tped, Z_design, method = "Ainv")
dim(Ainv_Z)
Ainv_Z[1:5, ]

## ----pedprod_geneflow---------------------------------------------------------
# One unit contribution vector per founder: column f isolates founder f
founder_ids <- tped[Gen == 1, Ind]
founder_design <- diag(length(founder_ids))
dimnames(founder_design) <- list(founder_ids, founder_ids)

# A %*% e_f is founder f's expected genetic contribution to every individual
footprint <- pedprod(tped, founder_design, method = "A")

# Founder composition of one candidate: contributions sum to one
round(footprint["Z1", ], 4)

# Average founder contribution to the youngest generation, ranked
young <- tped[Gen == max(Gen), Ind]
founder_share <- sort(colMeans(footprint[young, , drop = FALSE]), decreasing = TRUE)
round(founder_share, 4)

## ----pedprod_large_proof, eval=FALSE------------------------------------------
# # Pedigrees beyond the dense-A guard still work with pedprod()
# n <- 50000L
# ids <- paste0("I", seq_len(n))
# raw <- data.frame(
#   Ind  = ids,
#   Sire = c(NA_character_, ids[-n]),
#   Dam  = NA_character_,
#   stringsAsFactors = FALSE
# )
# tped_large <- tidyped(raw)
# 
# # pedprod works; pedmat would error
# result <- pedprod(tped_large, setNames(1, tail(ids, 1)))
# length(result)  # 50000

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

