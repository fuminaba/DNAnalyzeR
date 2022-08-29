
# FUNCTION Generate Random Oligo  
generateRandomOligo <- function(oligoLen = NULL, cg_frac = NULL) {
  randomOligo <- ""
  if (missing(oligoLen)) {
    oligoLen = sample(10:64, 1)
  } 
  if (missing(cg_frac)) {
    nucleotides <- list('A', 'C', 'T', 'G')
    nucleotide_indices <- sample(1:4, oligoLen, replace = TRUE)
    for (index in nucleotide_indices) {
      nt_to_paste <- nucleotides[index]
      randomOligo <- paste0(randomOligo, nt_to_paste)
    }
    return(randomOligo)
    
  } else {
    nucleotide_indices <- sample(1:2, oligoLen, replace = TRUE, prob = c(1-cg_frac, cg_frac))
    nucleotides_AT <- list('A', 'T')
    nucleotides_CG <- list('C', 'G')
    for (index in nucleotide_indices) {
      if (index == 1) {
        nt_to_paste <- nucleotides_AT[sample(1:2,1)]
        randomOligo <- paste0(randomOligo, nt_to_paste)
      } else {
        nt_to_paste <- nucleotides_CG[sample(1:2,1)]
        randomOligo <- paste0(randomOligo, nt_to_paste)
      }
    }
    return(randomOligo)
  }
}
# END - generateRandomOligo()

# FUNCTION Compute the CG Fraction in an Oligonucleotide
compute_CG_fraction <- function(oligo_sequence) {
  oligoLen <- nchar(oligo_sequence)
  nucleotides <- strsplit(oligo_sequence, "")[[1]]
  cg_count <- 0
  nt_count <- data.frame(row.names = c("A", "T", "C", "G"), val = c(0, 0, 0, 0))
  for (nt in nucleotides) {
    if (nt == "C" | nt == "G") {
      cg_count <- cg_count + 1
    }
  }
  cg_frac <- cg_count/oligoLen
  return(cg_frac)
}
# END - compute_CG_fraction()