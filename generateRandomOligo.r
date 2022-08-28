

generateRandomOligo <- function(oligoLen=NULL) {
  if (missing(oligoLen)) {
    oligoLen = sample(10:64, 1)
  } 
  randomOligo <- ""
  nucleotides <- list('A', 'C', 'T', 'G')
  nucleotide_indices <- sample(1:4, oligoLen, replace = TRUE)
  for (index in nucleotide_indices) {
    nt_to_paste <- nucleotides[nucleotide_indices[index]]
    randomOligo <- paste0(randomOligo, nt_to_paste)
  }
  return(randomOligo)
}
