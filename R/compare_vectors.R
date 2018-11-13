compare_vectors <- function(a, b) {
  a <- as.character(unique(a))
  b <- as.character(unique(b))

  inboth <- a[a %in% b]

  in1 <- a[!a %in% b]
  in2 <- b[!b %in% a]

  res <- list("In both"=sort(inboth), "In A"=sort(in1), "In B"=sort(in2))
  res
}



















