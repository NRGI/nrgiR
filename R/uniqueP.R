uniqueP <- function(x, collapse="; ", sort=T) {

  if(sort){
    paste0(sort(unique(x)), collapse=collapse)
  } else {
    paste0(unique(x), collapse=collapse)
  }

}
