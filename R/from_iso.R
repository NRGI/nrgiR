from_iso <- function(x) {

  lookup <- iso

  data.table::setDT(lookup)
  lookup[iso3=="NAM", iso2:="NA"]
  
  if(nchar(x[1])==3){
    lookup[, match_lower:=tolower(iso3)]
  }
  if(nchar(x[1])==2){
    lookup[, match_lower:=tolower(iso2)]
  }

  x_low <- gsub('[[:punct:]]','', x)

  out <- merge(
    data.table::data.table(rawName_in=tolower(x_low), input=x),
    unique(lookup[, c("match_lower","cleanName")]),
    by.x='rawName_in',
    by.y='match_lower',
    all.x=T,
    all.y=F,
    sort=FALSE
  )

  if(nrow(out[is.na(cleanName)])>0){
    n <- which(is.na(out$cleanName))

    warning(paste0("Could not match ", length(n), " value(s). Position(s): ", paste0(n, collapse=", ")))

  }

 final <- out$cleanName

 final






}
