to_iso <- function(x, output="iso3") {

  if(!output %in% c("iso2","iso3","clean")){ stop("`output` must be one of: 'iso2', 'iso3', or 'clean'")  }
  lookup <- iso

  data.table::setDT(lookup)

  lookup[, rawName_lower:=tolower(rawName)]
  x_low <- gsub('[[:punct:]]','', x)

  out <- merge(
    data.table::data.table(rawName_in=tolower(x_low), input=x),
    unique(lookup[, c("rawName_lower","cleanName","iso3","iso2")]),
    by.x='rawName_in',
    by.y='rawName_lower',
    all.x=T,
    all.y=F,
    sort=FALSE
  )

  if(nrow(out[is.na(iso3)])>0){
    n <- which(is.na(out$iso3))
    warning(paste0("Could not match ", length(n), " value(s). Position(s): ", paste0(n, collapse=", ")))
  }

  out[is.na(iso3), iso3:=input]
  out[is.na(iso2), iso2:=input]
  out[is.na(cleanName), cleanName:=input]

  if(output=="iso2"){
    final <- out$iso2
  } else if(output=="iso3") {
    final <- out$iso3
  } else if(output=="clean") {
    final <- out$cleanName
  }

  final

}
