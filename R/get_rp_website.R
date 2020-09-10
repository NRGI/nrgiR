get_rp_website <- function(DT=FALSE, mode = "wb"){
  
  temp <- tempfile(fileext = ".ext")
  download.file("https://s3.us-east-2.amazonaws.com/rp-20-sources/rp-final-data", temp, mode = mode)
  rp <- list()
  rp$source <- readxl::read_excel(temp, sheet = "sources")
  rp$project <- readxl::read_excel(temp, sheet = "projects")
  rp$entity <- readxl::read_excel(temp, sheet = "entities")
  
  if(DT==TRUE){
    cat("Loading as data.table")
    setDT(rp$entity)
    setDT(rp$source)
    setDT(rp$project)
  }
  
  file.remove(temp)
  
  rp
  
}
