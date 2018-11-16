get_rp_website <- function(DT=FALSE, tibble=FALSE) {

  if(DT & tibble) {
      stop("Select DT or tibble")
  }

  temp <- tempfile(fileext = ".zip")
  url <- "https://resourceprojects.org/api/projects?download=1"
  download.file(url, temp, mode='wb')

  z <- unzip(temp, files = c("Entity.csv", "Project.csv", "Source.csv"))

  res <- list()

  res$entity <- read.csv("Entity.csv", stringsAsFactors = FALSE)
  res$project <- read.csv("Project.csv", stringsAsFactors = FALSE)
  res$source <- read.csv("Source.csv", stringsAsFactors = FALSE)

  if(DT) {
    cat("Loading as data.table")
    lapply(res, data.table::setDT)
  }

  if(tibble) {
    cat("Loading as tibble")
    res$entity <- dplyr::as_tibble(res$entity)
    res$project <-dplyr::as_tibble(res$project)
    res$source <- dplyr::as_tibble(res$source)
  }

  file.remove(temp)
  file.remove(z[1], z[2], z[3])

  res

}
