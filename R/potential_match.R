potential_match <- function(vec1, group1=NULL, vec2, group2=NULL, fancy=FALSE, agrep_value=.3, adist_value=1) {
  require(data.table)
  if(is.null(group1) & !is.null(group2)){
    stop("Grouping variable required for both or neither vectors")
  }

  if(!is.null(group1) & is.null(group2)){
    stop("Grouping variable required for both or neither vectors")
  }

  if(!is.null(group1) & !is.null(group2)){
    if(length(vec1)!=length(group1)){
      stop("vec1 & group1 must be same length")
    }
    if(length(vec2)!=length(group2)){
      stop("vec2 & group2 must be same length")
    }
  }

  if(is.null(group1) & is.null(group2)){
    group1 <- ""
    group2 <- ""
  }




  d1 <- data.table::data.table(vec=vec1, group=group1)
  d2 <- data.table::data.table(vec=vec2, group=group2)

  if(fancy==FALSE){

    matches <- data.table::data.table('groupVals', 'original'=c(NA), "1"=c(NA), "2"=c(NA), "3"=c(NA), "4"=c(NA), "5"=c(NA), "6"=c(NA))

    check <- unique(d1)

    for(i in 1:nrow(check)) {
      print(paste0(i,"/", nrow(check)))
      country <- check[i]$group
      y <- agrep(tolower(check[i]$vec),
                 tolower(d2[group==country]$vec),
                 max.distance = agrep_value)
      y <- unique(d2[group==country]$vec[y])


      first_letter <- substr(check[i]$vec, 1, 1)

      ord1 <- which(substr(y, 1, 1)==first_letter)

      ord2 <- which(substr(y, 1, 1)!=first_letter)

      y <- y[c(ord1, ord2)]

      matches <- rbind(matches,
                       list(check[i]$group, check[i]$vec, y[1], y[2], y[3], y[4], y[5], y[6]))
    }

    final <- matches[!is.na(`1`)]
    final[is.na(final)] <- ""
    final
  }

  ####################################################################

  if(fancy==TRUE){
    lt <- list()

    for(j in 1:length(unique(d1$group))){
      print(paste0(j,"/", length(unique(d1$group))))

      country <- unique(d1$group)[j]

      if(nrow(d2[group==country])==0){
        next()
      }

      s1 <- d2[group==country]
      s2 <- d1[group==country]

      dist <- adist(s1$vec,
                    s2$vec,
                    partial = TRUE,
                    ignore.case = TRUE)

      dist2 <- apply(dist, 1, min)
      result <- NULL

      for(i in 1:nrow(dist)) {
        cat(paste0("  -", i, "/", nrow(dist), "\n"))
        s2.i <- match(dist2[i], dist[i,])
        s1.i <- i
        result <- rbind(
          data.frame(
            reconcilePosition = s2.i,
            lookupPosition = s1.i,
            reconcileName = s2[s2.i,]$vec,
            lookupName = s1[s1.i,]$vec,
            adist = dist2[i]
          ),
          result)
      }
      # and we then can have a look at the results
      data.table::setDT(result)

      #CHANGE ADIST HERE
      res <- result[order(reconcileName, adist)][adist<=adist_value][, c("reconcileName", "lookupName")]

      if(nrow(res)>0){
        res <- data.frame(groupVals=country, res)
        lt[[j]] <- res
      } else {
        lt[[j]] <- NULL
      }
    }

    dt <- data.table::rbindlist(lt)
    final <- data.table::copy(dt)
    final[, id:=1:.N, by=.(groupVals, reconcileName)]
    final <- final[id<=6]
    final[, lookupName:=as.character(lookupName)]
    final <- data.table::dcast(final, groupVals + reconcileName ~ id, value.var = "lookupName")
    final[is.na(final)] <- ""
    data.table::setnames(final, c("groupVals","reconcileName"), c("V1", "original"))
  }

  final

}
