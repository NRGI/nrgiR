clean_characters <- function(rawChars){

  unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A',
                        'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A',
                        'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E',
                        'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I',
                        'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O',
                        'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U',
                        'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='s', 'à'='a',
                        'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a',
                        '’'="'", '“'='"', '”'='"', 'ü'='u',#'æ'='a',
                        'ç'='c', 'è'='e', 'é'='e', 'ê'='e',
                        'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i',
                        'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o',
                        'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u',
                        'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')

  cleanTibble <- dplyr::tibble(rawVals = rawChars) %>%
    dplyr::filter(!is.na(rawVals)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cleanRaws = rawVals)


  inputChars <- cleanTibble$cleanRaws

  ##Initialize the vec
  out <- character()

  for (i in 1:length(inputChars)) {

    inputChars[i] <- stringr::str_replace_all(inputChars[i], "\\s+", " ") %>%
      stringr::str_trim()

    inputChars[i] <- gsub("£", "GBP", inputChars[i])
    inputChars[i] <- gsub("€", "EUR", inputChars[i])
    inputChars[i] <- gsub("–", "-", inputChars[i])
    inputChars[i] <- gsub("æ", "ae", inputChars[i])

    if (inputChars[i] == "" | inputChars[i] == "NA") {
      inputChars[i] <- NA
    }

    cleaned <- chartr(paste(names(unwanted_array),
                            collapse = ''),
                      paste(unwanted_array,
                            collapse = ''),
                      inputChars[i])

    if (Encoding(cleaned) == "UTF-8") {
      cleaned <- iconv(cleaned, from = "UTF-8", to = "ASCII", sub = "")
    }

    out <- c(out, cleaned)


  }
  cleanTibble$out <- out

  outTibble <- dplyr::tibble(rawVals = rawChars) %>%
    dplyr::left_join(., cleanTibble, by = "rawVals") %>%
    dplyr::select(out) %>%
    unlist(use.names = F) %>%
    as.character()

  outTibble
}
