xpo <- function(){

  dr123 <- rstudioapi::getActiveDocumentContext()

  row786 <- dr123$selection[[1]]$range$end[1]
  end786 <- dr123$selection[[1]]$range$end

  # dr123 <- document_range(start = document_position(row = row786, column = 1), end = end786)
  dr123 <- rstudioapi::document_range(
    start = rstudioapi::document_position(row = row786, column = 1),
    end = rstudioapi::document_position(row = row786, column = 1000)
  )

  dr123 <- rstudioapi::setSelectionRanges(dr123)

  dr123 <- rstudioapi::getActiveDocumentContext()

  dr123 <- dr123$selection[[1]]$text
  dr123 <- paste0("View(", dr123, ")")
  print(dr123)
  eval(parse(text=dr123))


}
