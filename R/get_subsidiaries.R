get_subsidiaries <- function(companyName) {
    ##create the search URL
    searchURL <- URLencode(paste("https://www.google.com/search?q=", companyName, " subsidiaries", sep = ""))
    
    ##start session
    out <- html_session(searchURL)
    
    ##get all the span
    companyNodes <- out %>% html_nodes("div#center_col ol span")%>% html_text()
    
    ##get spans that need to removed from list
    companyNodesFilter1 <- out %>% html_nodes("div#center_col ol span.st")%>% html_text()
    companyNodesFilter2 <- out %>% html_nodes("div#center_col ol span.mime")%>% html_text()
    
    ##identify only nodes of interest
    companyNodes <- companyNodes[!companyNodes %in% c(companyNodesFilter1, unique(companyNodesFilter2), "")]
    ##get the final list of companies
    companyNodes <- companyNodes[!str_detect(companyNodes, "PDF")] %>% 
        str_replace_all(., ",", "")
}