#Function to download EITI data from resourcedata.org

get_eiti_company <- function(country="All") {

  x <- data.table::fread("https://www.resourcedata.org/dataset/7dd3c8b6-9256-4e34-9360-1519efd87407/resource/bba3b646-131b-47c5-9f4c-37019f896575/download/company-payments.csv")
  x

}

get_eiti_entity <- function(country="All") {

  x <- data.table::fread("https://www.resourcedata.org/dataset/7dd3c8b6-9256-4e34-9360-1519efd87407/resource/4e4462a5-6029-4ae3-a4cc-66ad6ccc6d70/download/revenues-received-by-government-agencies.csv")
  x

}
