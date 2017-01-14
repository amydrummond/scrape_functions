if (require(jsonlite) == FALSE) {
  install.packages('jsonlite')
  library(jsonlite)
}

if (require(reshape2) == FALSE) {
  install.packages('reshape2')
  library(reshape2)
}

clarityContestLookup <- function(website){
  sum.json <- 'json/sum.json'
  summary.web <- paste0(website, sum.json)
  summary <- fromJSON(summary.web)  
  names <- summary$Contests$C
  contests <- summary$Contests$K
  lookupContests <- data.frame(names,contests, stringsAsFactors = FALSE)
  return(lookupContests)
}

clarityPrecinctScrape <- function(website, contest) {
  options(stringsAsFactors = FALSE)
  
  sum.json <- 'json/sum.json'
  details.json <- 'json/details.json'
  status.json <- 'status.json'
  
  summary.web <- paste0(website, sum.json)
  summary <- fromJSON(summary.web)
  candidates <-
    summary$Contests$CH[summary$Contests$K == contest][[1]]
  
  detail.web <- paste0(website, details.json)
  details <- fromJSON(detail.web)
  precinct.results <-
    as.data.frame(details$Contests$V[details$Contests$K == contest])
  precincts <- details$Contests$P[details$Contests$K == contest]
  
  precinct.results <- cbind(precincts, precinct.results)
  headers <- c("PRECINCT", candidates)
  names(precinct.results) <- headers
  precinct.results <- melt(precinct.results, id.vars = 'PRECINCT', stringsAsFactors = FALSE)
  names(precinct.results) <- c('precinct', 'candidate_name', 'votes')
  precinct.results$candidate_name <- unlist(lapply(precinct.results$candidate_name, as.character))
  return(precinct.results)
}

clarityCountyScrape <- function(website, contest){
  sum.json <- 'json/sum.json'
  summary.web <- paste0(website, sum.json)
  summary <- fromJSON(summary.web)
  candidate_name <-
    summary$Contests$CH[summary$Contests$K == contest][[1]]
  results <- summary$Contests$V[summary$Contests$K == contest][[1]]
  total.precincts <- summary$Contests$TP[summary$Contests$K == contest][[1]]
  precincts.reporting <- summary$Contests$PR[summary$Contests$K == contest][[1]]
  county.results <- data.frame(candidate_name, precincts.reporting, total.precincts, results, stringsAsFactors = FALSE)
  return(county.results)
    
}
