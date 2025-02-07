#' Get conditional background risk TreeScan statistic 
#'
#' This function calculate conditional background risk TreeScan statistic from the data.
#'
#' @param data Data frame, data for counts of pre-post contingency table for treatment A and B.
#' @return A data frame for counts and statistics of pre-post contingency table for a node.
#' @export

GetStatistic <- function(data){
  output <- list(count=list(), param=list())
  u_node <- unique(data$node) %>% sort
  
  output$count <- do.call('rbind',lapply(u_node, GetCount, data=data))
  output$param <- output$count %>% 
    reframe(node = node,
            RR_A = nA10/nA01, RR_B = nB10/nB01,
            muA = log(nA10/nA01), sigmaA = sqrt(1/nA10+1/nA01),
            muB = log(nB10/nB01), sigmaB = sqrt(1/nB10+1/nB01),
            mu0 = muA-muB, sigma0 = sqrt(1/nA10+1/nA01+1/nB10+1/nB01),
            statistic = (mu0^2/sigma0^2)*(mu0>0),
            statistic = if_else(is.infinite(statistic)|is.nan(statistic), NA, statistic))
  
  return(output)
}