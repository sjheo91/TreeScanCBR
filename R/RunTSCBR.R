#' Run conditional background risk TreeScan 
#'
#' This function run conditional background risk TreeScan.
#'
#' @param data Data frame, data for counts of pre-post contingency table for treatment A and B.
#' @param n_MCMC Integer, number of MCMC iterations.
#' @param n_core Integer, number of cores for parallel process.
#' @return A data frame for counts, statistics, and signal detection results of pre-post contingency table for a node.
#' @export

RunTSCBR <- function(data, n_MCMC, n_core=10){
  
  mc_dist <- GetMCMCdist(data=data, n_iter=n_MCMC, n_core=n_core)
  cut <- quantile(mc_dist, p=0.95, na.rm=T)
  
  stat <- GetStatistic(data)$param %>% left_join(signal_df, by='node')
  output <- stat %>% mutate(detect = if_else(statistic>cut, 1, 0))
  
  return(output)
  
}
