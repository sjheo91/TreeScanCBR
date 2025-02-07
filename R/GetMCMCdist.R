#' Run Markov Chain Monte Carlo simulation
#'
#' This function run Markov Chain Monte Carlo (MCMC) simulation and get empirical MCMC distribution.
#'
#' @param data Data frame, data for counts of pre-post contingency table for treatment A and B.
#' @param n_iter Integer, number of MCMC iterations.
#' @param n_core Integer, number of cores for parallel process.
#' @return A vector for empirical MCMC distribution.
#' @export

GetMCMCdist <- function(data, n_iter, n_core=10){
  
  n_pairs <- length(unique(data$id))
  count_info <- GetStatistic(data)$count
  u_node <- count_info$node
  pi <- apply(count_info, 2, sum)
  pi_A <- pi[2:5]/sum(pi[2:5])
  pi_B <- pi[6:9]/sum(pi[6:9])
  
  cl <- makeCluster(n_core)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = n_iter, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  mc_dist <- foreach(i=1:n_iter, .options.snow=opts, .combine='c',
                     .export=c('GenPair','GetStatistic','GetCount'), .packages=c('dplyr','tidyr')) %dopar% {
                       mc_data <- rbind(GenPair(n_pairs, pi_A, u_node, u_node) %>% mutate(drug = 'A'),
                                        GenPair(n_pairs, pi_B, u_node, u_node) %>% mutate(drug = 'B'))
                       return(max(GetStatistic(mc_data)$param$statistic, na.rm=T))
                     }
  
  stopCluster(cl)
  
  return(mc_dist)
}