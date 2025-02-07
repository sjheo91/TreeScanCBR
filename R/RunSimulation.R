#' Run simulation analysis
#'
#' This function performs a simulation analysis of your simulation settings.
#'
#' @param n_AE Integer, number of adverse events.
#' @param n_pairs Integer, number of pairs or number of individuals.
#' @param n_signals Integer, number of true signals.
#' @param RR_A Numeric, relative risk for treatment A.
#' @param RR_B Numeric, relative risk for treatment B.
#' @param null_pi_A Vector, null probabilities of marginal pre-post multinomial distribution for treatment A.
#' @param null_pi_B Vector, null probabilities of marginal pre-post multinomial distribution for treatment B.
#' @param n_iter Integer, number of MCMC iterations.
#' @param n_core Integer, number of cores for parallel process.
#' @return A list containing data frame generated for a specific simulation setting and data frame for true signals.
#' @export

RunSimulation <- function(n_AE, n_pairs, n_signal, RR_A, RR_B, null_pi_A, null_pi_B, n_MCMC, n_cores){
  
  gen <- GenData(n_AE=n_AE, n_pairs=n_pairs, n_signals=n_signals, RR_A=1, RR_B=1, pi_A=null_pi_A, pi_B=null_pi_B)
  mc_dist <- GetMCMCdist(data=gen$data, n_iter=n_MCMC, n_cores=n_cores)
  
  pi_A <- c(null_A[1],null_A[2]*RR_A,null_A[3],1-(null_A[1]+null_A[2]*RR_A+null_A[3]))
  pi_B <- c(null_B[1],null_B[2]*RR_B,null_B[3],1-(null_B[1]+null_B[2]*RR_B+null_B[3]))
  
  # run simulation
  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = n_simul, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  output <- foreach(i=1:n_simul, .options.snow=opts, .combine='rbind',
                    .export=c('GenPair','GetCount','GetStatistic'), .packages=c('dplyr','tidyr')) %dopar% 
    {
      gen <- GenData(n_AE=n_AE, n_pairs=n_pairs, n_signals=n_signals, RR_A=RR_A, RR_B=RR_B, pi_A=pi_A, pi_B=pi_B)
      simul_data <- gen$data
      signal_df <- gen$signal
      stat <- GetStatistic(simul_data)$param %>% left_join(signal_df, by='node')
      
      cut <- quantile(mc_dist, p=0.95, na.rm=T)
      
      output <- stat %>% 
        mutate(detect = if_else(statistic>cut, 1, 0)) %>%
        reframe(power = if_else(any(detect==1, na.rm=T), 1, 0),
                sens = sum(detect==1&signal==1, na.rm=T)/sum(signal, na.rm=T),
                FDR = sum(detect==1&signal==0, na.rm=T)/sum(detect, na.rm=T))
      
      return(output)
    }
  stopCluster(cl)
  
  output %>%
    reframe(sens = mean(sens[power==1], na.rm=T),
            FDR = mean(FDR[power==1], na.rm=T),
            power = mean(power, na.rm=T))
  
}
