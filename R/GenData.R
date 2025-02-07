#' Generate simulation data
#'
#' This function creates simulated data for a specific simulation setting. 
#'
#' @param n_AE Integer, number of adverse events.
#' @param n_pairs Integer, number of pairs or number of individuals.
#' @param n_signals Integer, number of true signals.
#' @param RR_A Numeric, relative risk for treatment A.
#' @param RR_B Numeric, relative risk for treatment B.
#' @param pi_A Vector, probabilities of marginal pre-post multinomial distribution for treatment A.
#' @param pi_B Vector, probabilities of marginal pre-post multinomial distribution for treatment B.
#' @return A list containing data frame generated for a specific simulation setting and data frame for true signals.
#' @export


GenDate <- function(n_AE, n_pairs, n_signals, RR_A, RR_B, pi_A, pi_B){
  
  node <- 1:n_AE
  if(RR_A==1&RR_B==1){
    s_node_A <- s_node_B <- node
    ns_node_A <- ns_node_B <- node
  }else if(RR_A!=1&RR_B==1){
    s_node_A <- sample(node, n_signals)
    ns_node_A <- node[!node%in%s_node_A]
    s_node_B <- node
    ns_node_B <- node
  }else if(RR_A==1&RR_B!=1){
    s_node_A <- node
    ns_node_A <- node
    s_node_B <- sample(node, n_signals)
    ns_node_B <- node[!node%in%s_node_B]
  }else{
    s_node_A <- s_node_B <- sample(node, n_signals)
    ns_node_A <- ns_node_B <- node[!node%in%s_node_A]
  }
  
  data <- rbind(GenPair(n_pairs, pi_A, s_node_A, ns_node_A) %>% mutate(drug = 'A'),
                GenPair(n_pairs, pi_B, s_node_B, ns_node_B) %>% mutate(drug = 'B'))
  
  return(list(data=data, signal=data.frame(node=node, signal=if_else(node%in%s_node_A, 1, 0))))
  
}

