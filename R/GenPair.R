#' Generate pre-post data for an adverse event
#'
#' This function creates data of pre-post contingency table for occurrence of an adverse event.
#'
#' @param n_pairs Integer, number of pairs or number of individuals.
#' @param pi Vector, probabilities of marginal pre-post multinomial distribution.
#' @param s_node Vector, node vector of true signal.
#' @param ns_node Vector, node vector of non-true signal.
#' @return A data frame generated for a pre-post contingency table for occurrence of an adverse event.
#' @export

GenPair <- function(n_pairs, pi, s_node, ns_node){
  n <- as.vector(rmultinom(1, n_pairs, pi))
  n11 <- data.frame(pre = sample(size=n[1], x=s_node, replace=T), post = sample(size=n[1], x=s_node, replace=T))
  n10 <- data.frame(pre = sample(size=n[2], x=s_node, replace=T), post = sample(size=n[2], x=ns_node, replace=T))
  n01 <- data.frame(pre = sample(size=n[3], x=ns_node, replace=T), post = sample(size=n[3], x=s_node, replace=T))
  n00 <- data.frame(pre = sample(size=n[4], x=ns_node, replace=T), post = sample(size=n[4], x=ns_node, replace=T))
  
  rbind(n11, n10, n01, n00) %>% 
    mutate(id=1:n()) %>%
    pivot_longer(pre:post, names_to='time', values_to='node') %>%
    data.frame()
}
