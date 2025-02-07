#' Get count information from data of pre-post contingency table
#'
#' This function summarize data of pre-post contingency table to count information.
#'
#' @param data Data frame, data for counts of pre-post contingency table for treatment A and B.
#' @param x Integer, number corresponding to a specific node.
#' @return A data frame for counts of pre-post contingency table for a node.
#' @export

GetCount <- function(data, x){
  data <- data %>% mutate(x = factor(if_else(node==x, 1, 0), levels=c(1,0)))
  pre_A <- data$x[data$time=='pre'&data$drug=='A']
  post_A <- data$x[data$time=='post'&data$drug=='A']
  pre_B <- data$x[data$time=='pre'&data$drug=='B']
  post_B <- data$x[data$time=='post'&data$drug=='B']
  count <- c(table(post_A,pre_A) %>% as.vector(),table(post_B,pre_B) %>% as.vector()) %>%
    setNames(c('nA11','nA10','nA01','nA00','nB11','nB10','nB01','nB00')) %>% t %>% data.frame
  count <- cbind(node=x, count)
  return(count)
}
