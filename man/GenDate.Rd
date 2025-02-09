% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GenData.R
\name{GenDate}
\alias{GenDate}
\title{Generate simulation data}
\usage{
GenDate(n_AE, n_pairs, n_signals, RR_A, RR_B, pi_A, pi_B)
}
\arguments{
\item{n_AE}{Integer, number of adverse events.}

\item{n_pairs}{Integer, number of pairs or number of individuals.}

\item{n_signals}{Integer, number of true signals.}

\item{RR_A}{Numeric, relative risk for treatment A.}

\item{RR_B}{Numeric, relative risk for treatment B.}

\item{pi_A}{Vector, probabilities of marginal pre-post multinomial distribution for treatment A.}

\item{pi_B}{Vector, probabilities of marginal pre-post multinomial distribution for treatment B.}
}
\value{
A list containing data frame generated for a specific simulation setting and data frame for true signals.
}
\description{
This function creates simulated data for a specific simulation setting.
}
