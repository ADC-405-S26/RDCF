#' Get Valuation Multiples for a Company
#'
#' Retrieves the four key valuation multiples used in comparable company
#' analysis for a single publicly traded company: Price-to-Earnings (P/E),
#' EV/EBITDA, Price-to-Sales (P/S), and Price-to-Book (P/B).
#'
#' @param ticker Character. A valid stock ticker symbol (e.g., \code{"AAPL"}).
#'   Case-insensitive.
#'
#' @return A named list of class \code{"rdcf_multiples"} containing:
#' \describe{
#'   \item{ticker}{The ticker symbol used.}
#'   \item{pe_ratio}{Price-to-Earnings ratio. \code{NA} if earnings negative.}
#'   \item{ev_ebitda}{Enterprise Value to EBITDA ratio. \code{NA} if EBITDA
#'     is negative or unavailable.}
#'   \item{ps_ratio}{Price-to-Sales ratio.}
#'   \item{pb_ratio}{Price-to-Book ratio.}
#' }
#'
#' @details
#' Multiples are derived from Yahoo Finance data. A multiple is set to
#'
