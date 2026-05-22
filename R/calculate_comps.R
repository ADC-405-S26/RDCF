#' Build a Comparable Company Analysis Table
#'
#' Retrieves valuation multiples for the target company and all of its peer
#' companies and assembles them into a single comparison table. Also computes
#' summary statistics (median, mean, min, max) across the peer group.
#'
#' @param company_info A list of class \code{"rdcf_company_info"} as returned
#'   by \code{get_company_info}.
#' @param peers A character vector of class \code{"rdcf_peers"} as returned
#'   by \code{get_peers}.
#'
#' @return A list of class \code{"rdcf_comps"} containing:
#' \describe{
#'   \item{target}{The target company ticker.}
#'   \item{target_multiples}{A list of class \code{"rdcf_multiples"} for the
#'     target company.}
#'   \item{peers_table}{A data frame with
