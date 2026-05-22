#' Run a Complete Comparable Company Analysis Valuation
#'
#' The master wrapper function for the RDCF package. Calls all five underlying
#' functions in sequence and returns a complete comparable company analysis
#' valuation for any publicly traded company with a single function call.
#'
#' @param ticker Character. A valid stock ticker symbol (e.g., \code{"AAPL"},
#'   \code{"MSFT"}, \code{"JPM"}). Case-insensitive.
#' @param n_peers Integer. Number of peer companies to include in the analysis.
#'   Defaults to \code{5}.
#' @param market_cap_range Numeric. How wide a market cap band to search for
#'   peers as a multiplier. Defaults to \code{10}.
#'
#' @return A list of class \code{"rdcf_result"} containing:
#' \describe{
#'   \item{company_info}{Output of \code{get_company_info}.}
#'   \item{peers}{Output of \code{get_peers}.}
#'   \item{comps}{Output of \code{calculate_comps}.}
#'   \item{valuation}{Output of \code{estimate_value}.}
#' }
#'
#' @details
#' The full pipeline is:
#' \enumerate{
#'   \item \code{get_company_info} retrieves sector, industry, and financial
#'     metrics for the target.
#'   \item \code{get_peers} automatically identifies comparable companies in
#'     the same industry ranked by market cap proximity.
#'   \item \code{calculate_comps} assembles the full comparison table with
#'     peer summary statistics.
#'   \item \code{estimate_value} applies peer median multiples to derive an
#'     implied share price range and a verdict.
#' }
#'
#' Because this function pulls live data for the target and all peers it
#' typically takes 2-5 minutes to complete.
#'
#' @examples
#' \dontrun{
#' result <- comps_valuation("AAPL")
#' result$valuation$verdict
#' result$valuation$implied_median
#' result$comps$summary_stats
#' result$peers
#' }
#'
#' @importFrom rlang abort
#' @importFrom stats median
#' @export
comps_valuation <- function(ticker, n_peers = 5, market_cap_range = 10) {

  # Input validation
  checkmate::assert_string(ticker, min.chars = 1)
  checkmate::assert_count(n_peers, positive = TRUE)
  checkmate::assert_number(market_cap_range, lower = 1)

  ticker <- toupper(trimws(ticker))

  cat("==========================================================\n")
  cat("  RDCF: Comparable Company Analysis\n")
  cat("  Target:", ticker, "\n")
  cat("==========================================================\n\n")

  # Step 1: Company info
  cat("Step 1/4  Getting company information ...\n")
  company_info <- tryCatch(
    get_company_info(ticker),
    error = function(e) {
      rlang::abort(paste0("Step 1 failed - could not get company info: ",
                          conditionMessage(e)))
    }
  )
  cat(sprintf("  OK  %s | %s | %s\n\n",
              ticker, company_info$sector, company_info$industry))

  # Step 2: Find peers
  cat("Step 2/4  Finding peer companies ...\n")
  peers <- tryCatch(
    get_peers(company_info, n = n_peers, market_cap_range = market_cap_range),
    error = function(e) {
      rlang::abort(paste0("Step 2 failed - could not find peers: ",
                          conditionMessage(e)))
    }
  )
  cat(sprintf("  OK  Found %d peers: %s\n\n",
              length(peers), paste(peers, collapse = ", ")))
  # Step 3: Build comps table
  cat("Step 3/4  Fetching multiples for target and peers ...\n")
  comps <- tryCatch(
    calculate_comps(company_info, peers),
    error = function(e) {
      rlang::abort(paste0("Step 3 failed - could not build comps table: ",
                          conditionMessage(e)))
    }
  )
  cat("  OK  Comps table complete\n\n")

  # Step 4: Estimate value
  cat("Step 4/4  Estimating implied share price ...\n")
  valuation <- tryCatch(
    estimate_value(comps, company_info),
    error = function(e) {
      rlang::abort(paste0("Step 4 failed - could not estimate value: ",
                          conditionMessage(e)))
    }
  )
  cat(sprintf("  OK  Verdict: %s\n\n", valuation$verdict))

  result <- structure(
    list(
      company_info = company_info,
      peers        = peers,
      comps        = comps,
      valuation    = valuation
    ),
    class = "rdcf_result"
  )

  print(result)
  invisible(result)
}


#' @export
print.rdcf_result <- function(x, ...) {
  cat("==========================================================\n")
  cat("  RDCF VALUATION SUMMARY:", x$valuation$target, "\n")
  cat("==========================================================\n\n")

  ci <- x$company_info
  cat(sprintf("  Company    : %s\n", ci$ticker))
  cat(sprintf("  Sector     : %s\n", ci$sector))
  cat(sprintf("  Industry   : %s\n", ci$industry))
  cat(sprintf("  Market Cap : $%.2fB\n\n", ci$market_cap / 1e9))

  print(x$comps)
  cat("\n")
  print(x$valuation)
  cat("\n==========================================================\n")

  invisible(x)
}
