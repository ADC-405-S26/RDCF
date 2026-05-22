#' Find Peer Companies for Comparable Company Analysis
#'
#' Automatically identifies publicly traded peer companies in the same industry
#' as the target company. Peers are selected from a representative universe of
#' large-cap stocks and filtered to companies in the same industry, then ranked
#' by similarity in market capitalisation to the target.
#'
#' @param company_info A list of class \code{"rdcf_company_info"} as returned
#'   by \code{get_company_info}.
#' @param n Integer. Maximum number of peer companies to return. Defaults to
#'   \code{5}.
#' @param market_cap_range Numeric. How wide a market cap band to search within,
#'   as a multiplier. For example, \code{10} means peers with market cap between
#'   1/10x and 10x the target are considered. Defaults to \code{10}.
#'
#' @return A character vector of ticker symbols of class \code{"rdcf_peers"}.
#'
#' @details
#' The function screens a large-cap stock universe for companies in the same
#' industry as the target, filters by market cap proximity, and returns the
#' closest matches. If no industry-level peers are found it falls back to
#' sector-level matching.
#'
#' Because this function makes multiple API calls it may take 30-60 seconds.
#'
#' @examples
#' \dontrun{
#' info  <- get_company_info("AAPL")
#' peers <- get_peers(info, n = 5)
#' peers
#' }
#'
#' @importFrom rlang abort warn
#' @export
get_peers <- function(company_info, n = 5, market_cap_range = 10) {

  # Input validation
  checkmate::assert_class(company_info, "rdcf_company_info")
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_number(market_cap_range, lower = 1)

  if (is.na(company_info$industry) || is.na(company_info$sector)) {
    rlang::abort(paste0(
      "Cannot find peers: sector/industry information is missing for '",
      company_info$ticker, "'. ",
      "Try running get_company_info() again or check the ticker symbol."
    ))
  }

  target_ticker   <- company_info$ticker
  target_industry <- company_info$industry
  target_mktcap   <- company_info$market_cap

  message("Finding peers for ", target_ticker,
          " in industry: ", target_industry, " ...")

  # Large-cap stock universe to screen
  universe <- c(
    "MSFT", "GOOGL", "AMZN", "NVDA", "META", "TSLA", "LLY", "JPM",
    "V", "XOM", "UNH", "JNJ", "MA", "PG", "HD", "CVX", "MRK", "ABBV",
    "COST", "PEP", "KO", "AVGO", "WMT", "BAC", "PFE", "TMO", "CSCO",
    "MCD", "ACN", "ABT", "CRM", "DHR", "NEE", "LIN", "TXN", "PM",
    "HON", "UPS", "LOW", "QCOM", "CAT", "SBUX", "GE", "IBM", "INTC",
    "AMD", "INTU", "SPGI", "GS", "ISRG", "BLK", "AXP", "DE", "GILD",
    "ADI", "MDLZ", "REGN", "VRTX", "PANW", "SYK", "ZTS", "TMUS",
    "T", "VZ", "DIS", "CMCSA", "NFLX", "NOW", "SNPS", "KLAC", "AMAT",
    "LRCX", "MRVL", "MU", "ORCL", "SAP", "ADBE", "CRM", "WDAY",
    "SHOP", "SQ", "PYPL", "COIN", "HOOD", "UBER", "LYFT", "ABNB",
    "BKNG", "EXPE", "MAR", "HLT", "F", "GM", "TM", "HMC", "STLA",
    "NKE", "LULU", "TGT", "AMGN", "BIIB", "MRNA", "BMY", "AZN",
    "GSK", "NVO", "SNY", "C", "WFC", "MS", "BX", "KKR", "APO"
  )

  # Remove the target itself
  universe <- universe[universe != target_ticker]

  message("Screening ", length(universe), " companies for industry match ...")

  # Screen each ticker for industry match
  peer_candidates <- list()

  for (tkr in universe) {
    info <- tryCatch({
      obj <- yfinancer::get_tickers(tkr)
      yfinancer::get_info(obj, modules = c("summaryProfile", "summaryDetail"))
    }, error = function(e) NULL)

    if (is.null(info)) next

    industry <- tryCatch(
      info[["summaryProfile"]][["industry"]],
      error = function(e) NA
    )
    mktcap <- tryCatch(
      as.numeric(info[["summaryDetail"]][["marketCap"]]),
      error = function(e) NA_real_
    )

    if (is.na(industry) || industry != target_industry) next

    # Market cap filter
    if (!is.na(target_mktcap) && !is.na(mktcap)) {
      ratio <- mktcap / target_mktcap
      if (ratio < (1 / market_cap_range) || ratio > market_cap_range) next
    }

    peer_candidates[[tkr]] <- list(ticker = tkr, market_cap = mktcap)
    message("  Found peer: ", tkr)

    if (length(peer_candidates) >= n * 3) break
  }

  # Fall back to sector level if no industry peers found
  if (length(peer_candidates) == 0) {
    rlang::warn(paste0(
      "No peers found in industry '", target_industry, "'. ",
      "Falling back to sector: '", company_info$sector, "'."
    ))

    for (tkr in universe) {
      info <- tryCatch({
        obj <- yfinancer::get_tickers(tkr)
        yfinancer::get_info(obj, modules = c("summaryProfile", "summaryDetail"))
      }, error = function(e) NULL)

      if (is.null(info)) next

      sector <- tryCatch(
        info[["summaryProfile"]][["sector"]],
        error = function(e) NA
      )
      mktcap <- tryCatch(
        as.numeric(info[["summaryDetail"]][["marketCap"]]),
        error = function(e) NA_real_
      )

      if (is.na(sector) || sector != company_info$sector) next

      peer_candidates[[tkr]] <- list(ticker = tkr, market_cap = mktcap)
      message("  Found sector peer: ", tkr)

      if (length(peer_candidates) >= n * 2) break
    }
  }

  if (length(peer_candidates) == 0) {
    rlang::abort(paste0(
      "Could not find any peers for '", target_ticker, "'. ",
      "The company may be in a niche industry not in the screening universe."
    ))
  }

  # Rank by market cap proximity to target
  peers_df <- do.call(rbind, lapply(peer_candidates, function(p) {
    data.frame(
      ticker  = p$ticker,
      mc_diff = abs(p$market_cap - target_mktcap),
      stringsAsFactors = FALSE
    )
  }))

  peers_df <- peers_df[order(peers_df$mc_diff), ]
  result   <- peers_df$ticker[seq_len(min(n, nrow(peers_df)))]

  message("Selected ", length(result), " peers: ", paste(result, collapse = ", "))

  structure(result, class = "rdcf_peers",
            target   = target_ticker,
            industry = target_industry)
}


#' @export
print.rdcf_peers <- function(x, ...) {
  cat("RDCF Peers for", attr(x, "target"), "\n")
  cat("  Industry :", attr(x, "industry"), "\n")
  cat("  Peers    :", paste(x, collapse = ", "), "\n")
  invisible(x)
}
