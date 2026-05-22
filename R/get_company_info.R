#' Get Company Information from Yahoo Finance
#'
#' Retrieves key information about a publicly traded company including its
#' sector, industry, market capitalisation, and financial metrics needed for
#' comparable company analysis. Data is sourced from Yahoo Finance via the
#' yfinancer package.
#'
#' @param ticker Character. A valid stock ticker symbol (e.g., \code{"AAPL"},
#'   \code{"MSFT"}). Case-insensitive.
#'
#' @return A named list of class \code{"rdcf_company_info"} containing:
#' \describe{
#'   \item{ticker}{The ticker symbol used (uppercased).}
#'   \item{name}{Full company name.}
#'   \item{sector}{The sector the company operates in (e.g., "Technology").}
#'   \item{industry}{The specific industry (e.g., "Consumer Electronics").}
#'   \item{market_cap}{Market capitalisation in USD.}
#'   \item{enterprise_value}{Enterprise value in USD.}
#'   \item{revenue}{Most recent annual revenue in USD.}
#'   \item{ebitda}{Most recent annual EBITDA in USD.}
#'   \item{net_income}{Most recent annual net income in USD.}
#'   \item{shares_outstanding}{Total shares outstanding.}
#'   \item{current_price}{Most recent closing stock price in USD.}
#'   \item{book_value_per_share}{Book value per share in USD.}
#' }
#'
#' @details
#' This function is typically the first step in a comps valuation workflow.
#' The sector and industry fields are used by \code{get_peers} to identify
#' comparable companies automatically.
#'
#' @examples
#' \dontrun{
#' info <- get_company_info("AAPL")
#' info$sector    # "Technology"
#' info$industry  # "Consumer Electronics"
#' }
#'
#' @importFrom rlang abort warn
#' @export
get_company_info <- function(ticker) {

  # Input validation
  checkmate::assert_string(ticker, min.chars = 1)
  ticker <- toupper(trimws(ticker))

  # Fetch from Yahoo Finance
  message("Fetching company info for ", ticker, " ...")

  ticker_obj <- tryCatch(
    yfinancer::get_tickers(ticker),
    error = function(e) {
      rlang::abort(paste0(
        "Could not find ticker '", ticker, "' on Yahoo Finance. ",
        "Please check the symbol and try again.\n",
        "Original error: ", conditionMessage(e)
      ))
    }
  )

  info <- tryCatch(
    yfinancer::get_info(ticker_obj, modules = c(
      "summaryProfile",
      "summaryDetail",
      "defaultKeyStatistics",
      "financialData"
    )),
    error = function(e) {
      rlang::abort(paste0(
        "Could not retrieve company information for '", ticker, "'. ",
        "Check your internet connection and try again.\n",
        "Original error: ", conditionMessage(e)
      ))
    }
  )

  # Extract fields safely
  safe_get <- function(lst, ...) {
    keys <- c(...)
    val <- lst
    for (k in keys) {
      if (is.null(val) || !k %in% names(val)) return(NA_real_)
      val <- val[[k]]
    }
    if (is.null(val)) NA_real_ else val
  }

  profile  <- if ("summaryProfile"       %in% names(info)) info[["summaryProfile"]]       else list()
  detail   <- if ("summaryDetail"        %in% names(info)) info[["summaryDetail"]]         else list()
  stats    <- if ("defaultKeyStatistics" %in% names(info)) info[["defaultKeyStatistics"]]  else list()
  fin_data <- if ("financialData"        %in% names(info)) info[["financialData"]]         else list()

  # Current price from quantmod as a reliable fallback
  current_price <- tryCatch({
    px <- quantmod::getSymbols(ticker, src = "yahoo",
                               from = Sys.Date() - 7,
                               to   = Sys.Date(),
                               auto.assign = FALSE)
    as.numeric(quantmod::Cl(px)[nrow(px)])
  }, error = function(e) {
    as.numeric(safe_get(detail, "regularMarketPrice"))
  })

  structure(
    list(
      ticker               = ticker,
      name                 = ticker,
      sector               = safe_get(profile,  "sector"),
      industry             = safe_get(profile,  "industry"),
      market_cap           = as.numeric(safe_get(detail,   "marketCap")),
      enterprise_value     = as.numeric(safe_get(stats,    "enterpriseValue")),
      revenue              = as.numeric(safe_get(fin_data, "totalRevenue")),
      ebitda               = as.numeric(safe_get(fin_data, "ebitda")),
      net_income           = as.numeric(safe_get(fin_data, "netIncomeToCommon")),
      shares_outstanding   = as.numeric(safe_get(stats,    "sharesOutstanding")),
      current_price        = current_price,
      book_value_per_share = as.numeric(safe_get(stats,    "bookValue"))
    ),
    class = "rdcf_company_info"
  )
}


#' @export
print.rdcf_company_info <- function(x, ...) {
  cat("RDCF Company Info:", x$ticker, "\n")
  cat(sprintf("  Sector           : %s\n", x$sector))
  cat(sprintf("  Industry         : %s\n", x$industry))
  cat(sprintf("  Current Price    : $%.2f\n", x$current_price))
  cat(sprintf("  Market Cap       : $%.2fB\n", x$market_cap / 1e9))
  cat(sprintf("  Enterprise Value : $%.2fB\n", x$enterprise_value / 1e9))
  invisible(x)
}
