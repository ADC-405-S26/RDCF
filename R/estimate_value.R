#' Estimate Implied Share Price from Comparable Company Multiples
#'
#' Applies the peer group's median valuation multiples to the target company's
#' financial metrics to produce an implied share price for each multiple. The
#' result is a valuation range showing where the target should trade if the
#' market valued it in line with its peers.
#'
#' @param comps A list of class \code{"rdcf_comps"} as returned by
#'   \code{calculate_comps}.
#' @param company_info A list of class \code{"rdcf_company_info"} as returned
#'   by \code{get_company_info}.
#'
#' @return A list of class \code{"rdcf_valuation"} containing:
#' \describe{
#'   \item{target}{The target company ticker.}
#'   \item{current_price}{The current market price of the target.}
#'   \item{implied_prices}{A named numeric vector with one implied price per
#'     multiple. \code{NA} where a multiple or required financial metric
#'     is unavailable.}
#'   \item{implied_low}{The lowest implied price across all valid multiples.}
#'   \item{implied_high}{The highest implied price across all valid multiples.}
#'   \item{implied_median}{The median implied price across all valid multiples.}
#'   \item{vs_current}{Percentage premium or discount of the median implied
#'     price versus the current price. Positive means undervalued.}
#'   \item{verdict}{A character string: Undervalued, Overvalued, or Fairly Valued.}
#' }
#'
#' @examples
#' \dontrun{
#' info      <- get_company_info("AAPL")
#' peers     <- get_peers(info, n = 5)
#' comps     <- calculate_comps(info, peers)
#' valuation <- estimate_value(comps, info)
#' valuation$verdict
#' valuation$implied_median
#' }
#'
#' @importFrom rlang abort warn
#' @export
estimate_value <- function(comps, company_info) {

  # Input validation
  checkmate::assert_class(comps, "rdcf_comps")
  checkmate::assert_class(company_info, "rdcf_company_info")

  if (comps$target != company_info$ticker) {
    rlang::abort(paste0(
      "Mismatch: comps is for '", comps$target,
      "' but company_info is for '", company_info$ticker, "'. ",
      "Make sure both arguments refer to the same company."
    ))
  }

  target         <- comps$target
  current_price  <- company_info$current_price
  shares         <- company_info$shares_outstanding
  revenue        <- company_info$revenue
  ebitda         <- company_info$ebitda
  net_income     <- company_info$net_income
  enterprise_val <- company_info$enterprise_value
  book_val_ps    <- company_info$book_value_per_share

  # EPS derived from net income and shares outstanding
  eps <- if (!is.na(net_income) && !is.na(shares) && shares > 0) {
    net_income / shares
  } else {
    NA_real_
  }

  # Net debt = enterprise value minus market cap
  net_debt <- if (!is.na(enterprise_val) && !is.na(company_info$market_cap)) {
    enterprise_val - company_info$market_cap
  } else {
    NA_real_
  }

  # Pull peer median multiples from summary stats
  med <- comps$summary_stats[comps$summary_stats$statistic == "Median", ]# P/E implied price
  pe_implied <- if (!is.na(med$pe_ratio) && !is.na(eps) && eps > 0) {
    med$pe_ratio * eps
  } else {
    NA_real_
  }

  # EV/EBITDA implied price
  ev_implied <- if (!is.na(med$ev_ebitda) && !is.na(ebitda) && ebitda > 0 &&
                    !is.na(net_debt) && !is.na(shares) && shares > 0) {
    (med$ev_ebitda * ebitda - net_debt) / shares
  } else {
    NA_real_
  }

  # P/S implied price
  ps_implied <- if (!is.na(med$ps_ratio) && !is.na(revenue) && revenue > 0 &&
                    !is.na(shares) && shares > 0) {
    med$ps_ratio * revenue / shares
  } else {
    NA_real_
  }

  # P/B implied price
  pb_implied <- if (!is.na(med$pb_ratio) && !is.na(book_val_ps) &&
                    book_val_ps > 0) {
    med$pb_ratio * book_val_ps
  } else {
    NA_real_
  }

  implied_prices <- c(
    PE        = pe_implied,
    EV_EBITDA = ev_implied,
    PS        = ps_implied,
    PB        = pb_implied
  )

  valid_implied <- implied_prices[!is.na(implied_prices) & implied_prices > 0]

  if (length(valid_implied) == 0) {
    rlang::warn(paste0(
      "Could not compute any implied prices for '", target, "'. ",
      "This may be due to missing financial data such as negative earnings."
    ))
    implied_low    <- NA_real_
    implied_high   <- NA_real_
    implied_median <- NA_real_
    vs_current     <- NA_real_
    verdict        <- "Insufficient Data"
  } else {
    implied_low    <- min(valid_implied)
    implied_high   <- max(valid_implied)
    implied_median <- stats::median(valid_implied)
    vs_current     <- (implied_median - current_price) / current_price * 100

    verdict <- if (vs_current > 15) {
      "Undervalued"
    } else if (vs_current < -15) {
      "Overvalued"
    } else {
      "Fairly Valued"
    }
  }

  structure(
    list(
      target         = target,
      current_price  = current_price,
      implied_prices = implied_prices,
      implied_low    = implied_low,
      implied_high   = implied_high,
      implied_median = implied_median,
      vs_current     = vs_current,
      verdict        = verdict
    ),
    class = "rdcf_valuation"
  )
}


#' @export
print.rdcf_valuation <- function(x, ...) {
  fmt_p   <- function(v) if (is.na(v)) "    N/A" else sprintf("$%6.2f", v)
  fmt_pct <- function(v) if (is.na(v)) "N/A"     else sprintf("%+.1f%%", v)

  cat("RDCF Valuation:", x$target, "\n\n")
  cat(sprintf("  Current Price     : $%.2f\n\n", x$current_price))
  cat("  Implied Prices by Multiple:\n")
  cat(sprintf("    P/E             : %s\n", fmt_p(x$implied_prices["PE"])))
  cat(sprintf("    EV/EBITDA       : %s\n", fmt_p(x$implied_prices["EV_EBITDA"])))
  cat(sprintf("    P/S             : %s\n", fmt_p(x$implied_prices["PS"])))
  cat(sprintf("    P/B             : %s\n", fmt_p(x$implied_prices["PB"])))
  cat("\n")
  cat(sprintf("  Implied Range     : %s  to  %s\n",
              fmt_p(x$implied_low), fmt_p(x$implied_high)))
  cat(sprintf("  Implied Median    : %s\n",  fmt_p(x$implied_median)))
  cat(sprintf("  vs. Current Price : %s\n",  fmt_pct(x$vs_current)))
  cat(sprintf("\n  Verdict           : %s\n", x$verdict))
  invisible(x)
}
