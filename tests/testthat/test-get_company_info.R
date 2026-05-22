test_that("get_company_info rejects invalid ticker types", {
  expect_error(get_company_info(123))
  expect_error(get_company_info(NULL))
  expect_error(get_company_info(""))
  expect_error(get_company_info(c("AAPL", "MSFT")))
})

test_that("get_company_info upcases the ticker", {
  skip_if_offline()
  result <- get_company_info("aapl")
  expect_equal(result$ticker, "AAPL")
})

test_that("get_company_info returns rdcf_company_info class", {
  skip_if_offline()
  result <- get_company_info("AAPL")
  expect_s3_class(result, "rdcf_company_info")
})

test_that("get_company_info returns expected fields", {
  skip_if_offline()
  result <- get_company_info("AAPL")
  expect_named(result, c("ticker", "name", "sector", "industry",
                         "market_cap", "enterprise_value", "revenue",
                         "ebitda", "net_income", "shares_outstanding",
                         "current_price", "book_value_per_share"))
})

test_that("get_company_info returns non-NA sector and industry for AAPL", {
  skip_if_offline()
  result <- get_company_info("AAPL")
  expect_false(is.na(result$sector))
  expect_false(is.na(result$industry))
})
