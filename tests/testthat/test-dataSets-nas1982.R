## ---- test-dataSets-nas1982
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)
    varnames <- c(
      "QUALITY",
      "NFACUL",
      "NGRADS",
      "PCTSUPP",
      "PCTGRT",
      "NARTIC",
      "PCTPUB"
    )
    mu <- c(
      27.63043,
      29.50000,
      68.04348,
      42.69565,
      23.71739,
      87.17391,
      68.93478
    )
    names(x = mu) <- varnames
    sigmacap <- matrix(
      data = c(
        101.43816,
        103.85556,
        181.48309,
        138.01836,
        128.64879,
        454.59903,
        90.93092,
        103.85556,
        275.23333,
        495.4,
        154.26667,
        49.16667,
        741.77778,
        52.43333,
        181.48309,
        495.4,
        1859.64251,
        342.34686,
        55.92367,
        1650.14783,
        113.8029,
        138.0184,
        154.2667,
        342.3469,
        554.4386,
        150.9121,
        510.543,
        126.0908,
        128.64879,
        49.16667,
        55.92367,
        150.91208,
        333.22947,
        471.2058,
        138.15894,
        454.599,
        741.7778,
        1650.1478,
        510.543,
        471.2058,
        3507.1691,
        542.3671,
        90.93092,
        52.43333,
        113.8029,
        126.09082,
        138.15894,
        542.36715,
        238.2401
      ),
      ncol = 7
    )
    colnames(x = sigmacap) <- varnames
    rownames(x = sigmacap) <- varnames
    testthat::test_that(
      desc = paste(text, "mu"),
      code = {
        testthat::expect_equal(
          object = colMeans(x = dataSets::nas1982),
          expected = mu,
          tolerance = tol
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "sigmacap"),
      code = {
        testthat::expect_equal(
          object = stats::var(x = dataSets::nas1982),
          expected = sigmacap,
          tolerance = tol
        )
      }
    )
  },
  text = "test-dataSets-nas1982",
  tol = 0.0001
)
