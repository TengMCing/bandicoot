test_that("define_pkg_fn() can import function from library", {
  define_pkg_fn(cli_alert_info, symbol, pkg = cli)
  expect_equal(symbol$tick, cli::symbol$tick)
})


test_that("define_pkg_fn() can import function with custom name from library", {
  define_pkg_fn(info = cli_alert_info, symsym = symbol, pkg = cli)
  expect_equal(symsym$tick, cli::symbol$tick)
})

test_that("define_pkg_fn() accept pkg via named argument", {
  define_pkg_fn(info = cli_alert_info, symbol, pkg = cli)
  expect_equal(symbol$tick, cli::symbol$tick)
})


