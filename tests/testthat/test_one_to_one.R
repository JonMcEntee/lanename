context("one to one functions")

test_that( "kma_to_readable produces correct output", {
  expect_equal(kma_to_readable('CA_STK'), 'Stockton, CA')
  expect_equal(kma_to_readable('TX_DAL'), 'Dallas, TX')
  expect_equal(kma_to_readable('NY_ALB'), 'Albany, NY')
})
