test_that("multiplication works", {

  df <- data.frame(USUBJID='01-701-1015', SAFFL= 'Y',   SEX='F', ARM='Placebo',AGE=63, DMDY=-7)
  dfs <- my_summmary(df,SAFFL=='Y', SEX=='F',gcols='ARM', acols=c('AGE'), param=name)

  expect_equal(dfs[[1]]$mean, 63)
  expect_equal(dfs[[1]]$n, 1)

  expect_false(is.na(dfs[[1]]$mean))
  expect_true(!is.na(dfs[[1]]$mean))

  expect_error(my_summmary(adsl,SAFFL=='Y', SEX=='F',gcols='ARM', acols=c('AGE'), param=name))

})

