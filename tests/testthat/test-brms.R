data("BPG06dat")
data("simdat")

test_that("random a", {
  skip_on_cran()
  fit.randa<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",random.a=TRUE, cores=2,
                             iter = 500,
                              seed = 1234)
  expect_snapshot(extract.modmed.mlm.brms(fit.randa, "indirect")$CI)
})

test_that("random b", {
  skip_on_cran()
  fit.randb<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",random.b=TRUE, cores=2,
                             iter = 500,
                              seed = 1234)
   expect_snapshot(extract.modmed.mlm.brms(fit.randb, "indirect")$CI)
})

test_that("random a and b", {
  skip_on_cran()
  fit.randboth<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",
     random.a=TRUE, random.b=TRUE, cores=2, seed = 1234, iter = 500)
  expect_snapshot(extract.modmed.mlm.brms(fit.randboth, "indirect")$CI)
})

test_that("all random", {
  skip_on_cran()
  fit.randall<-modmed.mlm.brms(BPG06dat,"id", "x", "y", "m",
     random.a=TRUE, random.b=TRUE, random.cprime=TRUE, cores=2, seed = 1234,
     iter = 500)
   expect_snapshot(extract.modmed.mlm.brms(fit.randall, "indirect")$CI)
})

test_that("moderation of a", {
  skip_on_cran()
  # moderation for a path
  fitmoda<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
     random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
     moderator = "mod", mod.a=TRUE, cores=2, iter=500,
     seed = 1234)

  expect_snapshot(extract.modmed.mlm.brms(fitmoda, "indirect")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmoda, "indirect", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmoda, "indirect", modval1=0, modval2=1)$CI)

})

test_that("moderation of b", {
  skip_on_cran()
  fitmodb<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
    moderator = "mod", mod.b=TRUE, cores=2, iter = 500,
    seed = 1234)

  expect_snapshot(extract.modmed.mlm.brms(fitmodb, "indirect")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodb, "indirect", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodb, "indirect", modval1=0, modval2=1)$CI)
})


test_that("moderation of a and b", {
  skip_on_cran()
  # moderation for both a and b paths
  fitmodab<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
    moderator = "mod", mod.a=TRUE, mod.b=TRUE, cores=2, iter=500,
    seed = 1234)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "indirect")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "indirect", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "indirect.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "a")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "a", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "a.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "b")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "b", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab, "b.diff", modval1 = 0, modval2=1)$CI)

})

test_that("moderation of a and b, re for a int", {
  skip_on_cran()
  fitmodab2<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
     random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
     moderator = "mod", mod.a=TRUE, mod.b=TRUE,
     random.mod.a = TRUE, random.mod.m = TRUE, cores=2,
     iter = 2000,
     seed = 1234)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "indirect")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "indirect", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "indirect.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "a")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "a", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "a.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "b")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "b", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab2, "b.diff", modval1 = 0, modval2=1)$CI)

})

test_that("moderation of a and b, re for b int", {
  skip_on_cran()
  fitmodab3<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
    moderator = "mod", mod.a=TRUE, mod.b=TRUE,
    random.mod.b = TRUE, random.mod.y = TRUE, cores=2,
    iter = 2000,
    seed = 1234)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "indirect")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "indirect", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "indirect.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "a")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "a", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "a.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "b")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "b", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab3, "b.diff", modval1 = 0, modval2=1)$CI)
})

test_that("moderation of a and b, re for both", {
  skip_on_cran()
  fitmodab4<-modmed.mlm.brms(simdat,"L2id", "X", "Y", "M",
     random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
     moderator = "mod", mod.a=TRUE, mod.b=TRUE,
     random.mod.a = TRUE, random.mod.b = TRUE,
     random.mod.m = TRUE, random.mod.y = TRUE, cores=2,
     iter = 2000,
     seed = 1234)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "indirect")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "indirect", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "indirect.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "a")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "a", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "a.diff", modval1 = 0, modval2=1)$CI)

  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "b")$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "b", modval1=1)$CI)
  expect_snapshot(extract.modmed.mlm.brms(fitmodab4, "b.diff", modval1 = 0, modval2=1)$CI)
})
