data("BPG06dat")
data("simdat")

test_that("custom double boot", {
  skip_on_cran()
  boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=100,
                    L2ID = "id", X = "x", Y = "y", M = "m",
                    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                    boot.type="caseboth",
                    parallel.type="parallel",ncores=2,seed=9912,
                    control=list(opt="nlm"))

  expect_snapshot(boot.result$t0)
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="a", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="b", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="cprime", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="covab", ci.conf=.95))
})

test_that("custom level 2 boot", {
  skip_on_cran()
  boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=100,
                                      L2ID = "id", X = "x", Y = "y", M = "m",
                                      random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                                      boot.type="case2",
                                      parallel.type="parallel",ncores=2,seed=9912,
                                      control=list(opt="nlm"))

  expect_snapshot(boot.result$t0)
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="a", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="b", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="cprime", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="covab", ci.conf=.95))
})


test_that("custom level 2 boot", {
  skip_on_cran()
  boot.result<-boot.modmed.mlm.custom(BPG06dat, nrep=100,
                                      L2ID = "id", X = "x", Y = "y", M = "m",
                                      random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                                      boot.type="resid",
                                      parallel.type="parallel",ncores=2,seed=9912,
                                      control=list(opt="nlm"))

  expect_snapshot(boot.result$t0)
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="a", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="b", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="cprime", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="covab", ci.conf=.95))
})
