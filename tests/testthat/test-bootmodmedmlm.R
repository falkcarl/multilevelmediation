data("BPG06dat")
data("simdat")

library(parallel)
library(boot)
ncpu<-6
RNGkind("L'Ecuyer-CMRG") # set type of random number generation that works in parallel
cl <- makeCluster(ncpu)
clusterSetRNGStream(cl, 9912)# set random number seeds for cluster

test_that("double boot, indirect only", {

  # bootstrap just the indirect effect
  boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
    L2ID = "id", X = "x", Y = "y", M = "m",
    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
    type="indirect",
    control=list(opt="nlm"),
    parallel="snow",ncpus=ncpu,cl=cl,
    boot.lvl="both")

  expect_snapshot(boot.result$t0)
  expect_snapshot(boot.ci(boot.result, index=1, type="perc"))
})

test_that("double boot, all", {

  # bootstrap just the indirect effect
  boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
                    L2ID = "id", X = "x", Y = "y", M = "m",
                    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                    type="all",
                    control=list(opt="nlm"),
                    parallel="snow",ncpus=ncpu,cl=cl,
                    boot.lvl="both")

  expect_snapshot(boot.result$t0)
  expect_snapshot(boot.ci(boot.result, index=1, type="perc"))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="a", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="b", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="cprime", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="covab", ci.conf=.95))

})

test_that("level 2 boot, indirect only", {

  # bootstrap just the indirect effect
  boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
                    L2ID = "id", X = "x", Y = "y", M = "m",
                    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                    type="indirect",
                    control=list(opt="nlm"),
                    parallel="snow",ncpus=ncpu,cl=cl,
                    boot.lvl="2")

  expect_snapshot(boot.result$t0)
  expect_snapshot(boot.ci(boot.result, index=1, type="perc"))
})

test_that("level 2 boot, all", {

  # bootstrap just the indirect effect
  boot.result<-boot(BPG06dat, statistic=boot.modmed.mlm, R=100,
                    L2ID = "id", X = "x", Y = "y", M = "m",
                    random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                    type="all",
                    control=list(opt="nlm"),
                    parallel="snow",ncpus=ncpu,cl=cl,
                    boot.lvl="2")

  expect_snapshot(boot.result$t0)
  expect_snapshot(boot.ci(boot.result, index=1, type="perc"))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="indirect", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="a", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="b", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="cprime", ci.conf=.95))
  expect_snapshot(extract.boot.modmed.mlm(boot.result, type="covab", ci.conf=.95))

})

test_that("bootresid", {

  set.seed(1234)
  # bootstrap just the indirect effect
  bootresid <- bootresid.modmed.mlm(BPG06dat,L2ID="id", X="x", Y="y", M="m",
                                    R=100, random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                                    control=list(opt="nlm")
  )

  expect_snapshot(extract.boot.modmed.mlm(bootresid, type="indirect"))

})
