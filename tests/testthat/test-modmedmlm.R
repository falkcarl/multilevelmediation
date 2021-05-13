data("BPG06dat")
data("simdat")

test_that("BPG rand a rand b", {

  fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
                  random.a=TRUE, random.b=TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type="indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type="a"))
  expect_snapshot(extract.modmed.mlm(fit, type="b"))
  expect_snapshot(extract.modmed.mlm(fit, type="covab"))
  expect_snapshot(extract.modmed.mlm(fit, type="cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type="fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov.vec"))
})

test_that("BPG rand a, rand b, rand c", {

  fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
                  random.a=TRUE, random.b=TRUE, random.cprime=TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type="indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type="a"))
  expect_snapshot(extract.modmed.mlm(fit, type="b"))
  expect_snapshot(extract.modmed.mlm(fit, type="covab"))
  expect_snapshot(extract.modmed.mlm(fit, type="cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type="fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov.vec"))
})

test_that("BPG rand a", {

  fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
                  random.a=TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type="indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type="a"))
  expect_snapshot(extract.modmed.mlm(fit, type="b"))
  expect_snapshot(extract.modmed.mlm(fit, type="cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type="fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov.vec"))
})

test_that("BPG rand b", {

  fit<-modmed.mlm(BPG06dat,"id", "x", "y", "m",
                  random.b=TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type="indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type="a"))
  expect_snapshot(extract.modmed.mlm(fit, type="b"))
  expect_snapshot(extract.modmed.mlm(fit, type="cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type="fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov"))
  expect_snapshot(extract.modmed.mlm(fit, type="recov.vec"))
})
