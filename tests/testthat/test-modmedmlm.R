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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

# moderated mediation
test_that("moderated a", {
  skip_on_cran()
  fitmoda<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
                      random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                      moderator = "mod", mod.a=TRUE)

  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect", modval1=0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect", modval1=1)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect", modval1=0)-
                     extract.modmed.mlm(fitmoda, "indirect", modval1=1)) # should match prev line
  expect_snapshot(extract.modmed.mlm(fitmoda, "a", modval1=0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmoda, "a", modval1=1)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmoda, "a.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmoda, "a", modval1=0)-
                    extract.modmed.mlm(fitmoda, "a", modval1=1)) # should match prev line
  expect_snapshot(extract.modmed.mlm(fitmoda, "b", modval1=0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmoda, "b", modval1=1)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmoda, "b.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmoda, "b", modval1=0)-
                    extract.modmed.mlm(fitmoda, "b", modval1=1)) # should match prev line

})

test_that("moderated b", {
  skip_on_cran()
  fitmodb<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
                      random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                      moderator = "mod", mod.b=TRUE)
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect", modval1=0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect", modval1=1)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect", modval1=0)-
    extract.modmed.mlm(fitmodb, "indirect", modval1=1)) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodb, "a", modval1=0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodb, "a", modval1=1)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodb, "a.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodb, "a", modval1=0)-
    extract.modmed.mlm(fitmodb, "a", modval1=1))  # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodb, "b", modval1=0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodb, "b", modval1=1)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodb, "b.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodb, "b", modval1=0)-
    extract.modmed.mlm(fitmodb, "b", modval1=1))  # should match prev line
})


test_that("moderated a and b", {

  fitmodab<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
                       random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                       moderator = "mod", mod.a=TRUE, mod.b=TRUE)

  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect", modval1=0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect", modval1=1)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect", modval1=0)-
    extract.modmed.mlm(fitmodab, "indirect", modval1=1)) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab, "a", modval1=0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab, "a", modval1=1)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab, "a.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab, "a", modval1=0)-
    extract.modmed.mlm(fitmodab, "a", modval1=1))  # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab, "b", modval1=0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab, "b", modval1=1)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab, "b.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab, "b", modval1=0)-
    extract.modmed.mlm(fitmodab, "b", modval1=1))  # should match prev line

})

test_that("moderated a and b rand interaction a", {
  skip_on_cran()
  fitmodab2<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
                        random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                        moderator = "mod", mod.a=TRUE, mod.b=TRUE,
                        random.mod.a = TRUE, random.mod.m = TRUE)

  expect_snapshot(extract.modmed.mlm(fitmodab2, "indirect", modval1=0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab2, "indirect", modval1=1)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab2, "indirect.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab2, "indirect", modval1=0)-
    extract.modmed.mlm(fitmodab2, "indirect", modval1=1)) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab2, "a", modval1=0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab2, "a", modval1=1)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab2, "a.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab2, "a", modval1=0)-
    extract.modmed.mlm(fitmodab2, "a", modval1=1))  # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab2, "b", modval1=0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab2, "b", modval1=1)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab2, "b.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab2, "b", modval1=0)-
    extract.modmed.mlm(fitmodab2, "b", modval1=1))  # should match prev line
})

test_that("moderated a and b rand interaction b", {
  skip_on_cran()
  fitmodab3<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
                        random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                        moderator = "mod", mod.a=TRUE, mod.b=TRUE,
                        random.mod.b = TRUE, random.mod.y = TRUE)

  expect_snapshot(extract.modmed.mlm(fitmodab3, "indirect", modval1=0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab3, "indirect", modval1=1)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab3, "indirect.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab3, "indirect", modval1=0)-
    extract.modmed.mlm(fitmodab3, "indirect", modval1=1)) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab3, "a", modval1=0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab3, "a", modval1=1)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab3, "a.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab3, "a", modval1=0)-
    extract.modmed.mlm(fitmodab3, "a", modval1=1))  # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab3, "b", modval1=0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab3, "b", modval1=1)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab3, "b.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab3, "b", modval1=0)-
    extract.modmed.mlm(fitmodab3, "b", modval1=1))  # should match prev line

})


test_that("moderated a and b rand interaction both", {
  skip_on_cran()
  fitmodab4<-modmed.mlm(simdat,"L2id", "X", "Y", "M",
                        random.a=TRUE, random.b=TRUE, random.cprime=TRUE,
                        moderator = "mod", mod.a=TRUE, mod.b=TRUE,
                        random.mod.a = TRUE, random.mod.b = TRUE,
                        random.mod.m = TRUE, random.mod.y = TRUE)

  expect_snapshot(extract.modmed.mlm(fitmodab4, "indirect", modval1=0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab4, "indirect", modval1=1)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab4, "indirect.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab4, "indirect", modval1=0)-
    extract.modmed.mlm(fitmodab4, "indirect", modval1=1)) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab4, "a", modval1=0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab4, "a", modval1=1)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab4, "a.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab4, "a", modval1=0)-
    extract.modmed.mlm(fitmodab4, "a", modval1=1))  # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab4, "b", modval1=0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab4, "b", modval1=1)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab4, "b.diff",
                     modval1 = 0, modval2=1)) # should match difference between the two above?
  expect_snapshot(extract.modmed.mlm(fitmodab4, "b", modval1=0)-
    extract.modmed.mlm(fitmodab4, "b", modval1=1))  # should match prev line

})

# to see code coverage of tests
#library(covr)
#covr <- package_coverage(path="./path/to/package")
#covr <- package_coverage(path=".")
#covr
#report(covr)
