# TODO: compare outputs from nlme against glmmTMB
# TODO: load prefit models using setup.R instead of rerunning within each test? (numbers can be more precise, rather than set looser tolerances)

test_that("Default extract.modmed.mlm", {
  fit <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  expect_snapshot_value(extract.modmed.mlm(fit), style = "json2")
})

# TODO: Improve error handling for missing/default args in `compute.indirect`:
# TODO: Better error handling: type = "covar" gives 'object covab not found' error when random.a and random.b are missing
# TODO: Better error handling: type = "indirect.diff" gives 'object ab2 not found' when mod.a and mod.b are not TRUE (also true for other ".diff" types)
# TODO: a.diff returns zero (should probs give missing error or somethign)
# TODO: b.diff returns zero (should probs give missing error or somethign)
# TODO: cprime.diff returns zero (should probs give missing error or somethign)
test_that("returns for all, fixef, recov, recov.vec, indirect, a, b, cprime are correct", {
  fit <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "all"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "fixef"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov.vec"), style = "json2")
  expect_equal(extract.modmed.mlm(fit, type = "indirect"), 0.422035, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "a"), 0.621841, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "b"), 0.678686, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "cprime"), 0.289264, tolerance = 1e-4)
})


# TODO: add more tests for correct number of NAs in output (eg with covariates, moderators, random paths, etc.)
test_that("Correct NA output", {
  fit1 <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  fit2 <- fit1
  fit1$conv <- FALSE
  fit2$model <- NULL
  # fit1, conv=FALSE
  expect_equal(extract.modmed.mlm(fit1, type = "all"), rep(NA, 9))
  expect_equal(extract.modmed.mlm(fit1, type = "fixef"), rep(NA, 5))
  expect_equal(extract.modmed.mlm(fit1, type = "recov"), matrix(NA, 4, 4))
  expect_equal(extract.modmed.mlm(fit1, type = "recov.vec"), rep(NA, 4))
  expect_equal(extract.modmed.mlm(fit1, type = "indirect"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "a"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "b"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "cprime"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "covab"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "indirect.diff"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "a.diff"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "b.diff"), NA)
  expect_equal(extract.modmed.mlm(fit1, type = "cprime.diff"), NA)
  # fit2, model=NULL
  expect_equal(extract.modmed.mlm(fit2, type = "all"), rep(NA, 9))
  expect_equal(extract.modmed.mlm(fit2, type = "fixef"), rep(NA, 5))
  expect_equal(extract.modmed.mlm(fit2, type = "recov"), matrix(NA, 4, 4))
  expect_equal(extract.modmed.mlm(fit2, type = "recov.vec"), rep(NA, 4))
  expect_equal(extract.modmed.mlm(fit2, type = "indirect"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "a"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "b"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "cprime"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "covab"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "indirect.diff"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "a.diff"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "b.diff"), NA)
  expect_equal(extract.modmed.mlm(fit2, type = "cprime.diff"), NA)
})

test_that("BPG rand a rand b", {
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.a = TRUE, random.b = TRUE)

  expect_snapshot_value(extract.modmed.mlm(fit), style = "json2", tolerance = 1e-7)
  expect_equal(extract.modmed.mlm(fit, type = "indirect"), 0.452432, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "a"), 0.61202037, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "b"), 0.58803691, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "covab"), 0.092541, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "cprime"), 0.24497536, tolerance = 1e-4)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "fixef"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov"), style = "json2", tolerance = 1e-7)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov.vec"), style = "json2", tolerance = 1e-7)
})

test_that("BPG rand a, rand b, rand c", {
  skip_on_cran()
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.a = TRUE, random.b = TRUE, random.cprime = TRUE)

  expect_snapshot_value(extract.modmed.mlm(fit), style = "json2", tolerance = 1e-7)
  expect_equal(extract.modmed.mlm(fit, type = "indirect"), 0.47420813, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "a"), 0.61189252, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "b"), 0.61176677, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "covab"), 0.09987261, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "cprime"), 0.21935512, tolerance = 1e-4)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "fixef"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov"), style = "json2", tolerance = 1e-7)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov.vec"), style = "json2", tolerance = 1e-7)
})

test_that("BPG rand a", {
  skip_on_cran()
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.a = TRUE)

  expect_snapshot_value(extract.modmed.mlm(fit), style = "json2")
  expect_equal(extract.modmed.mlm(fit, type = "indirect"), 0.41880466, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "a"), 0.61121679, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "b"), 0.68519822, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "cprime"), 0.2844941, tolerance = 1e-4)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "fixef"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov"), style = "json2", tolerance = 1e-7)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov.vec"), style = "json2", tolerance = 1e-7)
})

test_that("BPG rand b", {
  skip_on_cran()
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.b = TRUE)

  expect_snapshot_value(extract.modmed.mlm(fit), style = "json2")
  expect_equal(extract.modmed.mlm(fit, type = "indirect"), 0.37759856, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "a"), 0.6212775, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "b"), 0.60777762, tolerance = 1e-4)
  expect_equal(extract.modmed.mlm(fit, type = "cprime"), 0.26163174, tolerance = 1e-4)
  expect_snapshot_value(extract.modmed.mlm(fit, type = "fixef"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov"), style = "json2")
  expect_snapshot_value(extract.modmed.mlm(fit, type = "recov.vec"), style = "json2")
})

# moderated mediation
test_that("moderated a", {
  skip_on_cran()
  fitmoda <- modmed.mlm(
    simdat,
    "L2id",
    "X",
    "Y",
    "M",
    random.a = TRUE,
    random.b = TRUE,
    random.cprime = TRUE,
    moderator = "mod",
    mod.a = TRUE
  )

  expect_equal(extract.modmed.mlm(fitmoda, "indirect", modval1 = 0.0), 0.20640477, tolerance = 1e-4) # indirect effect when moderator = 0
  expect_equal(extract.modmed.mlm(fitmoda, "indirect", modval1 = 1.0), 0.24794896, tolerance = 1e-4) # indirect effect when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmoda, "indirect.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmoda, "indirect", modval1 = 0.0) - extract.modmed.mlm(fitmoda, "indirect", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmoda, "a", modval1 = 0.0), 0.28245721, tolerance = 1e-4) # a when moderator = 0
  expect_equal(extract.modmed.mlm(fitmoda, "a", modval1 = 1.0), 0.39881544, tolerance = 1e-4) # a when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmoda, "a.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmoda, "a", modval1 = 0.0) - extract.modmed.mlm(fitmoda, "a", modval1 = 1.0)
  ) # should match difference between the two above?

  # Values shouldn't change, since mod.b is not set
  expect_equal(extract.modmed.mlm(fitmoda, "b", modval1 = 0.0), 0.35703693, tolerance = 1e-4) # b when moderator = 0
  expect_equal(extract.modmed.mlm(fitmoda, "b", modval1 = 1.0), 0.35703693, tolerance = 1e-4) # b when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmoda, "b.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmoda, "b", modval1 = 0.0) - extract.modmed.mlm(fitmoda, "b", modval1 = 1.0)
  ) # should match difference between the two above?
})

test_that("moderated b", {
  skip_on_cran()
  fitmodb <- modmed.mlm(
    simdat,
    "L2id",
    "X",
    "Y",
    "M",
    random.a = TRUE,
    random.b = TRUE,
    random.cprime = TRUE,
    moderator = "mod",
    mod.b = TRUE
  )
  expect_equal(extract.modmed.mlm(fitmodb, "indirect", modval1 = 0.0), 0.30298, tolerance = 1e-4) # indirect effect when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodb, "indirect", modval1 = 1.0), 0.18803, tolerance = 1e-4) # indirect effect when moderator = 1 #TV: needed slightly higher tolerance for some reason
  expect_equal(
    extract.modmed.mlm(fitmodb, "indirect.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodb, "indirect", modval1 = 0.0) - extract.modmed.mlm(fitmodb, "indirect", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodb, "a", modval1 = 0.0), 0.34667579, tolerance = 1e-4) # a when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodb, "a", modval1 = 1.0), 0.34667579, tolerance = 1e-4) # a when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodb, "a.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodb, "a", modval1 = 0.0) - extract.modmed.mlm(fitmodb, "a", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodb, "b", modval1 = 0.0), 0.53906969, tolerance = 1e-4) # b when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodb, "b", modval1 = 1.0), 0.20750043, tolerance = 1e-4) # b when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodb, "b.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodb, "b", modval1 = 0.0) - extract.modmed.mlm(fitmodb, "b", modval1 = 1.0)
  ) # should match difference between the two above?
})


test_that("moderated a and b", {
  fitmodab <- modmed.mlm(
    simdat,
    "L2id",
    "X",
    "Y",
    "M",
    random.a = TRUE,
    random.b = TRUE,
    random.cprime = TRUE,
    moderator = "mod",
    mod.a = TRUE,
    mod.b = TRUE
  )

  expect_equal(extract.modmed.mlm(fitmodab, "indirect", modval1 = 0.0), 0.27049018, tolerance = 1e-4) # indirect effect when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab, "indirect", modval1 = 1.0), 0.2007316, tolerance = 1e-4) # indirect effect when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab, "indirect.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab, "indirect", modval1 = 0.0) - extract.modmed.mlm(fitmodab, "indirect", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab, "a", modval1 = 0.0), 0.28311503, tolerance = 1e-4) # a when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab, "a", modval1 = 1.0), 0.39985538, tolerance = 1e-4) # a when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab, "a.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab, "a", modval1 = 0.0) - extract.modmed.mlm(fitmodab, "a", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab, "b", modval1 = 0.0), 0.53840964, tolerance = 1e-4) # b when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab, "b", modval1 = 1.0), 0.20675796, tolerance = 1e-4) # b when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab, "b.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab, "b", modval1 = 0.0) - extract.modmed.mlm(fitmodab, "b", modval1 = 1.0)
  ) # should match difference between the two above?
})

test_that("moderated a and b rand interaction a", {
  skip_on_cran()
  fitmodab2 <- modmed.mlm(
    simdat,
    "L2id",
    "X",
    "Y",
    "M",
    random.a = TRUE,
    random.b = TRUE,
    random.cprime = TRUE,
    moderator = "mod",
    mod.a = TRUE,
    mod.b = TRUE,
    random.mod.a = TRUE,
    random.mod.m = TRUE
  )

  expect_equal(extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0.0), 0.30885, tolerance = 1e-4) # indirect effect when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1.0), 0.16141, tolerance = 1e-4) # indirect effect when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab2, "indirect.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0.0) - extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab2, "a", modval1 = 0.0), 0.28676, tolerance = 1e-4) # a when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab2, "a", modval1 = 1.0), 0.38067, tolerance = 1e-4) # a when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab2, "a.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab2, "a", modval1 = 0.0) - extract.modmed.mlm(fitmodab2, "a", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab2, "b", modval1 = 0.0), 0.53246910, tolerance = 1e-4) # b when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab2, "b", modval1 = 1.0), 0.20994, tolerance = 1e-4) # b when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab2, "b.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab2, "b", modval1 = 0.0) - extract.modmed.mlm(fitmodab2, "b", modval1 = 1.0)
  ) # should match difference between the two above?
})

test_that("moderated a and b rand interaction b", {
  skip_on_cran()
  fitmodab3 <- modmed.mlm(
    simdat,
    "L2id",
    "X",
    "Y",
    "M",
    random.a = TRUE,
    random.b = TRUE,
    random.cprime = TRUE,
    moderator = "mod",
    mod.a = TRUE,
    mod.b = TRUE,
    random.mod.b = TRUE,
    random.mod.y = TRUE
  )

  expect_equal(extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0.0), 0.29287155, tolerance = 1e-4) # indirect effect when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1.0), 0.19017041, tolerance = 1e-4) # indirect effect when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab3, "indirect.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0.0) - extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab3, "a", modval1 = 0.0), 0.28749544, tolerance = 1e-4) # a when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab3, "a", modval1 = 1.0), 0.4079777, tolerance = 1e-4) # a when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab3, "a.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab3, "a", modval1 = 0.0) - extract.modmed.mlm(fitmodab3, "a", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab3, "b", modval1 = 0.0), 0.59464648, tolerance = 1e-4) # b when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab3, "b", modval1 = 1.0), 0.214396, tolerance = 1e-4) # b when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab3, "b.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab3, "b", modval1 = 0.0) - extract.modmed.mlm(fitmodab3, "b", modval1 = 1.0)
  ) # should match difference between the two above?
})


test_that("moderated a and b rand interaction both", {
  skip_on_cran()
  fitmodab4 <- modmed.mlm(
    simdat,
    "L2id",
    "X",
    "Y",
    "M",
    random.a = TRUE,
    random.b = TRUE,
    random.cprime = TRUE,
    moderator = "mod",
    mod.a = TRUE,
    mod.b = TRUE,
    random.mod.a = TRUE,
    random.mod.b = TRUE,
    random.mod.m = TRUE,
    random.mod.y = TRUE
  )
  # TODO: also test modvals with default inputs, instead of setting to 0? Just to make sure things don't break?
  # FIXME: what happens when you set modval2 only? Or if you set both modval1 and modval2 without calling indirect.diff?
  expect_equal(extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0.0), 0.30909784, tolerance = 1e-4) # indirect effect when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1.0), 0.15121, tolerance = 1e-4) # indirect effect when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab4, "indirect.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0.0) - extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab4, "a", modval1 = 0.0), 0.29092025, tolerance = 1e-4) # a when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab4, "a", modval1 = 1.0), 0.387422, tolerance = 1e-4) # a when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab4, "a.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab4, "a", modval1 = 0.0) - extract.modmed.mlm(fitmodab4, "a", modval1 = 1.0)
  ) # should match difference between the two above?

  expect_equal(extract.modmed.mlm(fitmodab4, "b", modval1 = 0.0), 0.5846672, tolerance = 1e-4) # b when moderator = 0
  expect_equal(extract.modmed.mlm(fitmodab4, "b", modval1 = 1.0), 0.22829447, tolerance = 1e-4) # b when moderator = 1
  expect_equal(
    extract.modmed.mlm(fitmodab4, "b.diff", modval1 = 0.0, modval2 = 1.0),
    extract.modmed.mlm(fitmodab4, "b", modval1 = 0.0) - extract.modmed.mlm(fitmodab4, "b", modval1 = 1.0)
  ) # should match difference between the two above?
})
