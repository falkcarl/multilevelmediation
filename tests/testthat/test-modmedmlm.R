data("BPG06dat")
data("simdat")

#TODO: Load in pre-fit models, rather than need to rerun every time
#TODO: move tests that use extract function to test-extract.R.
#TODO: Check invalid stacked data is handled correctly
#TODO: add tests for datmfun...

# make_fixed_formula ----
test_that("make_fixed_formula default args match modmed.mlm defaults", {
  fe_args <- formals("make_fixed_formula")
  modmedmlm_args <- formals("modmed.mlm")
  expect_identical(as.list(fe_args), modmedmlm_args[c("mod.a", "mod.b", "mod.cprime", "covars.m", "covars.y")])
})

test_that("make_fixed_formula output is correct", {
  #FIXME: does the formula have to be in this exact order? or can just check if elements exist?

  expect_identical(make_fixed_formula(), "Z ~ 0 + Sm + Sy + SmX + SyX + SyM")
  expect_identical(make_fixed_formula(mod.a = TRUE), "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sm:W + SmX:W")
  expect_identical(make_fixed_formula(mod.b = TRUE), "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sy:W + SyM:W")
  expect_identical(make_fixed_formula(mod.cprime = TRUE), "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sy:W + SyX:W")
  expect_identical(
    make_fixed_formula(mod.a = TRUE, mod.b = TRUE, mod.cprime = TRUE),
    "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sm:W + SmX:W + Sy:W + SyM:W + SyX:W"
  )
  expect_identical(make_fixed_formula(covars.m = "covar"), "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sm:covar")
  expect_identical(make_fixed_formula(covars.y = "covar"), "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sy:covar")
  expect_identical(
    make_fixed_formula(mod.a = TRUE, mod.b = TRUE, mod.cprime = TRUE, covars.m = "covar", covars.y = "covar"),
    "Z ~ 0 + Sm + Sy + SmX + SyX + SyM + Sm:W + SmX:W + Sy:W + SyM:W + SyX:W + Sm:covar + Sy:covar"
  )
})

# make_random_formula ----
test_that("make_random_formula default args match modmed.mlm", {
  re_args <- formals("make_random_formula")
  modmedmlm_args <- formals("modmed.mlm")
  expect_identical(
    as.list(re_args),
    modmedmlm_args[c(
      "random.int.m",
      "random.int.y",
      "random.a",
      "random.b",
      "random.cprime",
      "random.mod.a",
      "random.mod.b",
      "random.mod.cprime",
      "random.mod.m",
      "random.mod.y",
      "random.covars.m",
      "random.covars.y",
      "mod.a",
      "mod.b",
      "mod.cprime"
    )]
  )
})

test_that("make_random_formula output is correct", {
  expect_identical(make_random_formula(), "~ 0 + Sm + Sy | L2id")
  expect_identical(make_random_formula(random.int.m = FALSE, random.int.y = FALSE), "~ 0 | L2id")
  expect_identical(make_random_formula(random.int.m = TRUE, random.int.y = FALSE), "~ 0 + Sm | L2id")
  expect_identical(make_random_formula(random.int.m = FALSE, random.int.y = TRUE), "~ 0 + Sy | L2id")
  expect_identical(make_random_formula(random.a = TRUE), "~ 0 + Sm + Sy + SmX | L2id")
  expect_identical(make_random_formula(random.b = TRUE), "~ 0 + Sm + Sy + SyM | L2id")
  expect_identical(make_random_formula(random.cprime = TRUE), "~ 0 + Sm + Sy + SyX | L2id")
  expect_error(
    make_random_formula(random.mod.a = TRUE),
    "mod.a must be set to TRUE to add random effect of moderated a path."
  )
  expect_identical(make_random_formula(random.mod.a = TRUE, mod.a = TRUE), "~ 0 + Sm + Sy + SmX:W | L2id")
  expect_error(
    make_random_formula(random.mod.b = TRUE),
    "mod.b must be set to TRUE to add random effect of moderated b path."
  )
  expect_identical(make_random_formula(random.mod.b = TRUE, mod.b = TRUE), "~ 0 + Sm + Sy + SyM:W | L2id")
  expect_error(
    make_random_formula(random.mod.cprime = TRUE),
    "mod.cprime must be set to TRUE to add random effect of moderated cprime path."
  )
  expect_identical(make_random_formula(random.mod.cprime = TRUE, mod.cprime = TRUE), "~ 0 + Sm + Sy + SyX:W | L2id")
  expect_error(
    make_random_formula(random.mod.m = TRUE),
    "mod.a must be set to TRUE to add random effect of moderated Sm."
  )
  expect_identical(make_random_formula(random.mod.m = TRUE, mod.a = TRUE), "~ 0 + Sm + Sy + Sm:W | L2id")
  expect_error(
    make_random_formula(random.mod.y = TRUE),
    "mod.b must be set to TRUE to add random effect of moderated Sy."
  )
  expect_identical(make_random_formula(random.mod.y = TRUE, mod.b = TRUE), "~ 0 + Sm + Sy + Sy:W | L2id")
  expect_identical(make_random_formula(random.covars.m = "covar"), "~ 0 + Sm + Sy + Sm:covar | L2id")
  expect_identical(make_random_formula(random.covars.y = "covar"), "~ 0 + Sm + Sy + Sy:covar | L2id")
})

# Default args ----
test_that("moderator args correct", {
  expect_error(
    modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", mod.a = TRUE),
    "No moderator was specified for the moderated path(s).",
    fixed = TRUE
  )
  expect_error(
    modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", mod.b = TRUE),
    "No moderator was specified for the moderated path(s).",
    fixed = TRUE
  )
  expect_error(
    modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", mod.cprime = TRUE),
    "No moderator was specified for the moderated path(s).",
    fixed = TRUE
  )
})

test_that("non-default 'control' arg is passed correctly", {
  #FIXME: maybe can just use update() function here to check things? eg can also just load from a prefit default model to make things easier/quicker...
  fit_lme_default <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  fit_lme_new <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", control = lmeControl())
  fit_glmmtmb <- modmed.mlm(
    data = BPG06dat,
    L2ID = "id",
    X = "x",
    Y = "y",
    M = "m",
    estimator = "glmmTMB",
    control = glmmTMBControl(optCtrl = list(iter.max = 100L))
  )
  expect_identical(
    fit_lme_default$args$control,
    lmeControl(maxIter = 10000L, msMaxIter = 10000L, niterEM = 10000L, msMaxEval = 10000L, tolerance = 1e-6)
  ) # this is the current default, keep in tests for backwards compatibility?
  expect_identical(fit_lme_new$args$control, lmeControl())
  expect_identical(fit_glmmtmb$args$control, glmmTMBControl(optCtrl = list(iter.max = 100L)))
})

# Returned objects ----
test_that("Returndata is correct", {
  fit <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", returndata = TRUE)
  expect_identical(fit$data, stack_bpg(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m"))
})

test_that("Valid stacked data is passed correctly", {
  stacked_df <- stack_bpg(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  fit <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", data.stacked = stacked_df, returndata = TRUE)
  expect_identical(fit$data, stack_bpg(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m"))
})

test_that("Invalid stacked data is handled correctly", {
  #TODO
})

# lme vs glmmTMB output ----
test_that("lme vs glmmTMB outputs", {
  fit_lme <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  fit_glmmtmb <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", estimator = "glmmTMB")

  expect_equal(fixef(fit_lme$model), fixef(fit_glmmtmb$model)$cond, tolerance = 1e-5) #fixed effects
  expect_equal(unclass(ranef(fit_lme$model)$Sm), ranef(fit_glmmtmb$model)$cond$L2id$Sm, tolerance = 1e-4) # Sm
  expect_equal(unclass(ranef(fit_lme$model)$Sy), ranef(fit_glmmtmb$model)$cond$L2id$Sy, tolerance = 1e-4) # Sy
})


#FIXME Old tests, should be removed or moved to test-extract.R ----
test_that("BPG rand a rand b", {
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.a = TRUE, random.b = TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type = "indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type = "a"))
  expect_snapshot(extract.modmed.mlm(fit, type = "b"))
  expect_snapshot(extract.modmed.mlm(fit, type = "covab"))
  expect_snapshot(extract.modmed.mlm(fit, type = "cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type = "fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov.vec"))
})

test_that("BPG rand a, rand b, rand c", {
  skip_on_cran()
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.a = TRUE, random.b = TRUE, random.cprime = TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type = "indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type = "a"))
  expect_snapshot(extract.modmed.mlm(fit, type = "b"))
  expect_snapshot(extract.modmed.mlm(fit, type = "covab"))
  expect_snapshot(extract.modmed.mlm(fit, type = "cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type = "fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov.vec"))
})

test_that("BPG rand a", {
  skip_on_cran()
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.a = TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type = "indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type = "a"))
  expect_snapshot(extract.modmed.mlm(fit, type = "b"))
  expect_snapshot(extract.modmed.mlm(fit, type = "cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type = "fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov.vec"))
})

test_that("BPG rand b", {
  skip_on_cran()
  fit <- modmed.mlm(BPG06dat, "id", "x", "y", "m", random.b = TRUE)

  expect_snapshot(extract.modmed.mlm(fit))
  expect_snapshot(extract.modmed.mlm(fit, type = "indirect"))
  expect_snapshot(extract.modmed.mlm(fit, type = "a"))
  expect_snapshot(extract.modmed.mlm(fit, type = "b"))
  expect_snapshot(extract.modmed.mlm(fit, type = "cprime"))
  expect_snapshot(extract.modmed.mlm(fit, type = "fixef"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov"))
  expect_snapshot(extract.modmed.mlm(fit, type = "recov.vec"))
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

  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect", modval1 = 0.0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect", modval1 = 1.0)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmoda, "indirect.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmoda, "indirect", modval1 = 0.0) -
      extract.modmed.mlm(fitmoda, "indirect", modval1 = 1.0)
  ) # should match prev line
  expect_snapshot(extract.modmed.mlm(fitmoda, "a", modval1 = 0.0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmoda, "a", modval1 = 1.0)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmoda, "a.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmoda, "a", modval1 = 0.0) -
      extract.modmed.mlm(fitmoda, "a", modval1 = 1.0)
  ) # should match prev line
  expect_snapshot(extract.modmed.mlm(fitmoda, "b", modval1 = 0.0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmoda, "b", modval1 = 1.0)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmoda, "b.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmoda, "b", modval1 = 0.0) -
      extract.modmed.mlm(fitmoda, "b", modval1 = 1.0)
  ) # should match prev line
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
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect", modval1 = 0.0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect", modval1 = 1.0)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodb, "indirect.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodb, "indirect", modval1 = 0.0) -
      extract.modmed.mlm(fitmodb, "indirect", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodb, "a", modval1 = 0.0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodb, "a", modval1 = 1.0)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodb, "a.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodb, "a", modval1 = 0.0) -
      extract.modmed.mlm(fitmodb, "a", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodb, "b", modval1 = 0.0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodb, "b", modval1 = 1.0)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodb, "b.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodb, "b", modval1 = 0.0) -
      extract.modmed.mlm(fitmodb, "b", modval1 = 1.0)
  ) # should match prev line
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

  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect", modval1 = 0.0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect", modval1 = 1.0)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(fitmodab, "indirect.diff", modval1 = 0.0, modval2 = 1.0)) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab, "indirect", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab, "indirect", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab, "a", modval1 = 0.0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab, "a", modval1 = 1.0)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab,
    "a.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab, "a", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab, "a", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab, "b", modval1 = 0.0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab, "b", modval1 = 1.0)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab,
    "b.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab, "b", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab, "b", modval1 = 1.0)
  ) # should match prev line
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

  expect_snapshot(extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0.0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1.0)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab2,
    "indirect.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab2, "indirect", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab2, "indirect", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab2, "a", modval1 = 0.0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab2, "a", modval1 = 1.0)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab2,
    "a.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab2, "a", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab2, "a", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab2, "b", modval1 = 0.0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab2, "b", modval1 = 1.0)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab2,
    "b.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab2, "b", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab2, "b", modval1 = 1.0)
  ) # should match prev line
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

  expect_snapshot(extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0.0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1.0)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab3,
    "indirect.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab3, "indirect", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab3, "indirect", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab3, "a", modval1 = 0.0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab3, "a", modval1 = 1.0)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab3,
    "a.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab3, "a", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab3, "a", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab3, "b", modval1 = 0.0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab3, "b", modval1 = 1.0)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab3,
    "b.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab3, "b", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab3, "b", modval1 = 1.0)
  ) # should match prev line
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

  expect_snapshot(extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0.0)) # indirect effect when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1.0)) # indirect effect when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab4,
    "indirect.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab4, "indirect", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab4, "indirect", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab4, "a", modval1 = 0.0)) # a when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab4, "a", modval1 = 1.0)) # a when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab4,
    "a.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab4, "a", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab4, "a", modval1 = 1.0)
  ) # should match prev line

  expect_snapshot(extract.modmed.mlm(fitmodab4, "b", modval1 = 0.0)) # b when moderator = 0
  expect_snapshot(extract.modmed.mlm(fitmodab4, "b", modval1 = 1.0)) # b when moderator = 1
  expect_snapshot(extract.modmed.mlm(
    fitmodab4,
    "b.diff",
    modval1 = 0.0,
    modval2 = 1.0
  )) # should match difference between the two above?
  expect_snapshot(
    extract.modmed.mlm(fitmodab4, "b", modval1 = 0.0) -
      extract.modmed.mlm(fitmodab4, "b", modval1 = 1.0)
  ) # should match prev line
})

# to see code coverage of tests
#library(covr)
#covr <- package_coverage(path="./path/to/package")
#covr <- package_coverage(path=".")
#covr
#report(covr)
