data("BPG06dat")
data("simdat")

#TODO: Load in pre-fit models, rather than need to rerun every time
#TODO: Check invalid stacked data is handled correctly
#TODO: add tests for datmfun...

# make_fixed_formula ----
test_that("make_fixed_formula default args match modmed.mlm defaults", {
  fe_args <- formals("make_fixed_formula")
  modmedmlm_args <- formals("modmed.mlm")
  expect_identical(as.list(fe_args), modmedmlm_args[c("mod.a", "mod.b", "mod.cprime", "covars.m", "covars.y")])
})

test_that("make_fixed_formula output is correct", {
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
  # TODO
})

# lme vs glmmTMB output ----
test_that("lme vs glmmTMB outputs", {
  fit_lme <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")
  fit_glmmtmb <- modmed.mlm(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m", estimator = "glmmTMB")

  expect_equal(fixef(fit_lme$model), fixef(fit_glmmtmb$model)$cond, tolerance = 1e-5) #fixed effects
  expect_equal(unclass(ranef(fit_lme$model)$Sm), ranef(fit_glmmtmb$model)$cond$L2id$Sm, tolerance = 1e-4) # Sm
  expect_equal(unclass(ranef(fit_lme$model)$Sy), ranef(fit_glmmtmb$model)$cond$L2id$Sy, tolerance = 1e-4) # Sy
})

# to see code coverage of tests
#library(covr)
#covr <- package_coverage(path="./path/to/package")
#covr <- package_coverage(path=".")
#covr
#report(covr)
