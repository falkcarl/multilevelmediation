data("BPG06dat")
data("simdat")
simdat$covar_m1 = rep(1:8, 100)
simdat$covar_m2 = rep(-(1:8), 100)
simdat$covar_y1 = rep(0.1:0.8, 100)
simdat$covar_y2 = rep(-(0.1:0.8), 100)


# No moderator or covars
test_that("stack_bpg: valid input", {
  expect_snapshot_value(stack_bpg(data = BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m"), style = "json2")
})

test_that("stack_bpg: L2ID, X, Y, or M variable not in dataset", {
  expect_error(
    stack_bpg(data = BPG06dat, L2ID = "idwrong", X = "x", Y = "y", M = "m"),
    "L2ID variable not found in dataset. Please check specified name."
  )
  expect_error(
    stack_bpg(data = BPG06dat, L2ID = "id", X = "xwrong", Y = "y", M = "m"),
    "X variable not found in dataset. Please check specified name."
  )
  expect_error(
    stack_bpg(data = BPG06dat, L2ID = "id", X = "x", Y = "ywrong", M = "m"),
    "Y variable not found in dataset. Please check specified name."
  )
  expect_error(
    stack_bpg(data = BPG06dat, L2ID = "id", X = "x", Y = "y", M = "mwrong"),
    "M variable not found in dataset. Please check specified name."
  )
})

test_that("stack_bpg: non-numeric X, Y, or M", {
  BPG06dat$x2 <- as.character(BPG06dat$x)
  BPG06dat$y2 <- as.character(BPG06dat$y)
  BPG06dat$m2 <- as.character(BPG06dat$m)
  expect_error(stack_bpg(data = BPG06dat, L2ID = "id", X = "x2", Y = "y", M = "m"), "X is of type")
  expect_error(stack_bpg(data = BPG06dat, L2ID = "id", X = "x", Y = "y2", M = "m"), "Y is of type")
  expect_error(stack_bpg(data = BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m2"), "M is of type")
})

# With moderator
test_that("stack_bpg: moderator valid input", {
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "mod"), style = "json2")
})

test_that("stack_bpg: moderator not in dataset", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "modwrong"),
    "Moderator not found in dataset. Please check specified name."
  )
})

test_that("stack_bpg: moderator name same as X, Y, M, covars.m, or covars.y", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "L2id"),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables."
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "X"),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables."
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "Y"),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables."
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "M"),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables."
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M",
              moderator = "covar_m1", covars.m = c("covar_m1", "covars_m2")),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables."
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M",
              moderator = "covar_y1", covars.y = c("covar_y1", "covar_y2")),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables."
  )
})

test_that("stack_bpg: more than one moderator given", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = c("mod", "X")),
    "Only 1 moderator variable is allowed."
  )
})

test_that("stack_bpg: moderator non-numeric", {
  simdat$mod2 = as.character(simdat$mod)
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "mod2"),
    "Moderator is of type"
  )
})


# With covars.m
test_that("stack_bpg: covars.m valid input ", {
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "covar_m1"), style = "json2")
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = c("covar_m1", "covar_m2")), style = "json2")
})

test_that("stack_bpg: covars.m not in dataset", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "covar_m1wrong"),
    "One or more covariates not found in dataset. Please check specified name(s).", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = c("covar_m1wrong", "covar_m2")),
    "One or more covariates not found in dataset. Please check specified name(s).", fixed = TRUE
  )
})

test_that("stack_bpg: covars.m name same as X, Y, M, or moderator", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "L2id"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "X"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "Y"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "M"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "mod", covars.m = "mod"),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables.",
  ) # Note the different error text, this is caught earlier when checking the moderator's validity
})

test_that("stack_bpg: covars.m non-numeric", {
  simdat$covar_m1char = as.character(simdat$covar_m1)
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = c("covar_m1char", "covar_m2")),
    "One or more covariates is not numeric. Currently, only numeric covariates are supported."
  )
})


# With covars.y
test_that("stack_bpg: covars.y valid input ", {
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = "covar_y1"), style = "json2")
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = c("covar_y1", "covar_y2")), style = "json2")
})

test_that("stack_bpg: covars.y not in dataset", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = "covar_y1wrong"),
    "One or more covariates not found in dataset. Please check specified name(s).", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = c("covar_y1wrong", "covar_y2")),
    "One or more covariates not found in dataset. Please check specified name(s).", fixed = TRUE
  )
})

test_that("stack_bpg: covars.y name same as X, Y, M, or moderator", {
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = "L2id"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = "X"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = "Y"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = "M"),
    "Covariate(s) have the same name as X, Y, M, moderator, or L2ID variables.", fixed = TRUE
  )
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", moderator = "mod", covars.y = "mod"),
    "Moderator has same name as X, Y, M, covars.m, or covars.y, or L2ID variables.",
  ) # Note the different error text, this is caught earlier when checking the moderator's validity
})

test_that("stack_bpg: covars.y non-numeric", {
  simdat$covar_y1char = as.character(simdat$covar_y1)
  expect_error(
    stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.y = c("covar_y1char", "covar_y2")),
    "One or more covariates is not numeric. Currently, only numeric covariates are supported."
  )
})


# With both covars.m and covars.y
test_that("stack_bpg: same covars.m as covars.y", {
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "covar_m1", covars.y = "covar_m1"), style = "json2")
})

test_that("stack_bpg: different covars.m and covars.y", {
  expect_snapshot_value(stack_bpg(data = simdat, L2ID = "L2id", X = "X", Y = "Y", M = "M", covars.m = "covar_m1", covars.y = "covar_y1"), style = "json2")
})


