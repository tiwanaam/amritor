model1 <- glm(y ~ x1 + x2, family = binomial("logit"), data = toydata)
model1coef <- summary(model1)$coef
OR_95CI(model1coef[,1], model1coef[,2], 0.05, 2)

test_that("OR function works", {
  expect_equal(OR_95CI(-0.03971, 0.25383, 0.05, 2), "0.96 (0.58, 1.58)")
})
