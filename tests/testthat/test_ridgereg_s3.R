#' @importFrom  MASS lm.ridge
context("ridgereg")

data("iris")

test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lamda=1))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lamnda=1))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lamnda=c(1,2)))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  
  expect_s3_class(ridgereg_mod, "ridgereg")
})

test_that("print() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  
  expect_output(print(ridgereg_mod),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length,")
  expect_output(print(ridgereg_mod),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("pred() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  
  expect_equal(round(unname(pred(ridgereg_mod)[c(1,5,7)]),2), c(1.86, 1.55, 1.11))    
})


test_that("coef() works", {
  ridgereg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  ridgereg_MASS <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  
  expect_true(all(round(unname(coef(ridgereg_mod)),2) %in% round(unname(coef(ridgereg_MASS)),2)))
})

test_that("ridgereg functions returns same value as lm.ridge", {
  ridgereg_mod1 <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  ridgereg_MASS1 <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=1)
  ridgereg_mod2 <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=2)
  ridgereg_MASS2 <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=2)
  ridgereg_mod3 <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  ridgereg_MASS3 <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=0.5)
  
  expect_equal(round(unname(coef(ridgereg_mod1)),2),
               round(unname(coef(ridgereg_MASS1)),2))
  expect_equal(round(unname(coef(ridgereg_mod2)),2),
               round(unname(coef(ridgereg_MASS2)),2))
  expect_equal(round(unname(coef(ridgereg_mod3)),2),
               round(unname(coef(ridgereg_MASS3)),2))
})





