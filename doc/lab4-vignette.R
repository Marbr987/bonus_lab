## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lab4)

## -----------------------------------------------------------------------------
# Crate an object of class linreg by calling the linreg function
linreg_obj <- linreg(formula = Petal.Length ~ Species, data = iris)
class(linreg_obj)

# The linreg object is a list containing the following elements
names(linreg_obj)

## -----------------------------------------------------------------------------
print(linreg_obj)

## ---- fig.width=7, fig.height=6-----------------------------------------------
plot_list <- plot(linreg_obj)
print(plot_list[1])
print(plot_list[2])

## -----------------------------------------------------------------------------
head(resid(linreg_obj))

## -----------------------------------------------------------------------------
head(pred(linreg_obj))

## -----------------------------------------------------------------------------
coef(linreg_obj)

## -----------------------------------------------------------------------------
summary(linreg_obj)

