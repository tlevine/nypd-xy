#!/usr/bin/env Rscript

# Load the data
if (!'crime' %in% ls())
  crime <- read.csv('data/02378420399528461352-11853667273131550346.csv')

# Take a sample for development.
cr <- crime[sample.int(nrow(crime), 1e3),]

#' Express a number as a character in unscientific format.
#' @param x numeric
#' @return character representation of x
unscientific <- function(x) format(x, scientific = F)

#' Write out an lm formula in pretty characters
write.formula <- function(model) {
  b0 <- coef(model)[1]
  b1 <- coef(model)[2]
  y <- names(attr(lat$terms, 'dataClasses'))[[1]]
  x1 <- names(attr(lat$terms, 'dataClasses'))[[1]]

  paste(format(y, scientific = FALSE),
        '=',
        format(b0, scientific = FALSE),
        '+',
        format(b1, scientific = FALSE),
        '×',
        format(x1, scientific = FALSE))
}

# Go
long <- lm(longitude ~ X, data = crime)
lat <- lm(latitude ~ Y, data = crime)
cat(paste('Assuming a flat Earth,',
          write.formula(long),
          write.formula(lat),
          '',
          sep = '\n'))
