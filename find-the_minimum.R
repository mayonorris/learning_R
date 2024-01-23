#!/usr/bin/R

# A function that finds the minimun value within a numeric vector.
find_min<- function(x) {
     if (!is.numeric(x)) {
          stop("Input must be a numeric vector.")
     }

     if (length(x) == 0) {
          warning("Input vector is empty.")
          return(NA)
     }

     min_val <- x[1]

     for (val in x) {
          if (val < min_val) {
               min_val <- val
          }
     }

     return(min_val)
}
#Test the function
set.seed(33)
x<-runif(50)
find_min(x)
min(x)
z_letter<-sample(letters, 6)
find_min(z_letter)
