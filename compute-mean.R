#Mean calculation function
comp_mean<-function(x, na_rm)
{
     if (!is.numeric(x)) {
          stop("Input must be a numeric vector.")
     }
     
     if (length(x) == 0) {
          warning("Input vector is empty.")
          return(NA)
     }
     
     if(na_rm == F)
     {
          return(sum(x)/length(x))
     } else {
          x<-x[!is.na(x)] 
          return(sum(x)/length(x))
     }
}
set.seed(33)
x<-runif(50)
comp_mean(x, na_rm = T)
y<-c(x[1:5], NA, NA, x[3:8], NA, 1, NA, x[48:50])
comp_mean(y, na_rm = F)
comp_mean(y, na_rm = T)
mean(y, na.rm = T)
z_letter<-sample(letters, 6)
comp_mean(z_letter)
