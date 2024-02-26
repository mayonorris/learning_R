matrix_product <- function(A, B)
{
     if (ncol(A) != nrow(B))
          {
               stop("Number of columns in matrix A must equal number of rows in matrix B")
          }
          
          res_matrix<- matrix(0, nrow(A), ncol(B))
          
          for (i in 1:nrow(A)) {
               for (j in 1:ncol(B)) {
                    for (k in 1:ncol(A)) {
                         res_matrix[i, j] <-  res_matrix[i, j] + A[i, k] * B[k, j]
                    }
               }
          }
          
          return(res_matrix)
     }


# Example matrices
A <- matrix(1:6, nrow = 2)
B <- matrix(c(1, 2, 3, 4, 0, 1), nrow = 3)

# Compute product using custom function
Z<- matrix_product(A, B)
print(Z)
