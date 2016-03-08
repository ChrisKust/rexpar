inv_tri <- function(Matrix)
{
  dim(Matrix)[2] - col(Matrix) + 1 < row(Matrix)
}
