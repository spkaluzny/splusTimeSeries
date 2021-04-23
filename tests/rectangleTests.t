{
  library(splusTimeSeries)
  TRUE
}

{
  # test is.rectangular
  xi <- as.integer(1:10)
  xn <- xi + .1
  xc <- letters
  y <- matrix(1:10, nrow=5)
  z <- data.frame( a = 1:10, b = 11:20 )
  lst <- list( a=1:10, b=letters[1:10] )

  ( is.rectangular(xi) && is.rectangular( xn ) && is.rectangular( xc ) &&
   is.rectangular(y ) && is.rectangular( z ) && !is.rectangular( lst ))
}

{
  # test as.rectangular on integer
    all.equal( as.rectangular( xi ), xi )
}
{

  # test as.rectangular on numeric

    all.equal( as.rectangular( xn ), xn )
}
{

  # test as.rectangular on character

    all.equal( as.rectangular( xc ), xc )
}
{

  # test as.rectangular on matrix

    all.equal( as.rectangular( y ), y )
}
{

  # test as.rectangular on data frame

    all.equal( as.rectangular( z ), z )
}
{

  # test as.rectangular on list

    all.equal( as.rectangular( lst ), as.data.frame(lst))
}

{
  # test subscript2d function
  ( all( subscript2d(xi,1:3,1) == xi[1:3] ) &&
    all( subscript2d(y,1:3, 1) == y[1:3,1,drop=FALSE] ) &&
    all( subscript2d(z,1:3, 1) == z[1:3,1,drop=FALSE] )) 
}

{
  # test subscript2d replace function
  subscript2d(xi,1:3,1) <- 4:6
  subscript2d(y,1:3,1) <- 4:6
  subscript2d(z,1:3,1) <- 4:6

  ( all( xi[1:3] == 4:6 ) &&
    all( y[1:3,1,drop=TRUE] == 4:6 ) &&
    all( z[1:3,1,drop=TRUE] == 4:6 ))
}

{
  # test numrows, numcols functions

  (( numRows(xi) == 10 ) && 
   ( numRows(y) == 5 ) && 
   ( numRows(z) == 10 ) &&
   ( numCols(xi) == 1 ) &&
   ( numCols(y) == 2 ) &&
   ( numCols(z) == 2 ))
}

{
  # test numrows, cols replacement
  numRows(xi) <- 3
  numRows(xn) <- 15
  y2 <- y
  numRows(y) <- 3
  numRows(y2) <- 15
  z2 <- z
  numRows(z) <- 3
  numRows(z2) <- 15

  numCols(xi) <- 1
  numCols(xn) <- 3
  numCols(y) <- 1
  numCols(y2) <- 3
  numCols(z) <- 1
  numCols(z2) <- 3

  ( is.null( dim(xi) ) && ( numRows(xi) == 3 ) && ( numCols(xi) == 1 ) &&
    ( numRows(xn) == 15 ) && ( numCols(xn) == 3 ) && 
    all( dim(xn) == c( 15, 3 )) &&
    ( numRows(y) == 3 ) && ( numCols(y) == 1 ) && 
    all( dim(y) == c( 3, 1 )) &&
    ( numRows(xn) == 15 ) && ( numCols(y2) == 3 ) && 
    all( dim(y2) == c( 15, 3 )) &&
    ( numRows(z) == 3 ) && ( numCols(z) == 1 ) && 
    all( dim(z) == c( 3, 1 )) &&
    ( numRows(z2) == 15 ) && ( numCols(z2) == 3 ) && 
    all( dim(z2) == c( 15, 3 )))
}

{
  # test rowIds, colIds functions

  rowIds(xi) <- letters[1:3]
  colIds(xn) <- letters[4:6]  
  rowIds(xn) <- letters[1:15]
  rowIds(z) <- letters[1:3]
  colIds(z2) <- letters[1:3]


  ( all( rowIds(xi) == letters[1:3] ) &&
    all( colIds(xn) == letters[4:6] ) &&
    all( rowIds(xn) == letters[1:15] ) &&
    all( colIds(z2) == letters[1:3] ) &&
    all( rowIds(z) == letters[1:3] ))
}
