t = [  [ 0, 36, 3, 105, 210], [1, 300, 10, 45, 66], [ 190, 6, 171, 15, 153], [28, 55, 1553, 21, 120], [91, 276, 231, 136, 78] ]
  
5.times { |x|
  printf(" ( ")
  5.times { |y|
    printf("%2d, ", t[x][y] % 64)
  }
  printf(" )\n")
}