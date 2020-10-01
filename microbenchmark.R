x = for(i in 1:100){
  for(j in 1:100)
  {
  data = rep(i, each = j)
  }
}

microbenchmark(
  mean(x),
  median(x),
  mode(x)
)
