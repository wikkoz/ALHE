selPrior <- function(log){
  return log[1]
}

N <- function(struct){
  visited = struct["visited"]
  result <- list()
  curr <- struct["curr"]
  neighbors <- getNeighbors(curr)
  for(n in neighbors){
    if(!n %in% visited){
      visited = c(visited, n)
      time <- struct["time"] + time(curr, n, struct["time"])
      price <- strut["price"] + price(n, price)
      heuristic <- count_heuristic(visited, time, price)
      element <- list("visited" = visited,
                     "curr" = n,
                     "time" = time,
                     "price" = price,
                     "heuristic"= heuristic
                     )
      result <- list(result, list(element))
    }
  }
  return result
}

H <- s0
while(1){
  x<-selPrior(H)
  Y<-N(x)
  H<-list(Y, H)
  H[order(sapply(H, function(x) x["price"]+x["heuristic"], simplify = TRUE),
          decreasing = TRUE)]
}