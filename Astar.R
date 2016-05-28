nodes <- list(
  "START" = (function(x) 0),
  "A"= (function(x) if(x>200) 0 else 200-x),
  "B" = (function(x) if(x>100) 0 else 200 - 2*x)
)

edges <- list (
  "START" = list("A"= function(x) 40, "B"= function(x) 80),
  "A" = list("START"= function(x) 40, "B"= function(x) 80),
  "B" = list("A"= function(x) 40, "START"= function(x) 80)
)

price <- function(n, time){
  return (nodes[[n]](time))
}
  
count_heuristic <- function(visited, time, price){
  return (0)
}

time <- function(curr, nextt, timee){
  return (edges[[curr]][[nextt]](timee))
}

getNeighbors <- function(node){
  return (names(edges[[node]]))
}

selPrior <- function(log){
  return (log[[1]])
}

N <- function(struct){
  visited = struct["visited"]
  result <- list()
  curr <- struct[["curr"]]
  neighbors <- getNeighbors(curr)
  for(n in neighbors){
    if(!n %in% visited){
      new_visited = c(visited, n)
      time <- struct[["time"]] + time(curr, n, struct[["time"]])
      price <- struct[["price"]] + price(n, time)
      heuristic <- count_heuristic(new_visited, time, price)
      element <- list("visited" = new_visited,
                     "curr" = n,
                     "time" = time,
                     "price" = price,
                     "heuristic"= heuristic
                     )
      result <- append(result, list(element))
    }
  }
  return (result)
}


H <- list(list("visited" = vector('character'),
          "curr" = "START",
          "time" = 0,
          "price" = 0,
          "heuristic" = count_heuristic(vector('character'), 0, 0)))
  x<-selPrior(H)
  Y<-N(x)H
  H<-append(Y, H)
  H[order(sapply(H, function(x) x[["price"]]+x[["heuristic"]], simplify = TRUE),
          decreasing = TRUE)]


