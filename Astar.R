nodes <- list(
  "START" = (function(x) 0),
  "A"= (function(x) if(x>200) 0 else 200-0.5*x),
  "B" = (function(x) if(x>100) 0 else 200 - 2*x),
  "C" = (function(x) if(x>100) 0 else 200 - 0.1*x),
  "D" = (function(x) if(x>100) 0 else 200 - 0.4*x),
  "E" = (function(x) if(x>100) 0 else 200 - 10*x),
  "F" = (function(x) if(x>100) 0 else 200 - 6.4*x),
  "G" = (function(x) if(x>100) 0 else 200 - 3*x),
  "H" = (function(x) if(x>100) 0 else 200 - 3.5*x),
  "I" = (function(x) if(x>100) 0 else 200 - 4*x),
  "J" = (function(x) if(x>100) 0 else 200 - 4.4*x),
  "K" = (function(x) if(x>100) 0 else 200 - 4.3*x)
)

edges <- list (
  "START" = list("A"= function(x) 4, "B"= function(x) 30, "
                 c"= function(x) 35),
  "A" = list("START"= function(x) 100, "B"= function(x) 30),
  "B" = list("A"= function(x) 40, "START"= function(x) 3),
  "C" = list("A"= function(x) 40, "START"= function(x) 23),
  "D" = list("A"= function(x) 40, "C"= function(x) 33),
  "E" = list("A"= function(x) 60, "K"= function(x) 43),
  "F" = list("A"= function(x) 80, "D"= function(x) 53),
  "G" = list("A"= function(x) 30, "K"= function(x) 63),
  "H" = list("A"= function(x) 10, "K"= function(x) 73),
  "I" = list("A"= function(x) 20, "C"= function(x) 83),
  "J" = list("A"= function(x) 90, "F"= function(x) 93, "B"= function(x) 9, "K"= function(x) 74, "E"= function(x) 3, "D"= function(x) 2, "START"= function(x) 44),
  "K" = list("A"= function(x) 100, "J"= function(x) 44, "B"= function(x) 49, "K"= function(x) 44, "E"= function(x) 23, "D"= function(x) 20, "START"= function(x) 44)
)

price <- function(n, time) {
  return (nodes[[n]](time))
}
  
count_heuristic <- function(visited, time, price) {
  value <- 0
  for (n in names(nodes)) {
    if (!n %in% visited) {
      value <- value + nodes[[n]](time)
    }
  }
  return(value)
}

time <- function(curr, nextt, timee) {
  return (edges[[curr]][[nextt]](timee))
}

getNeighbors <- function(node) {
  return (names(edges[[node]]))
}

selPrior <- function(log){
  return (log[[1]])
}

N <- function(struct) {
  visited = struct[["visited"]]
  result <- list()
  curr <- struct[["curr"]]
  neighbors <- getNeighbors(curr)
  for(n in neighbors) {
    if(!n %in% visited) {
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


H <- list(list("visited" = c("START"),
          "curr" = "START",
          "time" = 0,
          "price" = 0,
          "heuristic" = count_heuristic(vector('character'), 0, 0)))
  while(length(H[[1]][["visited"]])<3) {
    x<-selPrior(H)
    H[[1]] <- NULL
    Y<-N(x)
    H<-append(Y, H)
    H<-H[order(sapply(H, function(x) x[["price"]]+x[["heuristic"]], simplify = TRUE),
            decreasing = TRUE)]
  }


