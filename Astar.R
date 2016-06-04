nodes <- list(
  "START" = (function(x) 0),
  "A"= (function(x) if(x>700) 0 else 22-0.005*x),
  "B" = (function(x) if(x>700) 0 else 20 - 0.02*x),
  "C" = (function(x) if(x>700) 0 else 29 - 0.01*x),
  "D" = (function(x) if(x>700) 0 else 20 - 0.04*x),
  "E" = (function(x) if(x>700) 0 else 40 - 0.021*x),
  "F" = (function(x) if(x>700) 0 else 20 - 0.04*x),
  "G" = (function(x) if(x>700) 0 else 20 - 0.03*x),
  "H" = (function(x) if(x>700) 0 else 30 - 0.02*x),
  "I" = (function(x) if(x>700) 0 else 50 - 0.032*x),
  "J" = (function(x) if(x>700) 0 else 50 - 0.01*x),
  "K" = (function(x) if(x>700) 0 else 50 - 0.02*x),
  "L" = (function(x) if(x>700) 0 else 50 - 0.042*x),
  "M" = (function(x) if(x>700) 0 else 50 - 0.012*x),
  "N" = (function(x) if(x>700) 0 else 50 - 0.05*x),
  "O" = (function(x) if(x>700) 0 else 50 - 0.052*x)
  
)

edges <- list (
  "START" = list("A"= function(x) 15+0.0008*x*(700-x), "B"= function(x) 34+0.00052*x*(700-x), "E"= function(x) 33+0.00051*x*(700-x), "D"= function(x) 32+0.0002*x*(700-x),
                 "C"= function(x) 30+0.00053*x*(700-x), "G"= function(x) 30+0.0001*x*(720-x), "F"= function(x) 30+0.00032*x*(700-x),
                 "H"= function(x) 50+0.00001*x*(700-x), "I"= function(x) 30+0.0005*x*(740-x), "J"= function(x) 30+0.0005*x*(740-x), "K"= function(x) 30+0.0005*x*(740-x), 
                 "L"= function(x) 30+0.0005*x*(740-x), "M"= function(x) 30+0.0003*x*(740-x), "N"= function(x) 30+0.0005*x*(740-x)),
  "A" = list("A"= function(x) 31+0.0015*x*(700-x), "B"= function(x) 32+0.0001*x*(712-x), "E"= function(x) 30+0.0005*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 12+0.0003*x*(700-x), "G"= function(x) 20+0.0002*x*(640-x), "F"= function(x) 3+0.0025*x*(700-x),
             "H"= function(x) 33+0.0002*x*(720-x), "I"= function(x) 30+0.0005*x*(720-x),
             "L"= function(x) 30+0.0005*x*(740-x), "M"= function(x) 30+0.0003*x*(740-x), "N"= function(x) 30+0.0005*x*(740-x)),
  "B" = list("A"= function(x) 20+0.0004*x*(700-x), "B"= function(x) 30+0.0005*x*(700-x), "E"= function(x) 23+0.0007*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 30+0.00056*x*(700-x), "G"= function(x) 30+0.0015*x*(700-x), "F"= function(x) 31+0.0025*x*(700-x),
             "H"= function(x) 30+0.0001*x*(710-x), "I"= function(x) 30+0.0005*x*(550-x), "L"= function(x) 30+0.0015*x*(740-x),
             "M"= function(x) 30+0.0009*x*(740-x), "N"= function(x) 30+0.0002*x*(740-x)),
  "C" = list("A"= function(x) 32+0.0015*x*(600-x), "B"= function(x) 33+0.0001*x*(700-x), "E"= function(x) 32+0.0025*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 13+0.0005*x*(700-x), "G"= function(x) 32+0.0002*x*(800-x), "F"= function(x) 31+0.005*x*(700-x),
             "H"= function(x) 23+0.0005*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x), "J"= function(x) 30+0.0005*x*(740-x), "K"= function(x) 30+0.0003*x*(740-x), "L"= function(x) 30+0.0005*x*(740-x)),
  "D" = list("A"= function(x) 12+0.0005*x*(500-x), "B"= function(x) 20+0.0001*x*(490-x), "E"= function(x) 13+0.0002*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 34+0.0003*x*(700-x), "G"= function(x) 10+0.0002*x*(700-x), "F"= function(x) 12+0.0001*x*(700-x),
             "H"= function(x) 12+0.0005*x*(700-x), "I"= function(x) 30+0.0005*x*(680-x), "H"= function(x) 30+0.0005*x*(740-x), "I"= function(x) 30+0.0002*x*(740-x), "J"= function(x) 30+0.0005*x*(740-x)),
  "E" = list("A"= function(x) 30+0.0002*x*(700-x), "B"= function(x) 12+0.0005*x*(700-x), "E"= function(x) 33+0.0001*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 30+0.0005*x*(700-x), "G"= function(x) 13+0.0005*x*(640-x), "F"= function(x) 23+0.0002*x*(700-x),
             "H"= function(x) 33+0.0002*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x), "J"= function(x) 30+0.0005*x*(740-x), "J"= function(x) 30+0.0003*x*(740-x), "K"= function(x) 30+0.0005*x*(740-x)),
  "F" = list("A"= function(x) 32+0.0001*x*(700-x), "B"= function(x) 11+0.0025*x*(620-x), "E"= function(x) 30+0.0031*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 36+0.0001*x*(700-x), "G"= function(x) 23+0.001*x*(700-x), "F"= function(x) 30+0.00123*x*(700-x),
             "H"= function(x) 40+0.0001*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x)),
  "G" = list("A"= function(x) 7+0.002*x*(700-x), "B"= function(x) 12.3+0.0003*x*(700-x), "E"= function(x) 53+0.0012*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 16+0.0007*x*(700-x), "G"= function(x) 41+0.0004*x*(630-x), "F"= function(x) 32+0.00043*x*(700-x),
             "H"= function(x) 11+0.0005*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x)),
  "H" = list("A"= function(x) 21+0.0009*x*(700-x), "B"= function(x) 15+0.0002*x*(700-x), "E"= function(x) 34+0.0002*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 13+0.0005*x*(700-x), "G"= function(x) 12+0.0001*x*(700-x), "F"= function(x) 30+0.0002*x*(700-x),
             "H"= function(x) 22+0.0003*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x)),
  "I" = list("A"= function(x) 30+0.0002*x*(700-x), "B"= function(x) 12+0.0005*x*(700-x), "E"= function(x) 33+0.0001*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 13+0.0005*x*(700-x), "G"= function(x) 12+0.0001*x*(700-x), "F"= function(x) 30+0.0002*x*(700-x),
             "H"= function(x) 33+0.0002*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x)),
  "J" = list("M"= function(x) 30+0.0002*x*(700-x), "B"= function(x) 12+0.0005*x*(700-x), "E"= function(x) 33+0.0001*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 13+0.0005*x*(700-x), "G"= function(x) 12+0.0001*x*(700-x), "F"= function(x) 30+0.0002*x*(700-x),
             "H"= function(x) 33+0.0002*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x)),
  "K" = list("A"= function(x) 30+0.0002*x*(700-x), "L"= function(x) 12+0.0005*x*(700-x), "E"= function(x) 33+0.0001*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 13+0.0005*x*(700-x), "G"= function(x) 12+0.0001*x*(700-x), "F"= function(x) 30+0.0002*x*(700-x),
             "H"= function(x) 7+0.0002*x*(700-x), "I"= function(x) 30+0.0005*x*(700-x)),
  "L" = list("A"= function(x) 23+0.0002*x*(700-x), "B"= function(x) 12+0.0005*x*(700-x), "J"= function(x) 52+0.0001*x*(700-x), "C"= function(x) 35+0.0005*x*(700-x),
             "C"= function(x) 2+0.0005*x*(700-x), "G"= function(x) 12+0.0001*x*(700-x), "F"= function(x) 37+0.0002*x*(700-x),
             "H"= function(x) 77+0.0002*x*(700-x), "I"= function(x) 34+0.0005*x*(700-x)),
  "M" = list("A"= function(x) 37+0.0002*x*(700-x), "B"= function(x) 13+0.0005*x*(700-x), "K"= function(x) 35+0.0001*x*(700-x), "D"= function(x) 30+0.0005*x*(700-x),
             "C"= function(x) 19+0.0005*x*(700-x), "G"= function(x) 15+0.0001*x*(700-x), "F"= function(x) 33+0.0002*x*(700-x),
             "H"= function(x) 36+0.0002*x*(700-x), "I"= function(x) 39+0.0005*x*(700-x)),
  "N" = list("A"= function(x) 35+0.0002*x*(700-x), "B"= function(x) 19+0.0005*x*(700-x), "O"= function(x) 32+0.0001*x*(700-x), "D"= function(x) 34+0.0005*x*(700-x),
             "C"= function(x) 11+0.0005*x*(700-x), "G"= function(x) 18+0.0001*x*(700-x), "F"= function(x) 31+0.0002*x*(700-x),
             "H"= function(x) 32+0.0002*x*(700-x), "I"= function(x) 34+0.0005*x*(700-x)),
  "O" = list("A"= function(x) 33+0.0002*x*(700-x), "B"= function(x) 11+0.0005*x*(700-x), "N"= function(x) 34+0.0001*x*(700-x), "D"= function(x) 31+0.0005*x*(700-x),
             "C"= function(x) 23+0.0005*x*(700-x), "G"= function(x) 19+0.0001*x*(700-x), "F"= function(x) 38+0.0002*x*(700-x),
             "H"= function(x) 34+0.0002*x*(700-x), "I"= function(x) 38+0.0005*x*(700-x))
  
)

price <- function(n, time) {
  return (nodes[[n]](time))
}
  
count_heuristic <- function(visited, time, price, prev, curr) {
  value <- 0
  for (n in names(nodes)) {
    if (!n %in% visited) {
      value <- value + nodes[[n]](time)
    }
  }
  return(value)
}

count_heuristic1 <- function(visited, time, price, prev, curr) {
  value <- 0
  for (n in names(nodes)) {
    if (!n %in% visited) {
      if(n %in% names(edges[[curr]]))
        value <- value + nodes[[n]](time+time(curr, n, time))
      else
        value <- value + nodes[[n]](time)
    }
  }
  return(value)
}

count_heuristic2 <- function(visited, time, price, prev, curr) {
  value <- 0
  temp <- list()
  for (n in names(nodes)) {
    if (!n %in% visited) {
      temp<- c(n, temp)
    }
  }
  set.seed(001)
  temp<-sample(temp)
  lowest = which.min(unlist(lapply(edges, function(x) (lapply(x, function (y) y(0))))))[[1]]
  i = 0
  for(t in temp){
    i = i+1
    value <- value + nodes[[t]](time + lowest*i)
  }
  return(value)
}

count_heuristic3 <- function(visited, time, price, prev, curr) {
  value <- 0
  temp <- list()
  for (n in names(nodes)) {
    if (!n %in% visited) {
      temp<- c(n, temp)
    }
  }
  temp<-temp[order(sapply(temp, function(x) nodes[[x]](time), simplify = TRUE),
                   decreasing = TRUE)]
  lowest = which.min(unlist(lapply(edges, function(x) (lapply(x, function (y) y(0))))))[[1]]
  i = 0
  for(t in temp){
    i = i+1
    value <- value + nodes[[t]](time + lowest*i)
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

N <- function(struct, heur) {
  visited = struct[["visited"]]
  result <- list()
  curr <- struct[["curr"]]
  neighbors <- getNeighbors(curr)
  for(n in neighbors) {
    if(!n %in% visited) {
      new_visited = c(visited, n)
      time <- struct[["time"]] + time(curr, n, struct[["time"]])
      price <- struct[["price"]] + price(n, time)
      heuristic <- heur(new_visited, time, price, curr, n)
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

value <- list()
i <- 0
heuristics = list(count_heuristic, count_heuristic1, count_heuristic2, count_heuristic3)
for(heur in heuristics){
  i <- i +1
  LOG <- list(0)
  H <- list(list("visited" = c("START"),
            "curr" = "START",
            "time" = 0,
            "price" = 0,
            "heuristic" = heur(vector('character'), 0, 0, "START", "START")))
  while(length(H[[1]][["visited"]])<length(nodes)) {
    x<-selPrior(H)
    H[[1]] <- NULL
    Y<-N(x ,heur)
    H<-append(Y, H)
    LOG <- append(lapply(Y, function(x) x[["price"]]+x[["heuristic"]]), LOG)
    H<-H[order(sapply(H, function(x) x[["price"]]+x[["heuristic"]], simplify = TRUE),
            decreasing = TRUE)]
  }
  value <- c(value,H[[1]][["price"]])
  alist<-c(1:length(LOG))
  png(
    paste(i,"test.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 1
  )
  par(
    mar      = c(5, 5, 2, 2),
    xaxs     = "i",
    yaxs     = "i",
    cex.axis = 2,
    cex.lab  = 2
  )
  
  
  plot(alist, LOG, cex=0.1, pch=4, type="p", col="blue")
  dev.off()
}
