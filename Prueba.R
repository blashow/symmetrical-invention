n <- 200
p <- 0.5

Racha <- function(n,p) {
racha <- c()
for (j in 1:10000) {
  x <- rbinom(n,1,p)
  rachas <- 1
for (i in 1:(n-1)) { 
  if (x[i] != x[i+1]) {rachas <- rachas + 1}
}
racha[j] <- rachas
}

mean(racha)/p
}
Racha(n,p)

rachamaxima <- function(n,p,B) {
  rachamax <- c()
  for (j in 1:B) {
    
    rachamax[j] <- 1
    racha2 <- 1
    x <- rbinom(n,1,p)

    for (i in 1:(n-1)) {
      
      if (x[i]==x[i+1]) {
      racha2 <- racha2 + 1
      }
      
    else  {racha2 <- 1}
      
      if(racha2 > rachamax[j]) {rachamax[j] <- racha2}
    }
  }
return(rachamax)
}
mean(rachamaxima(200,0.5,10000))



p<-0.5
fun <- function(n,p) {
  x <- rbinom(n,1,p)
  racha2 <- 1
  rachamax <- 1
  for (i in 1:(n-1)) {
    
    if (x[i]==x[i+1]) {
      racha2 <- racha2 + 1
    }
    
    else  {racha2 <- 1}
    
    if(racha2 > rachamax) {rachamax <- racha2}
  }
  return(rachamax)
}
mean(replicate(10000,fun(200,0.5)))

sapply(n=1:100,FUN=fun)
