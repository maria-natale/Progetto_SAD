skw <-function (x){
  n<-length (x)
  m2 <-(n -1) *var (x)/n
  m3 <- (sum ( (x- mean(x))^3) )/n
  m3/(m2 ^1.5)
}

curt <-function (x){
  n <-length (x)
  m2 <-(n -1) *var (x)/n
  m4 <- (sum ((x-mean(x))^4) )/n
  m4/(m2 ^2) -3
}

skw(utenti)
skw(mediavittimeitalia)

curt(utenti)
curt(mediavittimeitalia)