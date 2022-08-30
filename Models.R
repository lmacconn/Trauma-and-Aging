
regression <- function(Y, X, A){
  svyglm(paste(Y, "~", X, "+", A), design=dfw, data=df)
}
regression()