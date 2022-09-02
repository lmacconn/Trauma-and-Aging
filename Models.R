
regression <- function(Y, X, A){
  svyglm(paste(Y, "~", X, "+", A), design=dfw, data=df)
}
regression(logIL6, prison, age)
load()
svyglm(logIL6~prison+age, design = dfw, data=df)
summary( svyglm(logIL6~prison+age, design = dfw, data=df) )
svyglm(logIL6~prison+age, design =dfw)
summary(svyglm(logIL6~prison+age, design =dfw))
