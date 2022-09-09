
regression <- function(Y, X, A){
 m = svyglm(paste(Y, "~", X, "+", A), design=dfw, data=df)
 s= summary(m)
 return(s)
}
mlist = NULL

for (Y in c("logIL6", "logTNF1", "logCRP")) {
  for (X in c("prison", "combatzone", "homeless")) {
    m = regression(Y, prison, age)
    mlist = c(mlist, m)  
  }
}


print(mlist)
regression(logIL6, prison, age)
load()
svyglm(logIL6~prison+age, design = dfw, data=df)
summary( svyglm(logIL6~prison+age, design = dfw, data=df) )
svyglm(logIL6~prison+age, design =dfw)
summary(svyglm(logIL6~prison+age, design =dfw))
summary(lm(logIL6~prison+age, data=df, weights = PVBSWGTR))

