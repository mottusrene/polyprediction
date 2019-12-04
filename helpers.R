require(glmnet)

foo = function(x,y1,y2=NULL,p=.67, fam, fam2 = NULL) {
  if(is.null(fam2)) fam2 = fam
  v = !is.na(y1)
  x =  x %>% filter(v) %>% sapply(scale)
  if(is.null(y2)) y2 = y1
  y1 = y1[v]
  y2 = y2[v] 
  s = sample(nrow(x), p*nrow(x))
  pr = cv.glmnet(x[s,], y1[s], alpha = .05, family = fam) %>%
    predict(x[-s,], s = "lambda.min")
  if(fam2 == "gaussian")
    cor(y2[-s], pr, use="pairwise")^2
  else
    r2(round(y2[-s]), pr, f = fam) %>% as.numeric
}

r2 = function(y,x,f) 1 - (logLik(glm(y~x,family=f)) / logLik(glm(y~1,family=f))) 