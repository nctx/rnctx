test.kldist <- function(){
#! [Dist Test KL_div]
#! library(RUnit)
  suppressMessages(library(nctx))
  a <- c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
  b <- c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
  a <- abs(a)
  a <- a/sum(a)
  b <- abs(b)
  b <- b/sum(b)
  checkEquals(dist_kl_divergence(a,b), 1.3689655011086466)
#! [Dist Test KL_div]
}

test.jsdist <- function(){
#! [Dist Test JS_div]
#! library(RUnit)
  suppressMessages(library(nctx))
  a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
  b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
  a <- abs(a)
  a <- a/sum(a)
  b <- abs(b)
  b <- b/sum(b)
  checkEquals(dist_js_divergence(a,b), 0.21286088091616043)
#! [Dist Test JS_div]
}

test.eucldist <- function(){
#! [Dist Test Eucl_Dist]
#! library(RUnit)
  suppressMessages(library(nctx))
  a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
  b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
  checkEquals(dist_euclidean_distance(a,b), 3.433288222859105)
#! [Dist Test Eucl_Dist]
}

test.cosdist <- function(){
#! [Dist Test Cos_Dist]
#! library(RUnit)
  suppressMessages(library(nctx))
  a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
  b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
  checkEquals(dist_cosine_similarity(a,b), 0.49634136855031963)
#! [Dist Test Cos_Dist]
}

test.angldist <- function(){
#! [Dist Test Ang_Dist]
#! library(RUnit)
  suppressMessages(library(nctx))
  a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
  b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
  checkEquals(dist_angular_distance(a,b), 0.3346764408212951)
#! [Dist Test Ang_Dist]
}
