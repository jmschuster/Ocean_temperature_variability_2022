# original code from: https://github.com/federicogiorgi/corto/blob/master/R/functions.R

#' scinot - Convert a number to a scientific notation expression
#'
#' This function will convert any numeric vector
#'
#' @param v The input numeric object. It can be a single value or a vector
#' @param digits An integer indicating how many significant digits to show. Default is 3.
#' @return An object of class _expression_.
#' @examples
#' # Usage on single value
#' scinot(0.00000543)
#' # Demonstration on a vector
#' numbers<-c(3.456e-12,0.00901,5670000,-3.16e18,0.000004522,rnorm(5,sd=0.0000001))
#' plot(0,xlim=c(0,10),ylim=c(0,10),type="n")
#' text(c(2,6),c(10,10),labels=c("Before","After"),font=2)
#' for(i in 10:1){
#'     text(c(2,6),c(i-1,i-1),labels=c(numbers[i],scinot(numbers)[i]))
#' }
#' @export
scinot<-function(v,digits=3){
  v<-signif(v,digits)
  vv<-format(v,scientific=TRUE)
  v1<-gsub("e.+","",vv)
  v2<-gsub(".+e","",vv)
  v2<-gsub("-0+","-",v2)
  v2<-gsub("\\+0","+",v2)
  v2<-gsub("\\++","",v2)
  
  vexpr<-vector("expression",length(v))
  for(i in 1:length(vv)){
    bq<-as.expression(bquote(.(v1[i])~x~10^.(v2[i])))
    vexpr[i]<-bq
  }
  return(vexpr)
}

v = 0.0005
digits = 3
v<-signif(v,digits)
vv<-format(v,scientific=TRUE)
v1<-gsub("e.+","",vv)
v2<-gsub(".+e","",vv)
v2<-gsub("-0+","-",v2)
v2<-gsub("\\+0","+",v2)
v2<-gsub("\\++","",v2)

vexpr<-vector("expression",length(v))
for(i in 1:length(vv)){
  bq<-as.expression(bquote(.(v1[i])~x~10^.(v2[i])))
  vexpr[i]<-bq
}
return(vexpr)