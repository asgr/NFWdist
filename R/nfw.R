.pnfwunorm=function(q, con=5){
  return(log(1 + q*con)-(con*q)/(1 + con*q))
}

dnfw = function(x, con=5, log=FALSE){
  if(log){
    d = log(con^2*x/((con*x+1)^2*(1/(con+1)+log(con+1)-1)))
  }else{
    d = con^2*x/((con*x+1)^2*(1/(con+1)+log(con+1)-1))
  }
  d[x>1]=0
  return(d)
}

pnfw = function(q, con=5, log.p=FALSE){
  if(log.p){
    p = log(.pnfwunorm(q=q, con=con)/.pnfwunorm(q=1, con=con))
  }else{
    p = .pnfwunorm(q=q, con=con)/.pnfwunorm(q=1, con=con)
  }
  return(p)
}

qnfw = function(p, con=5, log.p=FALSE){
  if(log.p){p=exp(p)}
  p=p*.pnfwunorm(q=1, con=con)
  return((-(1/lambert_W0(-exp(-p-1)))-1)/con)
}

rnfw = function(n, con=5){
  if(length(n)>1){n=length(n)}
  return(qnfw(p=runif(n), con=con))
}
