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
  d[x<=0]=0
  return(d)
}

pnfw = function(q, con=5, log.p=FALSE){
  p = .pnfwunorm(q=q, con=con)/.pnfwunorm(q=1, con=con)
  p[q>1]=1
  p[q<=0]=0
  if(log.p){
    return(log(p))
  }else{
    return(p)
  }
}

qnfw = function(p, con=5, log.p=FALSE){
  if(log.p){
    p=exp(p)
  }
  p[p>1]=1
  p[p<=0]=0
  p=p*.pnfwunorm(q=1, con=con)
  if(requireNamespace("lamW", quietly = TRUE)){
    return((-(1/lamW::lambertW0(-exp(-p-1)))-1)/con)
  }else if(requireNamespace("gsl", quietly = TRUE)){
    return((-(1/gsl::lambert_W0(-exp(-p-1)))-1)/con)
  }else{
    stop('One of lamW (fastest) or gsl (easier to install) package must be installed to use this function!')
  }
}

rnfw = function(n, con=5){
  if(length(n)>1){
    n=length(n)
  }
  return(qnfw(p=runif(n), con=con))
}
