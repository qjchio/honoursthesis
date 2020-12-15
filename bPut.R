bisection_Put = function(lower, upper, error, S0, K, r, t, p){
  guess = (lower+upper)/2
  d1 = (log(S0/K) + (r + guess*guess/2)*t)/(guess*sqrt(t)) 
  d2 = d1 - guess*sqrt(t)
  
  put = K*exp(-r*t)*pnorm(-d2) - S0*pnorm(-d1)
  
  diff = put - p
  
  if(abs(diff) > error){
    if(diff < 0){
      lower = guess
      bisection_Put(lower,upper,error,S0,K,r,t,p)
    }else{
      upper = guess
      bisection_Put(lower,upper,error,S0,K,r,t,p)
    }
    
  }else{
    guess
  }
}

# bisection_Put(0,2,10^-5,u1,K01, 0.0202, T1, 0.0250*u1)
