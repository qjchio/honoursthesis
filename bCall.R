# bisection = function(lower, upper){
#   error = 10^-5
#   
#   guess = (lower + upper)/2
#   
#   diff = abs(guess^2 - 2)
#   
#   if(diff > error){
#     if((guess^2 - 2) < 0 ){
#       # set guess as lower bound 
#       lower = guess
#       bisection(lower, upper)
#     }else{
#       upper = guess
#       bisection(lower,upper)
#     }
#   }else {
#     guess
#   }
# }
# 
# 
# 
# bisection(0,2)


bisection_Call = function(lower, upper, error, S0, K, r, t, c){
  guess = (lower+upper)/2
  d1 = (log(S0/K) + (r + guess*guess/2)*t)/(guess*sqrt(t)) 
  d2 = d1 - guess*sqrt(t)
  
  call = S0*pnorm(d1) - K*exp(-r*t)*pnorm(d2)
  
  diff = call - c
  
  if(abs(diff) > error){
    if(diff < 0){
      lower = guess
      bisection_Call(lower,upper,error,S0,K,r,t,c)
    }else{
      upper = guess
      bisection_Call(lower,upper,error,S0,K,r,t,c)
    }
    
  }else{
    guess
  }
}

#bisection_Call(0,2,10^-5,11663.96,12000,0.0202,0.1262234,0.0640*11663.96)
