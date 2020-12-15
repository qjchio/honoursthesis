getLast = function(data, K, S, r, t){
  
  data$Last = 0
  
  for(i in 1:nrow(data)){
    if(data$Strike[i] > K){
      ## use call formula
      d1 = (log(S/data$Strike[i]) + (r +data$IV[i]/2)*t)/(sqrt(t*data$IV[i]))
      d2 = d1 - sqrt(data$IV[i] * t)
      
      c = S*pnorm(d1) - data$Strike[i]*exp(-r*t)*pnorm(d2)
      
      data$Last[i] = c
      
    }else{
      ## use put formula
      d1 = (log(S/data$Strike[i]) + (r +data$IV[i]/2)*t)/(sqrt(t*data$IV[i]))
      d2 = d1 - sqrt(data$IV[i] * t)
      
      p = data$Strike[i]*exp(-r*t) * pnorm(-d2) - S*pnorm(-d1)
      
      data$Last[i] = p
      
    }
  }
  
  data
}