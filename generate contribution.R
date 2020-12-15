getContribution = function(data,r,t){
  data$Contribution = 0
  
  for ( i in 2:(nrow(data)-1)){
    delK = (data$Strike[i-1] - data$Strike[i+1])/2
    Ki = data$Strike[i]
    data$Contribution[i] = delK/Ki/Ki*exp(r*t)*data$Last[i]
  }
  
  data$Contribution[1] = (data$Strike[1]-data$Strike[2])/(data$Strike[1]^2) * exp(r*t) * data$Last[1]
  n = nrow(data)
  data$Contribution[n] = (data$Strike[n-1] - data$Strike[n])/(data$Strike[n]^2) * exp(r*t) * data$Last[n]
  data
}