extrapolate = function(data1,N,f,t,x0,xNN){
  cleandata = data1[,c(1,3,4)]
  
  x1 = data1$d2[1]
  increment = (x1 - x0)/N
  yprime1 = data1$yprime[1]
  
  
  
  #left extrapolation
  # print("asd")
  for(i in 1:N){
    target = x1 - i*increment #d2
    # print(target)
    # print(x1)
    # print(x0)
    IIV = yprime1*target 
    K = f*exp(-target*sqrt(IIV)*sqrt(t) - 0.5*IIV*t) #strike price
    addrow = c(K, IIV, target)
    cleandata = rbind(cleandata,addrow)
   
  }
  
  for(i in 1:N){
    target = x0 - i*increment
    IIV = yprime1*x0
    K = f*exp(-target*sqrt(IIV)*sqrt(t) - 0.5*IIV*t) #strike price
    addrow = c(K, IIV, target)
    cleandata = rbind(cleandata,addrow)
  }
  
  
  ### right extrapolation
  xn = data1$d2[nrow(data1)]
  increment1 = (xNN - xn)/N
  yprimen = data1$yprime[nrow(data1)]
  
  for(i in 1:N){
    target = xn + i*increment1 #d2
    IIV = yprimen * target
    K = f*exp(-target*sqrt(IIV)*sqrt(t) - 0.5*IIV*t) #strike price
    addrow = c(K, IIV, target)
    cleandata = rbind(cleandata,addrow)
    
  }
  for(i in 1:N){
    target = xNN + i*increment1
    IIV = yprimen*xNN
    K = f*exp(-target*sqrt(IIV)*sqrt(t) - 0.5*IIV*t) #strike price
    addrow = c(K, IIV, target)
    cleandata = rbind(cleandata,addrow)
  }
  
  
  
  cleandata = cleandata[order(cleandata$d2),]
  
  
}






# interpolateAll = function(data1, N,f,t){
#   cleandata = data1[,c(1,3,4)]
#   
#   for(i in 2:nrow(data1)){
#     increment = (data1$d2[i] - data1$d2[i-1])/N
#     for(j in 1:(N-1)){
#       target = data1$d2[i-1] + j*increment #d2
#       IIV = interpol(data1,target,f,t) #variance
#       #interpolated IV
#       K = f*exp(-target*sqrt(IIV)*sqrt(t) - 0.5*IIV*t) #strike price
#       addrow = c(K, IIV, target)
#       cleandata = rbind(cleandata,addrow)
#     }
#   }
#   
#   cleandata = cleandata[order(cleandata$Strike),]
# }