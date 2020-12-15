generate = function(data){
  #cleandata = cleandata[order(cleandata$Strike),]
  data = data[order(data$d2),]
  
  
  for(i in 1:(nrow(data)-1)){
    data$L[i+1] = sqrt( (data$d2[i+1] - data$d2[i])^2 + (data$IV[i+1] - data$IV[i])^2 )
  }
  
  
  for(i in 1:nrow(data)){
    if( (i == 1) | (i == nrow(data))){
      data$yprime[i] = data$IV[i] / data$d2[i]
    }else{
      data$yprime[i] = -( ((data$d2[i+1] - data$d2[i])/data$L[i+1]) - ((data$d2[i] - data$d2[i-1])/data$L[i]) ) / 
                           ( ((data$IV[i+1] - data$IV[i])/data$L[i+1])  - ((data$IV[i] - data$IV[i-1])/data$L[i])  )
    }
  }
  
  data
}