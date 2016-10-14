function.subset<-function(x,x_s){
  subset(x, is.na(x_s)==FALSE)
}

# subsetting away NA's where x is data frame and x.s is criteria for NA values, representing the called column of the data frame


# function to create predicted columns where x refers to data frame, k is the discount value, l is the later amt, d is the delay amt, and s s is the small amt
function.predicted.values<-function(x,k,l,d,s){
  for (i in c(as.numeric(unique(k)))){
    x[[paste0('pred',formatC(i, digits=5, format="f"))]]<-1
  }
  for (i in grep("pred", colnames(x))){
    for(j in 1:nrow(x)){
      x[j,i]<-ifelse((l[j]/(1+(as.numeric(unlist(strsplit(colnames(x[i]), split="pred",fixed=TRUE))[2])*d[j])))>s[j],2,1)
    }
  }
  x
}


# functions to create match percent columns where y is the output dataframe, x is the dataframe to manipulate, y_id is the id called column of the data frame, and y_choice is the called key press of the data frame 


create.match<-function(y,x){
  y<-x[grep("pred", colnames(x))]
  names(y)<-gsub("pred","match", names(y))
  y<-cbind(x,y)
  y
}


match.percent<-function(y,y_id,y_choice){
  for (j in as.character(as.vector(unique(y_id)))){
    for (i in grep("match", colnames(y))){
      y[y_id==j,i]<-sum(y[y_id==j,i]==y_choice[y_id==j])/length(y[y_id==j,i])
    }
  }
  y
}

# function to create geometric means and generate overall paarameter where y_p is the called parameter of the data frame


overall.parameter<-function(y_id,y_p){
  for (i in (unique(y_id))){
    y_p[y_id==i]<-prod(y_p[y_id==i])^(1/length(y_p[y_id==i]))
  }
  y_p
}

# for reward parameter estimation 


match.percent.reward<-function(y,y_id,y_reward,y_choice){
  for (j in as.character(as.vector(unique(y_id)))){
    for (k in unique(y_reward)){
      for (i in grep("match", colnames(y))){
        y[(y_id==j & y_reward==k),i]<-sum(y[(y_id==j & y_reward==k),i]==y_choice[(y_id==j & y_reward==k)])/length(y[(y_id==j & y_reward==k),i])
      }
    }
  }
y
}  


reward.parameter<-function(y_id,y_reward, y_p){
  for(j in unique(y_reward)){
    for(i in unique(y_id)){
      y_p[y_id==i & y_reward==j]<-prod(y_p[y_id==i & y_reward==j])^(1/length(y_p[y_id==i & y_reward==j]))
    }
  }
  y_p
}





