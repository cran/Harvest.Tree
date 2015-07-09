#' Predictions from a harvested tree
#' 
#' Return a predicted membership from a harvested tree
#' @param harfunc.object the output of harfunc function
#' @param data test data
#' @param num.var number of explaining varibles
#' @return "pred.mat" the result of prediction, belong is the node that data point belongs to, possibility is the probability of point being in class 1, predict is the simple perdict based on whether probability is larger than 0.5.
#' @export 



predict <- function(harfunc.object, data ,num.var)
{ 
  har.rule <- harfunc.object$har.rule
#first find the all category
  #delete the redundant category rule, 6 is the estimated upper limit of categroy numbers, which can be modified
  a=rep('exceed',length(har.rule)*num.var*6)
  dim(a)=c(length(har.rule),num.var,6)
  
  pattern=","
  for(i in 1:length(har.rule)){
    for(j in 1:length(har.rule[[i]]$rule[,1])){
      location=gregexpr(pattern,har.rule[[i]]$rule[j,3])  
      if(!is.na(location[[1]][1])&location[[1]][1]!=-1){
        v=as.numeric(substr(dimnames(har.rule[[i]]$rule)[[1]],2,2))
        for(p in 1:(length(location[[1]])+1)){
          if(p==1){
            a[i,v[j],p]=substr(har.rule[[i]]$rule[j,3],1,location[[1]][1]-1)
          }
          else if(p==(length(location[[1]])+1)){
            a[i,v[j],p]=substr(har.rule[[i]]$rule[j,3],location[[1]][p-1]+1,nchar(har.rule[[i]]$rule[j,3]))
          }
          else{
            a[i,v[j],p]=substr(har.rule[[i]]$rule[j,3],location[[1]][p-1]+1,location[[1]][p]-1)
          }  
        } 
      }
      else if(!is.na(har.rule[[i]]$rule[j,3])){
        v=as.numeric(substr(dimnames(har.rule[[i]]$rule)[[1]],2,2))
        a[i,v[j],1]=har.rule[[i]]$rule[j,3]
      }     
    }
  }

  
  belong=predict=possibility=numeric(length(data[,1]))
  for(i in 1:length(data[,1])){
   for(j in 1:length(har.rule)){
    v=as.numeric(substr(dimnames(har.rule[[j]]$rule)[[1]],2,2))+1
    logvec <- TRUE
     for(p in 1:length(har.rule[[j]]$rule[,1])){
    if((!is.infinite(har.rule[[j]]$rule[p,1]) | !is.infinite(har.rule[[j]]$rule[p,2])) & !is.na(data[i,v[p]])){
      logvec <- data[i,v[p]]>= har.rule[[j]]$rule[p,1] & data[i,v[p]] <= har.rule[[j]]$rule[p,2] & logvec
    }
    else if(!is.na(data[i,v[p]]) & !is.na(har.rule[[j]]$rule[p,3])) {
      logvec <- (sum(data[i,v[p]]== a[j,v[p]-1,])==1) & logvec
    }
   }
   if(logvec==TRUE){break}
   }
   belong[i]=j
  possibility[i]=as.numeric(har.rule[[j]][3])/as.numeric(har.rule[[j]][2])

  }
  predict=ifelse(possibility>0.5,1,0) 
  predict.mat=cbind(data,belong,possibility,predict)
  return(predict.mat)
 }
