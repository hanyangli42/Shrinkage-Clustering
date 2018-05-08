#' Shrinkage Clustering
#'
#' Partition a data matrix using Shrinkage Clustering
#'
#' @param s The similarity matrix (n*n) of a given data matrix (n*k)
#' @param w The minimum number of members required for each cluster
#' @param k The maximum number of clusters allowed
#' @param iter The maximum number of iterations
#' @param random  An integer specifying the number of random initializations
#' @param Path A boolean value determining whether or not to  track and record the number of remaining clusters in each iteration
#' @return \item{c}{An n*1 vector containg the cluster membership of each data point}
#' @return \item{a}{An n*k matrix indicating the cluster assignment status of each data point and each class}
#' @return \item{score}{An indicator of convergence}
#' @references Hu, Chenyue W., Hanyang Li, and Amina A. Qutub. “Shrinkage Clustering: A Fast and Size-Constrained Clustering Algorithm for Biomedical Applications.” BMC Bioinformatics 19 (2018): 19. PMC. Web. 7 May 2018.
#'
#' @export
#'
#' @example SuperCluster_example.R

#'


#cluster - n*1 vector containg cluster membership
#% x - n*k matrix containing cluster membership
#% f - indicator of convergence


SuperCluster<-function(s,w=NULL,k=NULL,iter=1000,random=1,Path=F){
  # parameter initialization
  Step=1
  n=dim(s)[1]
  f=0
  if(is.null(w)){
    w=ceiling(0.05*n)
  }
  if(is.null(k)){
    if(w==0){
      k=25
    }else{
      k=ceiling(n/w)
    }
  }
  if(is.null(Step)){
    Step=round(n/10)
  }
  #set.seed(100)
  cluster=matrix(rep(0,n*random),n,random)
  ds=1-2*s
  diag(ds)=0
  if(Path==T){
    path=matrix(rep(0,iter*random),iter,random)
  }
  for(rs in 1:random){
    step=Step
    # random initialization of x
    x=matrix(rep(0,n*k),n,k)
    x=t(apply(x,1,function(a){a[sample(1:k,1)]=1
    return(a)}))

    i=0
    cat("\nIteration:   ")
    flag=0
    while(i<iter){
      x=x[,which(colSums(x)!=0)]
      ss=ds %*% x
      if(Path==T){
        path[i,rs]=dim(x)[2]
      }
      # remove clusters with too small sizes
      kj=1
      while(kj<=dim(x)[2]){
        if(sum(x[,kj])<w){
          xi=which(x[,kj]==1)
          x=x[,-kj]
          ss=ss[,-kj]
          if(is.null(dim(ss))){
            x[xi]=1
            flag=1
            break
          }else{
            for(ri in xi){
              x[ri,which(ss[ri,]==min(ss[ri,]))[1]]=1
            }
          }
          ss=ds %*% x
        }else{
          kj=kj+1
        }
      }
      if(flag==1){
        break
      }else{
        ms=rowSums(ss*x)-apply(ss,1,min)
        if(sum(ms)==0){ # convergence
          f=1
          break
        }else{
          if(step==1){
            xi=which(ms==max(ms))[1]
            x[xi,]=0
            x[xi,which(ss[xi,]==min(ss[xi,]))[1]]=1
          }else{
            mi=order(ms,decreasing=T)
            if(sum(ms!=0)<=step){
              if(step>9){
                step=round(sum(ms!=0)/2)
              }else{
                step=1
              }
            }
            xi=mi[1:step]
            x[xi,]=0
            for(ri in xi){
              x[ri,which(ss[ri,]==min(ss[ri,]))[1]]=1
            }
          }
        }
        i=i+1
        cat("\b\b\b\b")
        cat(sprintf("%4d",i))
      }
    }
    if(rs==1){
      fr=sum((s-x%*%t(x))^2)
    }else{
      fr=c(fr,sum((s-x%*%t(x))^2))
    }
    if(Path==T){
      path[i,rs]=dim(x)[2]
    }
    # format output
    if(flag==1){
      cluster[,rs]=rep(1,n)
    }else{
      c=1
      for(i in 1:dim(x)[2]){
        cluster[x[,i]!=0,rs]=c
        c=c+1
      }
    }
  }
  if(Path==T){
    return(list(c=cluster[,which(fr==min(fr))[1]],ac=cluster,p=path[,which(fr==min(fr))[1]],ap=path))
  }else{
    return(list(c=cluster,a=x,score=fr))
  }
}
