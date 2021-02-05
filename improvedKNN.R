
k = 10
## datadim with rows samples and cols genes
## latentdim with rows samples and cols genes
improvedKNN <- function(dataclustering, latentclustering, datadim, latentdim, k){
 dim(datadim)
  dim(latentdim)
  #get distance matrices for each input data
  distdata <- as.matrix(dist(datadim))
  distlatent <- as.matrix(dist(latentdim))
  
  #rank each row's neighbors
  rankdata <- apply(distdata, 1, rank)
  ranklatent <- apply(distlatent, 1, rank)


  #find k nearest neighors for each sample (columns are NN's for that sample)
  dataNN <- apply(rankdata, 2, function(x) {names(x[which(x %in% 2:(k+1))])})
  latentNN <- apply(ranklatent, 2, function(x) {names(x[which(x %in% 2:(k+1))])})

  
  #calculate sum of the distance between each point and it's kNN (hk, gk)
  #hk sums
  hk <- c()
  
  #TODO: there's a better way to do this loop but haven't figured out how
  for (i in 1:ncol(dataNN)){
    hki <- sum(distdata[dataNN[,i], dataNN[,i]])
    hk <- c(hk, hki)
  }
  
  #gk sums
  gk <- c()
  
  #TODO: there's a better way to do this loop but haven't figured out how
  for (i in 1:ncol(latentNN)){
    gki <- sum(distlatent[latentNN[,i], latentNN[,i]])
    gk <- c(gk, gki)
  }

  
  #subset hk's and gk's into c cluster groups
  n <- length(latentclustering)
  nc <- table(latentclustering)
  
  
  #sum over k's for (gk - hk)/# clusters k
  
  
  
  
  
}