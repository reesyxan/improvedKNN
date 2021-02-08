
## datadim with rows samples and cols genes
## latentdim with rows samples and cols genes
improvedKNN <- function(groundtruth, datadim, latentdim, k, distdata, distlatent){
  #get distance matrices for each input data
  if (is.null(distdata)){
  distdata <- as.matrix(dist(datadim))
  }
  if (is.null(dist)){
  distlatent <- as.matrix(dist(latentdim))
  }
  #rank each row's neighbors
  rankdata <- apply(distdata, 1, rank)
  ranklatent <- apply(distlatent, 1, rank)


  colnames(rankdata) <- rownames(datadim)
  colnames(ranklatent) <- rownames(latentdim)
  
  rownames(rankdata) <- rownames(datadim)
  rownames(ranklatent) <- rownames(latentdim)

  #find k nearest neighors for each sample (columns are NN's for that sample)
  dataNN <- apply(rankdata, 2, function(x) {names(x[which(x %in% 2:(k+1))])})
  latentNN <- apply(ranklatent, 2, function(x) {names(x[which(x %in% 2:(k+1))])})
  
  dataNNmat <- matrix(0, ncol = length(rownames(rankdata)), nrow = length(rownames(rankdata)))
  rownames(dataNNmat) <- rownames(rankdata)
  colnames(dataNNmat) <- colnames(rankdata)
  
  latentNNmat <- matrix(0, ncol = length(rownames(rankdata)), nrow = length(rownames(rankdata)))
  rownames(latentNNmat) <- rownames(rankdata)
  colnames(latentNNmat) <- colnames(rankdata)


 
  #make NN matrix for original data and latent
  for (i in 1:dim(dataNN)[2]){
    dataNNmat[dataNN[,i],i] <- 1
  }

 i = 1
  for (i in 1:dim(latentNN)[2]){
    latentNNmat[latentNN[,i],i] <- 1
  }
  
  #calculate how many of each point's neighbors are it's own kind (hk, gk)
  
  # labels of each class
  names(groundtruth) <- rownames(datadim)
  clusterlabels <- c()
  for (i in unique(groundtruth)){
    clusterlabels[[i]] <- names(groundtruth[groundtruth == i])
  }
  

  #hk
  #subset dataNNmat for each c class and count number of neighbors with same class label
  hk <- c()
  for (i in 1:length(clusterlabels)) {
    hk[[i]] <- colSums(dataNNmat[clusterlabels[[i]], clusterlabels[[i]]])
  }


  #TODO: there's a better way to do this loop but haven't figured out how
  gk <- c()
  for (i in 1:length(clusterlabels)) {
    gk[[i]] <- colSums(latentNNmat[clusterlabels[[i]], clusterlabels[[i]]])
  }
  

  #subset hk's and gk's into c cluster groups
  n <- length(groundtruth)
  nc <- table(groundtruth)
 
  weights <- nc/n 

  #sum over c's for weight_c*sum((gk - hk)/k)
  improvedKNN <- c()
  for (i in 1:length(nc)){
    part <- weights[i]*sum((gk[[i]] - hk[[i]])/k)
    improvedKNN <- c(improvedKNN, part)
  }

  improvedKNN <- sum(improvedKNN)/length(groundtruth)
  return(improvedKNN)
}
