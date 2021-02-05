
## datadim with rows samples and cols genes
## latentdim with rows samples and cols genes

improvedKNN <- function(dataclustering, latentclustering, datadim, latentdim, k){
  
  #get distance matrices for each input data
  distdata <- dist(datadim)
  distlatent <- dist(latentdim)
  
  #rank each row's neighbors
  rankdata <- rank(distdata)
  
  #find k nearest neighrs for each sample
  
  
  #find each point's k nearest neighbors for each input clustering point
  
  #calculate sum of the distance between each point and it's kNN (hk)
  
  
  #find each point's k nearest neighbors for each latent clustering point
  
  #calculate sum of the distance between each point and it's kNN (gk)
  
  
  #so we should have k number of hk metrics and k number of gk's (one distance metric for each cluster)
  
  
  
  
  #sum over k's for (gk - hk)/# clusters k
  
  
  
  
  
}