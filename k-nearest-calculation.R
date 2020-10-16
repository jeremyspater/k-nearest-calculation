#Function to calculate K-nearest-neighbors exposure metrics

library(geosphere)

#Function to calculate K-nearest
k_sim = function(dat, k){ #inputs: data frame and k, number of neighbors to look at
  dat_out = dat #data frame for output
  
  #create distance matrix
  all_mat = dist(dat[,c('long','lat')]) %>% as.matrix()  #just use euclidean distance. units are meters
  diag(all_mat) = NA
  #if k > the number of geocoded neighbors, set to NA
  kmax = max(rowSums(!is.na(all_mat))) #number of non-NA entries in distance matrix; equal to # of non-na-geocodes -1
  if(k > kmax){ #if k is higher than total number of neighbors, set to na
    dat_out[,paste0('Nearest_',k,'_Same')] = NA # Set to na
  }else{
    dat_out[,paste0('Nearest_',k,'_Same')] = NA # Initialize
    for(i in 1:nrow(dat_out)){ #loop over ppl in neighborhood
      
      #make these NA if geocode is NA for the person for whom knn is being calculated
      if(is.na(dat_out$lat[i])){
        
        dat_out[,paste0('Nearest_',k,'_Same')] = NA

      }else{ #if we have a geocode
        Prox1R = all_mat[i,] #ith person's distances to others in neighborhood
        dat_out[i,paste0('Nearest_',k,'_Same')] = sum(dat_out[order(Prox1R)[1:k],'minor'] == dat_out$minor[i], na.rm = T) #count number of nearest-K who are same minority status
      }
    }
  }
  return(dat_out) #output has variable for the knn score for each geocoded individual in the neighborhood
}
