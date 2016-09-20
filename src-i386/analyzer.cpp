#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::mat get_eventpoints(arma::vec xts, double threshold, int window) {
  
  int n = xts.size();
  bool event = FALSE;
  
  arma::mat event_mat(n,2);
  
  int event_idx = 0;
  
  for(int i = 0; i < n; i++) {
    
    if ((xts(i) > threshold) && (event == FALSE)) { 
      
      event = TRUE;
      
      event_mat(event_idx, 0) = i;
      
      i += 1;
      
    }
    
    while ( (i < (n - window) ) && (event == TRUE) ) {
      
      if (arma::max(xts.subvec(i,i+window)) <= threshold) {
        
        event = FALSE;
        
        event_mat(event_idx, 1) = i;
        
        event_idx += 1;
        
      }
      
      i += 1;
      
    }
    
  }
  
  event_mat.resize(event_idx, 2);
  
  return(event_mat);
  
}