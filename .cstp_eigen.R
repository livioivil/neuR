.cstp_eigen<-function(matrix,threshold=NULL,quali=NULL){
  
  #Decomposizione spettrale della
  #matrice avuta come input
  eig_matrix=eigen(matrix)
  
  if((is.null(threshold))||(threshold!="autovalori")){
    threshold=(1/10^6)*eig_matrix[[1]][1]
    #Tengo solo gli autovalori che soddisfano tale requisito
    eig.in_matrix=eig_matrix[[1]][which(eig_matrix[[1]]>threshold)]
    
    #Tengo gli autovettori corrispondenti
    eiv.in_matrix=eig_matrix[[2]][,which(eig_matrix[[1]]>threshold)]
    
    EI=list(eig.in_matrix,eiv.in_matrix,threshold)
  }
 else if(threshold=="autovalori"){
    #Tengo solo gli autovalori indicati dall'utente
    eig.in_matrix=eig_matrix[[1]][1:quali]
    
    #Tengo gli autovettori corrispondenti
    eiv.in_matrix=eig_matrix[[2]][,1:quali]
    
    EI=list(eig.in_matrix,eiv.in_matrix)
    }
  return(EI)
}