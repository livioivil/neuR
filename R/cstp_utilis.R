.cstp_weight<-function(filtri.list,dots){
  B=filtri.list[[1]]
  D=filtri.list[[2]]
  A=filtri.list[[3]]
  E=filtri.list[[4]]
  
  wtemp=0
  w=0
  
  for(i in 1:dim(dots)[3]){
    wtemp[i]=norm(E%*%t(D)%*%dots[,,i]%*%B%*%t(A),type="F")/norm(dots[,,i]-(E%*%t(D)%*%dots[,,i]%*%B%*%t(A)),type="F")
  }
  wtemp
  for(i in 1:length(wtemp)){
    w[i]=(dim(dots)[3]*wtemp[i])/sum(wtemp)
  }
  return(w)
}


.sum.matrix<-function(dots.array){
  somma=matrix(0,nrow=dim(dots.array)[1],ncol=dim(dots.array)[2])
  for(i in 1:dim(dots.array)[3]){
    somma=somma+dots.array[,,i]
  }
  somma
}

.cstp_output<-function(f.s,f.t,g.s,g.t,Y,w,P){
  # Matrice F(s) dell'articolo
  fs=f.s
  # Matrice F(t) dell'articolo
  ft=f.t
  # Matrice G(s) dell'articolo
  gs=g.s
  # Matrice G(t) dell'articolo
  gt=g.t
  # Calcolo la media dei 
  # trials per la classe z
  trials.mean=cstp_avg(Y,w)
  
  # Decomposizione in valori singolari della 
  # matrice F(s)XF(t) dove X ? la media dei trials 
  # per la classe z
  Z=t(ft)%*%trials.mean%*%fs
  
  # Decomposizione ai valori singolari
  # dei dati puliti
  sv=svd(Z,nu=P,nv=P)
  
  # Filtri finali
  B=fs%*%sv[[3]]
  D=ft%*%sv[[2]]
  A=gs%*%sv[[3]]
  E=gt%*%sv[[2]]
  
  filtri=list(B,D,A,E)
  return(filtri)
}

########### .cstp_mask
.cstp_mask<-function(X,names.channels,t){
  r=dim(X)[1]
  c=dim(X)[2]
  Q=matrix(0,nrow=r,ncol=c)
  time=t
  pos.channels=which(match(colnames(X),table=names.channels)!="NA")
  Q[time,pos.channels]=X[time,pos.channels]
  return(Q)
}


############# .cstp_filt

.cstp_filt<-function(list.eigen){
  
  col.filt=matrix(0,nrow=dim(list.eigen[[2]])[1],ncol=dim(list.eigen[[2]])[2])
  #cicli for che mi calcola le colonne del filtro
  for(i in 1:length(list.eigen[[1]])){
    col.filt[,i]=list.eigen[[1]][i]^(-1/2)*list.eigen[[2]][,i]
    filt=col.filt
  }
  return(filt)
}

############# .cstp_inv_filt
.cstp_inv_filt<-function(list.eigen){
  
  col.filt=matrix(0,nrow=dim(list.eigen[[2]])[1],ncol=dim(list.eigen[[2]])[2])
  
  #cicli for che mi calcola le colonne del filtro
  for(i in 1:length(list.eigen[[1]])){
    col.filt[,i]=list.eigen[[1]][i]^(1/2)*list.eigen[[2]][,i]
    filt=col.filt
  }
  return(filt)
}

############### .cstp_eigen
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


############# .cstp_cov
.cstp_cov<-function(trials.all,type="temporal"){
  
  pesi=1/dim(trials.all)[3]
  
  if(type=="spatial"){
    
    sum.cov.t=.sum.matrix(.cov.matrix(trials.all,type=type))
    cov.media=pesi*sum.cov.t
  }
  else{
    
    sum.cov.s=.sum.matrix(.cov.matrix(trials.all,type=type))
    cov.media=pesi*sum.cov.s
  }
}

##############.cstp_avg
.cstp_avg<-function(array,w){
  my.mean<-function(x){
    weighted.mean(x,w)
  }
  avg=apply(array,c(1,2),my.mean)
  avg=(1/sum(w))*avg
  return(avg)
}
###########################.cov.matrix
.cov.matrix<-function(trials.all,type="temporal"){
  if(type=="spatial")
    
    array.cov=array(NA,c(dim(trials.all)[2],dim(trials.all)[2],dim(trials.all)[3]))
  
  else{
    trials.all=aperm(trials.all,c(2,1,3))
    array.cov=array(NA,c(dim(trials.all)[2],dim(trials.all)[2],dim(trials.all)[3]))
    
  }
  
  for(i in 1:dim(array.cov)[3]){
    array.cov[,,i]=cov(trials.all[,,i])
  }
  
  array.cov
  
}