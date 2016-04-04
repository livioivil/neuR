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
  # matrice F(s)XF(t) dove X è la media dei trials 
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