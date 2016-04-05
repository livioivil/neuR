#' @description (English)
#'              Function that extract signal from fNIRS data, using 
#'              a spatio-temporal filter made up of a pair of matrices
#'              whose number of columns is that that maximize the SNR of
#'              the class under analysis.
#'              A class corresponds to a specific stimulus.
#'              The pair of matrices is achieved by a two step procedure
#'              1° step--> denoising of the data with a pair of matrices
#'                         make up of eigenvalues and egeinvectors of the
#'                         spatial and temporal variance and covariance matrix
#'                         of the data. Applying this pair of matrices 
#'                         to the mean of the trial of the z class, you get
#'                         clean data
#'              2° step--> In this step we apply the singular values 
#'                         decomposition to the clean data obtained in the 
#'                         previous step. The number of the components, that
#'                         are found through the svd that are to be keep, is 
#'                         automatically 
#'                         chosen from the function in a way to maximize
#'                         the SNR of the class under analysis
#'              (Italian)           
#'              Funzione che estrae il segnale da dati fNIRS 
#'              eseguendo un filtraggio spazio-temporale servendosi di 
#'              una coppia di matrici il cui numero di colonne 
#'              è scelto automaticamente dalla funzione in
#'              modo da massimizzare il SNR della classe in esame.
#'              La classe corrisponde a un determinato stimolo.
#'              La coppia di matrici è costruita secondo una procedura
#'              a due passi:
#'              1° passo --> fase di denoising dei dati attraverso 
#'                           la costruzione di due matrici ottenute 
#'                           a seguito della decomposizione spettrali
#'                           delle matrici di varianza e covarianza
#'                           spaziale e temporale dei dati. Ottenute tali 
#'                           matrici, si applicano alla media dei 
#'                           trials della classe z.
#'               2° passo--> estrazione dell'informazione dai dati puliti 
#'                           al passo 1°. La media filtrata al passo 1°
#'                           viene decomposta con il metodo di 
#'                           decomposizione ai valori singolari. Il
#'                           numero di componenti ottenute dalla svd 
#'                           da tenere, è scelto in maniera automatica 
#'                           dall'algoritmo. Tale numero è scelto 
#'                           in maniera tale da massimizzare il
#'                           SNR della classe in esame. 
#' @title Function for fNIRS signal extraction
#' @name ACSTP
#' @param dat array (time X channel X trial) containig all the trials
#' @param dots array (time X channel X trial) containing the 
#'             trials of z class
#' @param n.c names of the rows with which build mask up, 
#'            for the automatic choice of the subspace dimension  
#' @param time names of the columns with which build mask up, for 
#'             the automatic choice of the subspace dimension
#' @param threshold
#'        default: the data are cleaned choosing the eigenvalues and
#'                 eigenvectors of spatial and temporal variance and
#'                 covariance matrix that are bigger than a threshold
#'                 equal to (1/10^6)* 1° eigenvalues of the spatial 
#'                 variance and covariance matrix
#'        if threshold = "autovalori": only the eigenvalues and the corresponding
#'                                     eigenvectors from the first
#'                                     to that specified by the parameter "quali"
#'                                     are kept  
#' @param quali specifies the last eigenvalue and corresponding eigenvector
#'              of the spatial and temporal covariance matrix 
#'              that are to be kept starting from the first.
#'              quali is not NULL only when threshold is equal to "autovalori" 
#' @return a list containing in this order:
#'         p subspace dimension
#'         Q matrix containing the signal extracted with the function
#'         weights a vector of weights for evry trials 
#'         SNR a number that indicate the SNR of the extracted signal in dB 
#' @author Michelle Viola
#' @seealso link https://sites.google.com/site/marcocongedo
#' @export


acstp<-function(dat,dots,n.c,time,threshold=NULL,quali=NULL){
  
  # Inizializzazione di alcune quantità
  Q.mask=list()
  Q=list()
  SNR=0
  weights=list()
  
  channels=c("A1.HbO","A1.Hb","A2.HbO","A2.Hb","A3.HbO","A3.Hb","A7.HbO","A7.Hb","A8.HbO","A8.Hb","B3.HbO","B3.Hb","B4.HbO",
             "B4.Hb","B5.HbO","B5.Hb","B6.HbO","B6.Hb","B7.HbO","B7.Hb","C1.HbO","C1.Hb","C2.HbO",
             "C2.Hb","C3.HbO","C3.Hb","C7.HbO","C7.Hb","C8.HbO","C8.Hb","D3.HbO","D3.Hb","D4.HbO","D4.Hb","D5.HbO",
             "D5.Hb","D6.HbO","D6.Hb","D7.HbO","D7.Hb")
  
  t=c(-0.881,-0.752,-0.641,-0.496,-0.366,-0.255,-0.111,0.000,0.145,0.272,0.386,0.528,0.641,0.768,
      0.912,1.024,1.168,1.295,1.409,1.553,1.665,1.792,1.936,2.050,2.193,2.321,2.433,2.577,2.688,
      2.818,2.960,3.073,3.218,3.345,3.456,3.600,3.711,3.841,3.985,4.097,4.241,4.368,4.482,4.625,4.738,
      4.865,5.009,5.120,5.264,5.394,5.505,5.650,5.761,5.890,6.032,6.146,6.289,6.418,6.529,6.673,6.803,
      6.914,7.059,7.170,7.314,7.441,7.555,7.698,7.811,7.938,8.082,8.193,8.337,8.464,8.578,8.723,8.834,
      8.961,9.105,9.219,9.362,9.475,9.602,9.746,9.858,9.986,10.130,10.243,10.387,10.514,10.625,10.770,
      10.881,11.011,11.155,11.266,11.410,11.537,11.651,11.794,11.907)
  
  dimnames(dat)[[1]]=t
  dimnames(dots)[[1]]=t
  dimnames(dat)[[2]]=channels
  dimnames(dots)[[2]]=channels
  
  
  # Inizializzo i pesi per il calcolo della 
  # media pesata dei trials della classe z
  g=0
  for(i in 1:dim(dots)[3]){
    g[i]=1/norm(dots[,,i],type="F")
  }
  
  # Calcolo le matrici di cov spaziale e 
  # temporale a prescindere dalla classe 
  cov.s=.cstp_cov(dat,type="spatial")
  cov.t=.cstp_cov(dat,type="temporal")
  
  # Tengo solo alcuni degli autovalori 
  # delle matrici di covarianza spaziale e temporale
  if((is.null(threshold))||(threshold!="autovalori")){
    list.eigen_s=.cstp_eigen(cov.s,threshold=NULL,quali=NULL)
    list.eigen_t=.cstp_eigen(cov.t,threshold=list.eigen_s[[3]],quali=NULL)
  }
  else if(threshold=="autovalori"){
      list.eigen_s=.cstp_eigen(cov.s,threshold="autovalori",quali=quali)
      list.eigen_t=.cstp_eigen(cov.t,threshold="autovalori",quali=quali)
  }
  
  # Costruisco le matrici che operano una pulizia dei dati 
  fs=.cstp_filt(list.eigen_s)
  ft=.cstp_filt(list.eigen_t)
  
  # e le loro inverse
  gs=.cstp_inv_filt(list.eigen_s)
  gt=.cstp_inv_filt(list.eigen_t)
  
  # Media pesata dei trials classe z
  # dal min(dim(X)), ricavo il valore massimo di P
  # ovvero della sottodimensione
  maxP=min(dim(t(ft)%*%.cstp_avg(dots,g)%*%fs))
  
  # Ciclo che per ogni valore di P,
  # la sottodimensione, calcola i filtri,
  # i pesi, e nuovamente i fltri con i pesi
  # stimati al punto precedente
  
  for(j in 2:maxP){
    filtri.list=.cstp_output(fs,ft,gs,gt,dots,g,P=j)
    c=.cstp_weight(filtri.list,dots)
    z=.cstp_output(fs,ft,gs,gt,dots,c,P=j)
    
    # Salvo i pesi per ogni j
    weights[[j]]=c
    
    # Calcolo la media dei trial classe z
    X=.cstp_avg(dots,c) 
    
    # Assegno i nomi delle dimensioni
    rownames(X)=t
    colnames(X)=channels
    
    #creo una maschera dove sia più probabile trovare la risposta evocata 
    #sia a livello temporale che di derivazioni 
    Q.mask[[j]]=z[[4]]%*%t(z[[2]])%*%.cstp_mask(X,names.channels=n.c,t=time)%*%z[[1]]%*%t(z[[3]])
    Q[[j]]=z[[4]]%*%t(z[[2]])%*%X%*%z[[1]]%*%t(z[[3]])
    
    #maxSNR(p)=F(mask)/F(media filtrata)
    SNR[j]=norm(Q.mask[[j]],type="F")^2/norm(Q[[j]],type="F")^2
  }
  p=which(SNR==max(SNR))
  rownames(Q[[p]])=t
  colnames(Q[[p]])=channels
  SNR=log10(SNR[p])*10
  
  S=list(p,Q[[p]],weights[[p]],SNR)
  return(S)
}