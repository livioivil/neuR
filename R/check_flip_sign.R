## controlla i contributi di ogni pc_i e cambia il segno di loding_i e scores_i 
## in modo tale che la maggior parte siano positivi
check.flip.sign <- function(sv){
    flip=median(sv$v[,1])<0
    if(flip){
      sv$u[,1]=-sv$u[,1]
      sv$v[,1]=-sv$v[,1]
    }
    if (ncol(sv$u)>1) 
      for (i in 2:ncol(sv$u)){
        flip=median(sv$v[,i])<0
        if(flip){
          sv$u[,i]=-sv$u[,i]
          sv$v[,i]=-sv$v[,i]
        }
      }
    sv
}
