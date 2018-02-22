##' @name peaks counter via number of roots
##' @description The function counts how many maxima the data \code{y} have, when
##' treated as a continuous function of time. To take all the number of maxima we solve
##' the smoothed first derivative of the funtion using the uniroot.all function. See 'Details' for more information.
##' @author Livio Finos, Erlis Ruli,	Euloge Clovis Kenne Pagui
##' @details Given the time series \code{y}, the function \code{.find.peaks} creates a smooting
##' spline \eqn{f(t)}{f(t)} to the time series and computes its first order derivative \eqn{f^'(t)}{f'(t)} at the time point \eqn{t}{t}. Then, a data set is created by taking the values of the derivative at each time point \eqn{t}{t}. To this dataset we fit a two-component mixture of Gaussian densities. Let the second group be the mixture 
##' be the one with the highest mean. Then, we select the samples that follow in this cluster. Based on this subsample we compute the median (med_2) and we build a grid \eqn{g = (g_1,\ldots,g_{n.points})}{g = (g_1, ..., g_{n.points})} of equi-spaced values in \eqn{[med_2 \pm \delta sd_2]}{[med_2 +- delta*sd_2]}. Finally, for each \eqn{d_i}{d_i}, we solve the equation \deqn{f^'(t) == d_i}{f'(t) == d_i} and count its roots. Lastly, we find the optimal number of roots as the value which has frequency of at least 4 at each iteration. 
##'
##' @export peaks
##' @examples 
##' \dontrun{
##' freq <- 8
#' thin <- 32
#' 
#' original.freq <- freq * thin
#' 
#' ## analizziamo un individuo alla volta
#' ID <- 2
#' 
#' dati_ID = read.table(paste("00", ID, ".txt", sep = ""))$V1
#' 
#' # ricampioniamo i dati ad una frequanza piu' bassa
#' # dati originari hanno freq 256
#' # ora hanno freq 8
#' # E' meglio lavorare con frequenze basse perche' altrimenti ci sono troppe
#' # misuarzioni è la procedura diventa molto lenta con tante osservazioni
#' # Se proprio si vuole lavorare sulle 256 frequenze allora bisognerebbe spezzare l'intervallo
#' # dei 10 min in 5 intervalli da 2 min ciascuno, cosi i calcoli sono ragionevolmente veloci,
#' # tuttavia questo significa che bisogna lanciare la stessa funzione 5 volte per ogni individuo.
#' 
#' # Possiamo ridurre la frequenza dei dati direttamente e automaticamente in R.
#' # Qui prendo la prima misurazione in un intervallo di "thin = 32" valori
#' 
#' y <- dati_ID[seq(1, length(dati_ID), by = thin)]
#' 
#' # per il significato degli argomenti della funzione vedi il file "peaksFn.R"
#' # ricorda:
#' #         "min.intertime.in.sec": piu' è basso piu' è probabilie accettare picchi ravvicinati.
#' #         "degree": valori tipici sono 2 opure 3. Non abbiamo notato grossisime differenze
#' #                  tra usare 2 o 3. Il valore di 3 cerca di accomodare maggiormente i dati.
#' IDout <- peaks(y = y,
#'                min.intertime.in.sec = 0.25,
#'                degree = 2,
#'                freq. = freq,
#'                plot.split = 6)
#' 
#' # salviamo il risultato nella directory di lavoro
#' write.table(
#'   x = IDout$hb.time,
#'   file = paste("time_00", ID, ".txt", sep = "")
#' )
#' write.table(
#'   x = diff(IDout$hb.time)/freq,
#'   file = paste("RR_00", ID, ".txt", sep = "")
#' )
#' }
#' 

peaks <- function(y,
                  n.grid = 100,
                  divide.by = 100,
                  cut.here = NULL,
                  plot.split,
                  degree = 2,
                  min.intertime.in.sec = 0.3,
                  freq.,
                  smooth){
  switch(smooth,
         yes = .find.peaksL(y = y, degree = degree,
                            min.intertime.in.sec = min.intertime.in.sec,
                            plot.split = plot.split, freq. = freq.),
         no = .find.peaksEC2(y = y, n.grid = n.grid, divide.by = divide.by,
                             cut.here = cut.here,
                             plot.out = TRUE))
}

.find.peaksL <- function(y,
                        degree = 2,
                        min.intertime.in.sec = 0.3,
                        plot.split = 6,
                        freq.){
  y[which(y < 25)] = 25
  y[which(y > 55)] = 50
  n = length(y)
  
  #time resolution points/sec
  # time.resol = n/(2*60)
  
  # espresso in punti
  
  # min.intertime = min.intertime.in.sec*time.resol
  
  min.intertime = min.intertime.in.sec*freq.
  
  mod = lm(y ~ splines::bs(1:n, knots = seq(1,n,by=min.intertime),
                  degree = degree))
  x.sp = predict(mod)
  fun.yx <- splinefun(x = 1:n, y = y)
  
  # empirical first derivative
  d1 = diff(x.sp)
  # plot(d1, type = "l",xlim=c(1,5000))
  #now using splined data
  # d1 = diff(x)
  # lines(d1,col=2)
  
  #' find maxima starting from the first derivative
  find.0splus <- function(x.sp)  which((sign(x.sp[-1]) <= 0) & (sign(x.sp[-length(x.sp)]) >= 0))
  # find.0splus <- function(x)  which((sign(x[-1])<=0) & (sign(x[-length(x)])>=0))
  maxs = find.0splus(d1)
  
  # abline(0,0)
  # points(maxs,rep(0,length(maxs)),col="gray",pch=20)
  
  #' identifica massimi multipli (i.e. maxima which are too close each other) 
  maxs.keep = maxs[which(c(Inf, maxs[-1] - maxs[-length(maxs)]) >= min.intertime)]
  
  optimal.hbr <- length(maxs.keep)
  # fun.yx.sm = splinefun(y = x.sp, x = 1:n)
  
  hb.time <- rep(NA, optimal.hbr)
  # hb.time[1] <- 0
  low = mean(c(1,maxs.keep[1]))
  high =  mean(maxs.keep[1:2])
  tvec = seq(low, high, len = 1000)
  hb.time[1] <- tvec[which.max(fun.yx(tvec))]
  
  for(j in 2:(optimal.hbr-1)){
    
    low = mean(maxs.keep[c(j-1, j)])
    high = mean(maxs.keep[c(j, j+1)])
    tvec = seq(low, high, len = 1000)
    hb.time[j] <- tvec[which.max(fun.yx(tvec))]
  }
  
  low = mean(maxs.keep[c(optimal.hbr-1, optimal.hbr)])
  high = n
  tvec = seq(low, high, len = 1000)
  hb.time[optimal.hbr] <- tvec[which.max(fun.yx(tvec))]
  
  part <- floor(n/plot.split)
  
  if(plot.split == 4) {
  par(mfrow = c(min(c(plot.split,2)),1))
  
  plot(y = y[1:part], x = 1:part, type = "l", xlab = "time")
    # abline(h = cut.here, col = 1)
  points(x = hb.time[hb.time <= part],y = fun.yx(hb.time[hb.time <= part]),
         pch = "*", cex = 1.3, col = "red")
    
  plot(y = y[(part+1):(2*part)], x = (part+1):(2*part), type = "l",
       xlab = "time")
    # abline(h = cut.here, col = 1)
  points(x = hb.time[hb.time >= part+1 & hb.time<= 2*part],
         y = fun.yx(hb.time[hb.time >= part+1 & hb.time<= 2*part]),
         pch = "*", cex = 1.3, col = "red")
  
  plot(y = y[(2*part+1):(3*part)], x = (2*part+1):(3*part), type = "l",
       xlab = "time")
    # abline(h = cut.here, col = 1)
  
  points(x = hb.time[hb.time >= 2*part+1 & hb.time<= 3*part],
         y = fun.yx(hb.time[hb.time >= 2*part+1 & hb.time<= 3*part]),
         pch = "*", cex = 1.3, col = "red")
    
    plot(y = y[(3*part+1):(n)], x = (3*part+1):(n), type = "l",
         xlab = "time")
    # abline(h = cut.here, col = 1)
    points(x = hb.time[hb.time >= 3*part+1& hb.time<= n],
           y = fun.yx(hb.time[hb.time >= 3*part+1 & hb.time<= n]),
           pch = "*", cex = 1.3, col = "red")
  } 
  
  if(plot.split == 6) {
    pdf("prova.pdf")
    par(mfrow = c(6,1))
    par(mar=c(0,0,0,0))
    
    plot(y = y[1:part], x = 1:part, type = "l", xlab = "time")
    # abline(h = cut.here, col = 1)
    points(x = hb.time[hb.time <= part],y = fun.yx(hb.time[hb.time <= part]), pch = "*",
           cex = 1.3, col = "red")
    
    plot(y = y[(part+1):(2*part)], x = (part+1):(2*part), type = "l",
         xlab = "time")
    # abline(h = cut.here, col = 1)
    points(x = hb.time[hb.time >= part+1 & hb.time<= 2*part],
           y = fun.yx(hb.time[hb.time >= part+1 & hb.time<= 2*part]),
           pch = "*", cex = 1.3, col = "red")
    
    plot(y = y[(2*part+1):(3*part)], x = (2*part+1):(3*part), type = "l",
         xlab = "time")
    # abline(h = cut.here, col = 1)
    
    points(x = hb.time[hb.time >= 2*part+1 & hb.time<= 3*part],
           y = fun.yx(hb.time[hb.time >= 2*part+1 & hb.time<= 3*part]), 
           pch = "*", cex = 1.3, col = "red")
    
    plot(y = y[(3*part+1):(4*part)], x = (3*part+1):(4*part), type = "l",
         xlab = "time")
    # abline(h = cut.here, col = 1)
    
    points(x = hb.time[hb.time >= 3*part+1 & hb.time<= 4*part],
           y = fun.yx(hb.time[hb.time >= 3*part+1 & hb.time<= 4*part]),
           pch = "*", cex = 1.3, col = "red")
    
    plot(y = y[(4*part+1):(5*part)], x = (4*part+1):(5*part), type = "l",
         xlab = "time")
    # abline(h = cut.here, col = 1)
    points(x = hb.time[hb.time >= 4*part+1 & hb.time<= 5*part],
           y = fun.yx(hb.time[hb.time >= 4*part+1 & hb.time<= 5*part]),
           pch = "*", cex = 1.3, col = "red")
    
    plot(y = y[(5*part+1):(n)], x = (5*part+1):(n), type = "l",
         xlab = "time")
    # abline(h = cut.here, col = 1)
    points(x = hb.time[hb.time >= 5*part+1& hb.time<= n],
           y = fun.yx(hb.time[hb.time >= 5*part+1 & hb.time<= n]),
           pch = "*", cex = 1.3, col = "red")
dev.off()
  } 
  
  
  # points(maxs.keep,rep(0,length(maxs.keep)),col=3,pch=20)
  # return(length(maxs.keep))

  return(list("hbr" = optimal.hbr,
              "hb.time" = hb.time))
} 

## details This function counts the number of peaks by finding the solutions of the equation given by the first derivate equal to a small positive threshold The reason of why not zero is to avoid double peaks that frequently occour with such data. The threshold suitable for the data at hand is found semi-automatically as follows. Given \code{divide.by} parameter (here set to 100 by default) and \code{cut.here} (if not provided is internally set to 0), the function starts at iteration 1 by solving f'(x) = cut.here and counting the solutions. Obviously these solutions are maxima and minima. Then at iteration t, cut.here(t) is set to cut.here(t-1) + step.h, where step.h = max(y.prime)/(sum(y.prime>=0)/\code{divide.by}). The function will iterate until the number of solutions sought stabilises across 4 consecutive iterations. After the iterations we have many solutions and we select those having negative second derivative. These are the heart bits.
## 
.find.peaksEC2 <- function(y,
                          n.grid = 100,
                          divide.by = 100,
                          cut.here = NULL,
                          plot.out = TRUE){
  
  # time.select = which(time > start.end[1] & time < start.end[2])
  # n = length(time.select)
  # yobs = y[time.select]
  y[which(y < 25)] = 25
  y[which(y > 55)] = 50
  n = length(y)
  fun.yx = splinefun(y = y, x = 1:n)
  # fun.yx2 = splinefun(y = yobs2, x = 1:n)
  
  fun.yx.prime = function(x) fun.yx(x, deriv = 1)
  y.prime = fun.yx.prime(1:n)
  
  # mix.y.prime = normalmixEM(y.prime, k = 2)
  
  step.h = max(y.prime)/(sum(y.prime >= 0)/divide.by)
  # y.group2 = y.prime[mix.y.prime$posterior[,2] >= 0.5]
  
  # cut.hereM <- median(y.group2)
  # cut.here <- quantile(y.group2, probs = 0.25)
  
  # cut.here <- diff(range(y.group2))/3
  
  # low <- cut.here - delta*mix.y.prime$sigma[2]
  # low <- cut.here - delta*range(y.group2)/2
  #   if(low <= 0){
  #     low = 0
  #   }
  #   
  #   high = cut.here + delta*mix.y.prime$sigma[2]
  #   grid.lh <- seq(low, high, length.out = n.grid) 
  
  #   abline(h = high, col = 2)
  #   abline(h = cut.here)
  #   
  n.roots = rep(NA, n.grid)
  optimal.thresh = optimal.hbr = NA
  pb <- txtProgressBar(min = 1, max = n.grid, style = 3)
  
  if(is.null(cut.here)) {
    cut.here = 0.0
  }
  part <- floor(n/4)
  
  par(mfrow = c(4,1))
  
  for(i in 1:n.grid){
    cut.here = cut.here + step.h
    
    all.roots = uniroot.all(function(x) fun.yx.prime(x) - cut.here,
                            interval = c(1, n), n = n)
    # cat(paste("\nIteration:", i, "; # of roots found is:", length(all.roots),
    # "\r"))

    soc = fun.yx(all.roots, deriv = 2)
    roots = all.roots[soc < 0]
    n.roots[i] = length(roots)
    
    plot(y = y.prime[1:part], x = 1:part, type = "l", xlab = "time",
         ylab = "first  derivative")
    abline(h = cut.here, col = 1)
    plot(y = y.prime[(part+1):(2*part)], x = (part+1):(2*part), type = "l",
         xlab = "time", ylab = "first  derivative")
    abline(h = cut.here, col = 1)
    plot(y = y.prime[(2*part+1):(3*part)], x = (2*part+1):(3*part), type = "l",
         xlab = "time", ylab = "first  derivative")
    abline(h = cut.here, col = 1)
    plot(y = y.prime[(3*part+1):n], x = (3*part+1):n, type = "l",
         xlab = "time", ylab = "first  derivative")
    abline(h = cut.here, col = 1)
    
    setTxtProgressBar(pb, i)
    
    if(max(tabulate(na.omit(n.roots))) >= 4){
      optimal.hbr = n.roots[i]
      optimal.thresh = cut.here
      roots = roots
      break
    }
  }
    
    #     if (i > 5) {
    #       
    #       cond = rep(NA, 4)
    #       cond[1] = n.roots[i] == n.roots[i-1]
    #       cond[2] = n.roots[i] == n.roots[i-2]
    #       cond[3] = n.roots[i] == n.roots[i-3]
    #       cond[4] = n.roots[i] == n.roots[i-4]
    #       
    #       if (all(cond == TRUE)) {
    #         
    #       optimal.hbr = n.roots[i]
    #       optimal.thresh = grid.lh[i]
    #       
    #       break
    #     }
    #     }
    #     setTxtProgressBar(pb, i)
  # ff.opt <- fun.yx(c(1,roots))
  
  hb.time <- rep(NA, optimal.hbr)
  # hb.time[1] <- 0
  low = mean(c(1,roots[1]))
  high =  mean(roots[1:2])
  tvec = seq(low, high, len = 1000)
  hb.time[1] <- tvec[which.max(fun.yx(tvec))]
  
  for(j in 2:(optimal.hbr-1)){
    
    low = mean(roots[c(j-1, j)])
    high = mean(roots[c(j, j+1)])
    tvec = seq(low, high, len = 1000)
    hb.time[j] <- tvec[which.max(fun.yx(tvec))]
  }
  
  low = mean(roots[c(optimal.hbr-1, optimal.hbr)])
  high = n
  tvec = seq(low, high, len = 1000)
  hb.time[optimal.hbr] <- tvec[which.max(fun.yx(tvec))]
  
  if(plot.out == TRUE ){
  par(mfrow = c(4,1))
  plot(y = y[1:part], x = 1:part, type = "l", xlab = "time")
  # abline(h = cut.here, col = 1)
  points(x = hb.time[hb.time <= part], y = fun.yx(hb.time[hb.time <= part]), pch = "*", cex = 1.2)
  
  plot(y = y[(part+1):(2*part)], x = (part+1):(2*part), type = "l",
       xlab = "time")
  # abline(h = cut.here, col = 1)
  points(x = hb.time[hb.time >= part+1 & hb.time<= 2*part],
         y = fun.yx(hb.time[hb.time >= part+1 & hb.time<= 2*part]), pch = "*", cex = 1.2)
  
  plot(y = y[(2*part+1):(3*part)], x = (2*part+1):(3*part), type = "l",
       xlab = "time")
  # abline(h = cut.here, col = 1)
  points(x = hb.time[hb.time >= 2*part+1 & hb.time<= 3*part],
         y = fun.yx(hb.time[hb.time >= 2*part+1 & hb.time<= 3*part]), pch = "*", cex = 1.2)
  
  plot(y = y[(3*part+1):(n)], x = (3*part+1):(n), type = "l",
       xlab = "time")
  # abline(h = cut.here, col = 1)
  points(x = hb.time[hb.time >= 3*part+1& hb.time<= n],
         y = fun.yx(hb.time[hb.time >= 3*part+1 & hb.time <= n]), pch = "*", cex = 1.2)
  }
  
  return(list("thresh" = optimal.thresh,
              "hbr" = optimal.hbr,
              "hb.time" = hb.time))
}

