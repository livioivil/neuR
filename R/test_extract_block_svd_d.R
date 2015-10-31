#extract the singular values of M
.extract.block.svd.d <-function(M) svd(M,0,0)$d