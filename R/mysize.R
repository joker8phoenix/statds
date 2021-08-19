############
### size ###
############
metallic.ratios = function(x, k=1){
  (k+sqrt(k^2+4))*.5*x
}
sh <- 8.27*0.8
sw <- sh*sqrt(2)
gh <- sqrt((sqrt(10)-sqrt(2))/2)*sh
gw <- gh*(1+sqrt(5))/2
png.sh <- 320
png.sw <- png.sh*sqrt(2)
png.gh <- sqrt((sqrt(10)-sqrt(2))/2)*png.sh
png.gw <- png.gh*(1+sqrt(5))/2
pdf.sh <- 4.5*0.8
pdf.sw <- pdf.sh*sqrt(2)
pdf.gh <- sqrt((sqrt(10)-sqrt(2))/2)*pdf.sh
pdf.gw <- pdf.gh*(1+sqrt(5))/2

silver.h <- 8.27
silver.w <- silver.h*sqrt(2)
gold.h <- sqrt((sqrt(10)-sqrt(2))/2)*silver.h
gold.w <- gold.h*(1+sqrt(5))/2
