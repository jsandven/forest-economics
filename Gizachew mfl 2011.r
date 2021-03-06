#https://www.researchgate.net/publication/254358271_Density-growth_relationships_in_thinned_and_unthinned_Norway_spruce_and_Scots_pine_stands_in_Norway

#Gizachew


SI <- 26
age_ij <- 60
Gij <- 1:90
P <- 0

PAI_vol_spruce <- function(SI,Gij,Tij, P) {
  #P = 1 if thinned and 0 otherwise
  #Spruce function after Gizachew et al 2011.
  a0 <- -19.1206 # constant
  a1 <- 4.2142 # site
  a2 <- 0.6836 # age
  a3 <- NA  # site x age
  b0 <- NA # constant
  b1 <- 1.4496 #site
  b2 <- 3.0003 #  age
  b3 <- NA # site x age
  c <- 0.0358 # autcorrelation
  d1 <- 0.1038 # thinning
  a = a0 + (a1 * SI) + age_ij ^ a2 
  b = b1 * SI + b2 * age_ij
  l1 <- a * Gij
  l2 <- b + Gij
  uij <- c
  PAIij = ((a * Gij) / (b + Gij)) * (1 + d1 * P)
  return(PAIij)
}

#df <- data.frame(Gij)
#df$S1 <- PAI_vol_spruce(17,Gij,40, 0)
#df$S2 <- PAI_vol_spruce(17,Gij,40, 1)
#df$S3 <- PAI_vol_spruce(20,Gij,40, 0)
#df$S4 <- PAI_vol_spruce(20,Gij,40, 1)

# df$y <- y
# df$MAI <- y / age_ij

# ggplot(df, aes(x=Gij)) + 
#  geom_line(aes(y = S1), color = "black") + 
#  geom_line(aes(y = S2), color="black", linetype="twodash") +
#  geom_line(aes(y = S3), color="grey") +
#  geom_line(aes(y = S4), color="grey", linetype="twodash") + ylab("m3/ha/år") + xlab("Volum m3/ha")


