#https://www.researchgate.net/publication/263718137_Stand_responses_to_initial_spacing_in_Norway_spruce_plantations_in_Norway
#Gizachew

SI <- 26
spacing <- 4
age <- 1:200
# Figure 2. Basal area and volume development predicted by the fixed effects parameters of the mixed models for four initial spacings (from
# top to bottom 2, 4, 8, and 16 m2
# ) at three site indices: SI26 m (solid black lines), SI 20 m (gray solid lines), and SI 14 m (gray broken
# lines).

BAmax <- 68.0752 * (1 - exp(-0.4831 * SI)) ^ 11.7149

vol_spruce_gizachew_2012 <-
  function(SI, spacing, age, method  = "Volume") {
    Vmax <- 829.1434 * (1 - exp(-0.2074 * SI)) ^ 3.9178
    b1 <-  0.0166
    b2 <-  0.0605
    b3 <- -0.1055
    c1 <-  0.7110
    di <-  0.5190
    dk <- 0.000005
    a <- Vmax
    b <- b1 * exp(b2 * SI) * spacing ^ b3 + dk
    c <- SI ^ c1 + di
    
    if (method == "Volume") {
      y = a * ((1 -  exp(-b * age))) ^ c
      out <- y
    }
    
    if (method == "CAI") {
      cai = (a * b * c * (1 - exp(-b * age)) ^ (c - 1) * exp(-b * age))
      out <- cai
    }
    
    return(out)
    
  }

ba_spruce_gizachew_2012 <-
  function(SI, spacing, age, method  = "Ba") {
    BAmax <- 68.0752 * (1 - exp(-0.4831 * SI)) ^ 11.7149
    b1 <-  0.0182
    b2 <-  0.0574
    b3 <- -0.1804
    c1 <-  0.5028
    di <-  0.5190
    dk <- 0.000005
    a <- BAmax
    b <- b1 * exp(b2 * SI) * spacing ^ b3 + dk
    c <- SI ^ c1 + di
    
    if (method == "Volume") {
      y = a * ((1 -  exp(-b * age))) ^ c
      out <- y
    }
    
    if (method == "CAI") {
      cai = (a * b * c * (1 - exp(-b * age)) ^ (c - 1) * exp(-b * age))
      out <- cai
    }
    
    return(out)
    
  }

#vol_spruce_gizachew_2012(SI = 26,spacing = 4,age = 1:200, method = "CAI")
#ba_spruce_gizachew_2012(SI = 26,spacing = 4,age = 1:200, method = "CAI")
#di and dk are the random plot and random experiment effects, respectively.

#df <- data.frame(age)
#df$CAI <- vol_spruce_gizachew_2012(SI = 26, spacing = 4, age = df$age, method = "CAI")
#df$y <- vol_spruce_gizachew_2012(SI = 26, spacing = 4, age = df$age, method = "Volume")
#df$MAI <- df$y / age

#ggplot(df, aes(x = age)) +
#  geom_line(aes(y = CAI), color = "black") +
#  geom_line(aes(y = MAI), color = "black", linetype = "twodash") + ylab("m3/ha/år") + xlab("Alder") +  xlim(0, 110)





