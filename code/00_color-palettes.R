# color palettes

library(RColorBrewer)

a <- RColorBrewer::brewer.pal(11, "RdYlBu")


av1 <-"#A50026"
av2 <-"#FDAE61"
av3 <-"#FFFFBF"
av4 <- "#ABD9E9"
av5 <- "#313695"

c1 <- "#a0ccea"
c2 <- "#f66255"
c3 <- "#faae05"


dkbl1 <- "#2d2d8a"
ylw1 <- "#ffc000"

#bv1 <- "#6F3A02"
bv1 <- "#5c3002"
bv2 <- "#179A2A"
bv3 <- "#E8E385"

cv1 <- '#f0f921'
cv2 <- '#f89540'
cv3 <- '#cc4778'
cv4 <- '#7e03a8'
cv5 <- '#0d0887'
  
  
th1 <- theme(strip.background = element_rect(fill = "gray80"),
             strip.text = element_text(size = rel(1.5)))


# nice labels -------------------------------------------------------------

#--desired cctrt_id order
ord.cctrt_id <- c("nocc", "mix_E", "mix_M", "rad_M", "rad_L")

#--desired cctrt_nice order
ord.cctrt_niceS <- c("NoCC", "MixE", "MixM", "RadM", "RadL")
ord.cctrt_niceL <- c("NoCC", "MixEarly", "MixMid", "RadMid", "RadLate")

ord.cctrt_niceNEW <- c("No CC", "Early Mix", "Mid Mix", "Mid Rad", "Late Rad")
ord.cctrt_nicePRES <- c("No CC", "+early mix", "+mid mix", "+mid rad", "+late rad")


#--desired till_id order
ord.till_id <- c("notill", "surface", "inversion")

#--desired till_nice order
ord.till_nice <- c("No-till", "Surf", "Inv")
ord.till_nice2 <- c("No-till", "Surface", "Inversion")

ord.cover_cat2 <-  c("Soil", "Cover Crop", "Weed", "Volunteer")

ord.crop <- c("Spring barley (2018)", "Oat (2019)", "Faba bean (2020)")


# labels ------------------------------------------------------------------

myyieldlab <- (expression(atop("Dry grain yield", paste("(Mg "~ha^-1*")"))))

mybmlab <- (expression(atop("Fall biomass", paste("(Mg "~ha^-1*")"))))

myweedcountlab <- (expression(atop("Spring weed count", paste("(plants "~m^-2*")"))))

