# color palettes

library(RColorBrewer)

a <- RColorBrewer::brewer.pal(11, "RdYlBu")


av1 <-"#A50026"
av2 <-"#FDAE61"
av3 <-"#FFFFBF"
av4 <- "#ABD9E9"
av5 <- "#313695"


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
