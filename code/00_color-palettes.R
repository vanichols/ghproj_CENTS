# color palettes

library(RColorBrewer)
library(ggtext)
library(scales)

a <- RColorBrewer::brewer.pal(11, "RdYlBu")

hue_nocc <- "gray70"
hue_radm<- "#FDAE61"
hue_radl <- "#A50026"
hue_mixe <- "#313695"
hue_mixm <- "#ABD9E9"

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
  

# make nice label fxn -----------------------------------------------------

MakeNiceLabels <- function(dat){
  d <- dat %>% 
    mutate(cctrt_nice = case_when(
      cctrt_id == "nocc" ~ "NoCC",
      cctrt_id == "mix_E" ~ "MixE",
      cctrt_id == "mix_M" ~ "MixM",
      cctrt_id == "rad_M" ~ "RadM",
      cctrt_id == "rad_L" ~ "RadL",
      TRUE~"XXX"
    ),
    cctrt_id = factor(cctrt_id, levels = ord.cctrt_id),
    cctrt_nice = factor(cctrt_nice, levels = ord.cctrt_niceS)) %>% 
    #--make nice till
    mutate(
      till_id = factor(till_id, levels = ord.till_id),
      till_nice = case_when(
        till_id == "notill" ~ "No-till",
        till_id == "inversion" ~ "Inv",
        till_id == "surface" ~ "Surf",
        TRUE ~ "XXX"
      ),
      till_nice = factor(till_nice, levels = ord.till_nice))
  return(d)
}




# th1 <- theme(strip.background = element_rect(fill = "gray80"),
#              strip.text = element_text(size = rel(1.5)))


th1 <- theme(strip.background = element_rect(fill = "transparent"))


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

ord.cover_cat2 <-  c("Soil", "Cover crop", "Weed", "Volunteer")
ord.cover_cat3 <-  c("Soil", "Cover crop", "Other (weed)", "Volunteer")

ord.crop <- c("2018 (Spring barley)", "2019 (Spring oat)", "2020 (Faba bean)")


# labels ------------------------------------------------------------------

myyieldlab <- (expression(atop("Dry grain yield", paste("(Mg "~ha^-1*")"))))

mybmlab <- (expression(atop("Fall biomass", paste("(Mg "~ha^-1*")"))))

myweedcountlab <- (expression(atop("Spring weed count", paste("(plants "~m^-2*")"))))

