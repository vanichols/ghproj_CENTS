# created 25/3/2024
# standardize visuals

Sys.setenv(LANG = "en")

library(ggplot2)


# themes ------------------------------------------------------------------

theme_set(theme_bw())

th1_x45 <- 
  theme(
  strip.text = element_text(size = rel(1.5)),
  strip.background = element_rect(fill = "tan"),
  axis.text = element_text(size = rel(1.2)),
  axis.title = element_text(size = rel(1.3)),
  axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.9)),
  #axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
  plot.title = element_text(size = rel(2))
)

th1_yhoriz <- 
  theme(
    strip.text = element_text(size = rel(1.5)),
    strip.background = element_rect(fill = "tan"),
    axis.text = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.3)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.9)),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    plot.title = element_text(size = rel(2))
  )

th1_legend <- 
  theme(
    strip.text = element_text(size = rel(1.5)),
    strip.background = element_rect(fill = "tan"),
    axis.text = element_text(size = rel(1.2)),
    axis.title = element_text(size = rel(1.3)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.9)),
    #axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    plot.title = element_text(size = rel(2)),
    legend.position = "top",
    legend.text = element_text(size = rel(1.5))
  )




# colors ------------------------------------------------------------------


p2_blu <- "#025196"
p2_ylw <- "#FDB338"

p2_ltb <- "#2F67B1"
p2_red <- "#BF2C23"

p4_pnk <- "#D81B60"
p4_blu <- "#1E88E5"
p4_ylw <- "#FFC107"
p4_grn <- "#004D40"


p7_grn <- "#009E73"
p7_blu <- "#0072B2"
p7_ltb <- "#56B4E9"
p7_ylw <- "#F0E442"
p7_ora <- "#E69F00"
p7_dko <- "#D55E00"
p7_pur <- "#CC79A7"

p5_ltb <- "#5B8EFD"
p5_blu <- "#725DEF"
p5_pnk <- "#DD217D"
p5_ora <- "#FF5F00"
p5_ylw <- "#FFB00D"
