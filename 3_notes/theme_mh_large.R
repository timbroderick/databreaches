theme_mh_large <- function (...) { 
  #theme_grey() +
  theme_set(theme_get() + theme(text = element_text(family = 'Arial', size = 11.5, lineheight=0.9))) + 
    
    theme(
      # add padding to the plot
      plot.margin = unit(rep(0.5, 4), "cm"),
      # remove the plot background and border
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      ## make the legend and strip background transparent
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "transparent", colour = NA),
      # put the legend across the top
      # This puts the legend across the top
      legend.position="top", 
      legend.direction="horizontal",
      ## add light, dotted major grid lines only
      panel.grid.major.x = element_line(linetype = "dotted", colour = "#C6C7C8", size = 0.3), #757575
      panel.grid.major.y = element_line(linetype = "solid", colour = "#C6C7C8", size = 0.3), # 9D9FA2
      panel.grid.minor = element_blank(),
      ## remove the axis tick marks and hide axis lines
      axis.ticks = element_blank(),
      axis.line = element_line(color = "#FFFFFF", size = 0.3),
      ## modify the bottom margins of the title and subtitle
      plot.title = element_text( face = 'bold', color = "#000000", size = 11, hjust = 0, margin = margin(b = 4)),
      plot.title.position = "plot",
      plot.caption.position= "plot",
      plot.subtitle = element_text(size = 9, hjust = 0, margin = margin(b = 10)),
      ## add padding to the caption
      plot.caption = element_text(size = 7.5, face = "oblique", hjust = 0 , margin = margin(t = 15)),
      ## tweack axes titles, tick labels, legend title and legend key, and strip text
      axis.title.x = element_text(size = 9, face = "plain", hjust = 0.5),
      axis.title.y = element_text(size = 9, face = "plain", hjust = 0.5),
      axis.text = element_text(size = 8, face = "plain"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 10, face = "bold"),
      ...
    )
}


mhcolors <- c("#237a82", "#99c2cb", "#de6427", "#fbc854", "#5e152d", "#ae1b1f", "#508ebc", "#3d505a")

alternating_colors <- c("#237a82","#99c2cb","#237a82","#99c2cb","#237a82","#99c2cb")
