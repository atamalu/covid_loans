require(ggthemr)

##### ggthemr setup ---------------
cols <- c("#555555", '#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467BD', '#8C564B', '#CFECF9', '#7F7F7F', '#BCBD22', '#17BECF')

thme <- define_palette(swatch = cols,
                       gradient = c(lower = cols[1L], 
                                    upper = cols[2L]), 
                       background = "#FFFFFF",
                       gridline = 'gray90')
ggthemr(thme) # sets theme

##### further theme tweaks ---------------

### bar graphs
theme.tweaks.bar <- theme(
  axis.title.x = element_text(face = 'bold'),
  axis.title.y = element_text(face = 'bold',
                              margin = margin(r = 18)),
  plot.title = element_text(size = 16, 
                            hjust = 0.5,
                            margin = margin(b = 20)),
  plot.caption = element_text(size = 8),
  panel.grid.major.x = element_blank(),
  axis.ticks = element_blank()
)


