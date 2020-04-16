


#style graphs
tema_linea <- theme_minimal() +
  theme(text = element_text(family="Poppins", 
                            color = "#333333",
                            size = 14
  ),
  plot.title = element_text(size = 13.5, 
                            face = "bold", 
                            margin = margin(10,0,20,0), 
                            family="Poppins", color = "grey25"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  axis.text.x= element_text(angle=90, hjust=1),
  axis.text = element_text(family = "Poppins")
  ) 


size_point =5
color_chart = "steelblue"
size_line = 1




