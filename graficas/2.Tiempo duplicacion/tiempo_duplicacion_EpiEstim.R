##calcular tasa de duplicacion y numero reproductivo R
library(EpiEstim)
library(incidence)
library(TTR)


Sys.setlocale("LC_TIME", "es")

##leer datos de casos confirmados 
dias = covidMex::covidWWSituation() %>%
  filter(geo_id =="MX") %>%
  select(fecha_corte, casos_nuevos, decesos) %>%
  mutate(Fecha = ymd(fecha_corte)) %>%
  arrange(Fecha) %>%
  ##mantener despues del 28 de febrero
  filter(Fecha > "2020-02-28")

##crear variables
diasData = dias %>%
  mutate(Casos = cumsum(casos_nuevos),
         Casoslog=log(Casos),
         Dia = row_number()
         ) %>%
  rename(Muertos = decesos,
         Nuevos = casos_nuevos) %>%
  select(-fecha_corte) %>%
  arrange(Dia) %>%
  mutate(fecha= paste(day(Fecha), month(Fecha, label = T)),
         Muertoslog = log(Muertos)
         )



##mantener desde el dia que llegamos a los 100 casos
for_chart  = diasData %>%
  filter(Casos>100) %>%
  mutate(r = (Casos-lag(Casos))/lag(Casos),
         tasa_dup = log(2)/r,
         tasa_dup9 = round(SMA(tasa_dup, n=7),digits = 1)
  ) %>%
  filter(!is.na(tasa_dup9))




##Estimar numero reproductivo R
res_parametric_si <- estimate_R(for_chart$Casos, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 4.7, 
                                  std_si = 2.9))
)



##data para numero reproductivo R
r_data = res_parametric_si$R
plot(res_parametric_si, legend = FALSE)









##crear grafica de tiempo de duplicacion
ggplot(data =  for_chart,
        aes(x=Fecha,
            y = tasa_dup9
            )
       )+
  ##puntos
  geom_point(shape = 21,
             size = 5,
             colour = "red",
             fill ='white'
             
  ) +
  ##fit line (usando un modelo linear)
  stat_smooth(se = T,
              color ="#FE5B5D",
              fill = "#D0D0D0",
              size = 2) +
  ##incluir label cada 5 observaciones
  geom_label(data = for_chart[seq(nrow(for_chart), 1,-5), ],
             aes(label=paste(tasa_dup9, "dias")),
             nudge_y = .5,
             color ='red',
             fill = "white",
             label.size=0)+
  
  ##anotar que la serie comienza desde el dia 100
  annotate("text", x=as.Date("2020-03-27"), y= 7, 
           label="Primer dia con\n100casos") +

  ##establecer limites del eje x
  scale_y_continuous(limits = c(2,8),
                     name="") +
  theme_classic()
  


# 
# 
# 
#   theme(text = element_text(family="Poppins",
#                             color = "#333333",
#                             size = 14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(family = "Poppins"),
#         plot.subtitle = element_text(size = 12,
#                                      hjust = .5),
#         
#   ) +
#     xlim(0, 10)
#   
#   labs(title = "Tiempo de duplicación de casos confirmados por\n COVID19 en México",
#        subtitle = "Cuántos días tardan los casos confirmados en duplicarse",
#        x = NULL,
#        y ="Días",
#        caption = "Creado por @AndresArau\n
#        Datos de la @ssalud obtenidos a través del paquete covidMX (Reyes,2020) \n y el repositorio covid19_mex(Guzmán, 2020)."
#   )
# 
# 
