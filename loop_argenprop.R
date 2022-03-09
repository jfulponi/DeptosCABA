library(tidyverse)
library(lubridate)
library(httr)
library(rvest)

#final <- deptos 

deptos <- list()

for (pagina in 1:10){
  
  link_pagina <- paste0("https://argenprop.com/departamento-venta-localidad-capital-federal-pagina-", pagina) # link 
  
  venta_deptos <- read_html(link_pagina) # Le o código HTML do link
  
  precios <- venta_deptos %>% # Com o objeto pagina_celulares
    html_nodes(".card__price") %>% # Extrai o nó .CellPrice_MainValue__3s0iP
    html_text() # Extrai texto
  
  precios <- precios %>% # Com o objeto menor_preco
    str_remove_all('USD ') %>% # remove R$
    str_remove_all('\\.') %>% # remove .
    str_replace_all('\\,', '\\.') %>% # Substituir , por .
    as.numeric()
  
  direccion <- venta_deptos %>% # Com o objeto pagina_celulares
    html_nodes(".card__address") %>% # Nó da descrição. .css + .css
    html_text() %>% 
    str_remove_all('\\\n            ') %>% 
    str_remove_all('\\\n        ') %>% 
    str_remove_all(pattern = "\\,.*") %>% 
    str_remove_all(pattern = " al") 
  
  barrio <- venta_deptos %>% # Com o objeto pagina_celulares
    html_nodes(".card__title--primary") %>% 
    html_text() %>%
    str_remove_all("Departamento en Venta en ") %>% 
    str_remove(pattern = "\\,.*") %>% 
    str_replace_all(pattern ="Palermo .*", "Palermo") %>% 
    str_replace_all(pattern ="Belgrano .*", "Belgrano") %>% 
    str_replace_all(pattern = "Las Cañitas", "Palermo")
  
  features <- venta_deptos %>% # Com o objeto pagina_celulares
    html_nodes(".card__main-features") %>% 
    html_text() %>% 
    str_remove_all('\\\n                                \\\n\\\n                            \\\n                                 ') %>%  
    str_replace('\\\n                                \\\n\\\n                            \\\n                                 ', " - ") %>%  
    str_replace_all('\\\n                            \\\n                        \\\n                        ', " - ") %>% 
    str_remove_all('\\\n                            \\\n                        \\\n            ') 
  
  features <- features %>% 
    str_split(" - ")
  
  metros <- list()
  dorm <- list()
  antig <- list()
  banios <- list()
  
  for (i in 1:length(precios)){
    if(length(features[[i]][grep("m²", 
                                 features[[i]])])==0){
      metros[[i]] <- NA
    } else {
      metros[[i]] <- features[[i]][grep("m²", 
                                        features[[i]])]%>% 
        str_remove_all(" m²") %>% 
        as.numeric()
    }
    
    if(length(features[[i]][grep("dorm", 
                                 features[[i]])])==1){
      dorm[[i]] <- features[[i]][grep("dorm", 
                                      features[[i]])] %>% 
        str_remove_all(" dorm.") %>% 
        as.numeric()
    } else if (length(features[[i]][grep("Monoam.", 
                                         features[[i]])])==1){
      dorm[[i]] <- 0 } else {
        dorm[[i]] <- NA
      }
    if(length(features[[i]][grep(" años", 
                                 features[[i]])])==0){
      antig[[i]] <- NA
    } else {
      antig[[i]] <- features[[i]][grep(" años", 
                                       features[[i]])] %>% 
        str_remove_all(" años") %>% 
        str_replace_all('\\,', '\\.') %>% # Substituir , por .
        as.numeric()
    }
    if(length(features[[i]][grep("baño", 
                                 features[[i]])])==0){
      banios[[i]] <- NA
    } else {
      banios[[i]] <- features[[i]][grep("baño", 
                                        features[[i]])] %>% 
        str_remove_all(" baño") %>% 
        str_remove_all("s") %>% 
        as.numeric()
    }
  }
  
  features <- as.data.frame(cbind(do.call(rbind, metros), do.call(rbind, dorm),
                                  do.call(rbind, antig), do.call(rbind, banios))) 
  
  colnames(features) <- c("area", "dorm", "antig", "banios")
  
  deptos[[pagina]] <- cbind(direccion, precios, barrio, features)
  
#  final <- rbind(deptos, final)
  
  print(pagina/1000)

}

deptos <- bind_rows(deptos)

rm(list="barrio")
final <- deptos

deptos %>%  
  filter(barrio %in% c("Belgrano", "Liniers", "Recoleta"), 
         precios < 500000) %>% 
  ggplot()+
  geom_density(aes(precios, fill = barrio), alpha = .5)+
  labs(fill = "Barrio", title = "Densidad de precios por barrio.",
       subtitle = "Ciudad de Buenos Aires. Febrero de 2022", 
       caption = "Fuente: Elaboración propia en base a Argenprop",
       x = "Precios", y = "Densidad")


unique(final$barrio)

library(sf)

barrios <- sf::st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

plot(barrios)

colnames(barrios)[2] <- "barrio"

library(stringi)

barrios[,2] %>% 
  mutate(barrio = stri_trans_general(tolower(barrio), "Latin-ASCII")) %>% 
  left_join(final %>% mutate(barrio = tolower(barrio)) %>% 
              group_by(barrio) %>% summarize(precio = mean(precios/area, na.rm = TRUE)) %>% 
             mutate(barrio = stri_trans_general(barrio,"Latin-ASCII"))) %>%
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill=precio), color = NA) + 
  scale_fill_distiller(palette = 8, na.value = "#FFEFD6", direction = 1)+
  theme_void()+
  labs(fill = "m² en USD", title = "Valor promedio del metro cuadrado por barrio",
       subtitle = "Ciudad de Buenos Aires. Febrero de 2022", 
       caption = "Fuente: Elaboración propia en base a Argenprop")
  

barrios[,2] %>% 
  mutate(barrio = stri_trans_general(tolower(barrio), "Latin-ASCII")) %>% 
  left_join(final %>% mutate(barrio = tolower(barrio)) %>% 
              group_by(barrio) %>% summarize(precio = mean(dorm, na.rm = TRUE)) %>% 
              mutate(barrio = stri_trans_general(barrio,"Latin-ASCII"))) %>%
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill=precio), color = NA) + 
  scale_fill_distiller(palette = 12, na.value = "#C2C2E2", direction = 1)+
  theme_void()+
  labs(fill = "Habitaciones", title = "Cantidad promedio de habitaciones de departamentos en venta",
       subtitle = "Ciudad de Buenos Aires. Febrero de 2022", 
       caption = "Fuente: Elaboración propia en base a Argenprop")

barrios[,2] %>% 
  mutate(barrio = stri_trans_general(tolower(barrio), "Latin-ASCII")) %>% 
  left_join(final %>% mutate(barrio = tolower(barrio)) %>% 
              group_by(barrio) %>% summarize(precio = mean(antig, na.rm = TRUE)) %>% 
              mutate(barrio = stri_trans_general(barrio,"Latin-ASCII"))) %>%
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill=precio), color = NA) + 
  scale_fill_distiller(palette = 3, na.value = "#C2C2E2", direction = 1)+
  theme_void()+
  labs(fill = "Años", title = "Antigüedad promedio de departamentos en venta",
       subtitle = "Ciudad de Buenos Aires. Febrero de 2022", 
       caption = "Fuente: Elaboración propia en base a Argenprop")

