#install.packages("eurostat")

library(eurostat)
library(dplyr)
library(stringr) #do stri count
library(ggplot2)
library(viridis)
library(scales)
library(extrafont) #do czcionek
#font_import()
loadfonts(device="win")


c("Miasto Wroclaw"     = "black", 
  "Miasto Poznan"      = "#231151CC", 
  "Miasto KrakÛw"      = "#5F187FCC", 
  "Region Trojmiejski" = "#982D80CC", 
  "Region Katowicki"   = "#D3436ECC", 
  "Miasto LÛdz"        = "#F8765CCC",
  "Miasto Warszawa"    = "#FEBA80CC")

setwd("D:\\KK\\OneDrive\\Wroclaw w Liczbach\\Gotowe projekty\\Centralizacja\\ile miast w top miescie")


#do≥acznei opisÛw  NUTS 3 
opis <- read.csv2("opis.csv", stringsAsFactors = F) %>%
  select(GEO.TIME) %>%
  cbind(. , str_split_fixed(. $GEO.TIME, " - ", 2)) %>%
  mutate("1" = as.character(.$"1"),
         "2" = as.character(.$"2"),
         kraj = substr(GEO.TIME, 1, 2))

names(opis) <- c("GEO", "ID", "Region", "Kraj") 

opis <- opis %>%
  mutate(dl = nchar(ID))
#%>%
#  filter(dl == 5)




#sprawdzenie jakie metryki sπ z PKB
#query <- search_eurostat("GDP", type = "table")

PKB <- get_eurostat(id="nama_10r_3gdp", time_format="num")
names(PKB)[4] <- "PKB"
PKB_NUTS3 <- PKB[PKB$unit == "MIO_EUR", ] %>% 
  mutate(geo = as.character(geo)) %>%
  filter(nchar(geo) == 5) %>%
  merge(opis, by.x = "geo", by.y = "ID", all.x = T) #%>%
  #mutate(NUTS.3.2010.code.and.name = ifelse(is.na(NUTS.3.2010.code.and.name), "NA - NA", NUTS.3.2010.code.and.name))


PKB_NUTS3_bledy <- PKB[PKB$unit == "MIO_EUR", ] %>% 
  mutate(geo = as.character(geo)) %>%
  filter(nchar(geo) == 5) %>%
  merge(opis, by.x = "geo", by.y = "ID", all.x = T) %>%
  filter(is.na(Region))
 
#sprawdzenie statytyk
min(PKB$time) #zakres lat 
max(PKB$time) #zakres lat 

write.csv2(PKB_NUTS3, file = "1. tabela poczatkowa.csv")



END <- data.frame()
lata  <- unique(PKB_NUTS3$time)
i=1
j=2
for (i in 1:length(lata)){
  PKB_NUTS3_rok <- PKB_NUTS3 %>% 
    filter(time == lata [i]) 
  
  kraje <- unique(PKB_NUTS3_rok$Kraj)
  
  for (j in 1:length(kraje)){
    
    tmp <- PKB_NUTS3 %>%  
      filter(Kraj == kraje[j]) %>% #ile dla PL
      filter(time == lata[i])    %>% #ostatni rok 
      arrange(desc(PKB))      %>% 
      mutate(max = max(PKB),
             suma = cumsum(PKB) - max,
             prc =  suma/max,
             brak = lag(max - suma),
             czesc_miasta = brak / PKB,
             nr = row_number()-1) %>%
      filter(nr != 0 ) %>%
      filter(brak > 0 ) %>%
      mutate(ile_miast = ifelse(czesc_miasta > 1 , 1, czesc_miasta),
             Wynik = cumsum(ile_miast) ) %>%
      group_by(Kraj, time) %>%
      summarise(Regiony = paste(Region, collapse = ", "),
                Wynik = max(Wynik)) %>%
      data.frame()
    
    END <- rbind(END, tmp)
    
  }
  
}
 
write.csv2(END, file = "2. tabela koÒcowa.csv")


rok <- 2000

for (i in 1:(length(lata) - 1)){
  
  rok = lata[i]
  
  Rok <- END %>% 
    filter(time == rok) %>%
    filter(Kraj != "MT") %>%
    arrange(Wynik) %>%
    mutate(PL = ifelse(Kraj == "PL", "PL", "NPL"))
   
  srednia <- mean(Rok$Wynik)
  
  
  png(filename = paste("wykresy\\gif\\", Sys.Date()," ", rok , ".png", sep=""),
      bg="white", width = 10, height = 8, units = 'in', res = 100)
      plot(
        ggplot(data=Rok, aes(x= reorder(Kraj, Wynik), y=Wynik, fill=PL)) +
          geom_bar(stat="identity") +
          labs(title = paste0("Ile kolejnych regionÛw tworzy PKB rÛwne miastu o najwiÍkszym PKB"),
               subtitle = paste0("Rok: ", rok),
               x = "Kraj",
               y = "Liczba regionÛw",
               caption = "Autor: WroData | èrÛd≥o: Eurostat" ) +
          scale_fill_manual(
            values = c("grey", "red"),
            breaks = c("PL",   "NPL"),
            labels = c("Polska", "Inne kraje")) + 
          coord_cartesian(ylim = c(0,10)) + 
          geom_hline(yintercept = srednia) + 
          geom_text(x=1, y=srednia + 0.3, hjust=0, 
                    label=paste0("årednia europejska: ", round(srednia, 2)),
                    family = "Ubuntu", size = 6) +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.title.x = element_blank(),
                axis.text    = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
                axis.title   = element_text(family = "Ubuntu", size = 15, color = "#22211d"),
                text = element_text(family = "Ubuntu", color = "#22211d"),
                panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
                legend.position   = "bottom",
                legend.title      = element_blank(),
                plot.title    = element_text(family = "Ubuntu", size = 20,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 15,  hjust = 0.99,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 12,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
                )  
      )
  dev.off()

}






#≥aczenie w gifa
library(magick)
library(purrr)
list.files(path = ".\\wykresy\\gif\\", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=0.5) %>% # animates, can opt for number of loops
  image_write("W czasie.gif") # write to current dir


#########################################################################
#wykres 2
Kolejnosc <- data.frame(Region= c("Miasto Wroclaw", 
                                  "Miasto Poznan" , 
                                  "Miasto KrakÛw"  , 
                                  "Region Trojmiejski" , 
                                  "Region Katowicki"  , 
                                  "Miasto LÛdz" ,
                                  "Miasto Warszawa"),
                        Kolejnosc = 1:7)
                                   
DanePL <- PKB_NUTS3 %>%
  filter(Kraj == "PL") %>%
  arrange(time, desc(PKB)) %>%
  group_by(time) %>%
  mutate(PKB_WAWY = max(PKB),
         PRC_PKB_WAWY = PKB / PKB_WAWY,
         t = 1,
         t = cumsum(t)) %>% ungroup()

wybrane <- DanePL %>% 
  filter(t>1,
         t<7) %>%
  arrange( desc(PRC_PKB_WAWY))  %>%
  select(Region) %>% distinct() %>%
  mutate(wybrane = 1)

DanePL_wybrane <- DanePL %>% 
  merge(wybrane, by = "Region") %>%
  mutate(WRO = ifelse(Region == "Miasto Wroclaw", "dashed", "solid")) %>%
  merge(Kolejnosc, by = "Region") %>%
  arrange(Kolejnosc)

legenda <- wybrane$Region 
legenda <- ifelse(substr(legenda, 1, 5) == "Miast", legenda, paste("Region", legenda))

png(filename = paste("wykresy\\", Sys.Date()," ", "PKB regionu w stosunku do PKB Warszawy" , ".png", sep=""),
    bg="white", width = 10, height = 8, units = 'in', res = 150)
  plot(ggplot(data=DanePL_wybrane, aes(x=time, y=PRC_PKB_WAWY, colour=reorder(Region, Kolejnosc))) +
          geom_line(data=DanePL_wybrane, aes(linetype = WRO), size = 0.8)  +
          geom_point() + 
          labs(title = "PKB regionu w stosunku do PKB Warszawy",
               x = "Rok",
               y = "Procent PKB Warszawy",
               caption = "Autor: WroData | èrÛd≥o: Eurostat" ) +
         scale_colour_manual(
           values = c("Miasto Wroclaw"     = "#800000", 
                      "Miasto Poznan"      = "#ffa400", 
                      "Miasto KrakÛw"      = "#3f1f09", 
                      "Region Trojmiejski" = "#ad7d5a", 
                      "Region Katowicki"   = "#00cc66", 
                      "Miasto LÛdz"        = "#808080",
                      "Miasto Warszawa"    = "black"),
           #labels = Kolejnosc$Region,
           name = " ",
           drop = FALSE, 
           guide = guide_legend(
             direction = "horizontal"))   + 
         scale_x_continuous(breaks = 2000:2014) +
         scale_y_continuous(labels = scales::percent) +
         scale_size(guide = 'none') +
         scale_linetype(guide = 'none') +
         theme(panel.grid.minor.y = element_blank(),
               panel.grid.minor.x = element_blank(),
               axis.title.x = element_blank(),
               axis.text    = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
               axis.title   = element_text(family = "Ubuntu", size = 15, color = "#22211d"),
               text = element_text(family = "Ubuntu", color = "#22211d"),
               panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
               plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
               panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
               legend.background = element_rect(fill = "#f5f5f2", color = NA),
               legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
               legend.position   = "bottom",
               legend.title      = element_blank(),
               plot.title    = element_text(family = "Ubuntu", size = 20,  hjust = 0.5,  color = "#4e4d47"),
               plot.subtitle = element_text(family = "Ubuntu", size = 15,  hjust = 0.8,  face = "italic", color = "#4e4d47"),
               plot.caption  = element_text(family = "Ubuntu", size = 12,  hjust = 0.99, color = "#4e4d47"),
               panel.border = element_blank()
         )  
  ) 
dev.off()


############################################################################
#wzkres indeks
DanePL_indeks <- PKB_NUTS3 %>%
  filter(Kraj == "PL") %>%
  filter(time == 2000) %>%
  select(Region, PKB)
names(DanePL_indeks) <- c("Region", "PKB2000")

DanePL_indeks <- PKB_NUTS3 %>%
  merge (DanePL_indeks)     %>%
  mutate(indeks = PKB / PKB2000) 

wybrane_indeks <- rbind(wybrane, data.frame(Region = "Miasto Warszawa", wybrane = 1))


DanePL_indeks <- DanePL_indeks %>% 
  merge(wybrane_indeks, by = "Region") %>%
  mutate(WRO = ifelse(Region == "Miasto Wroclaw", "dashed", "solid"))  %>%
  merge(Kolejnosc, by = "Region") %>%
  arrange(Kolejnosc)


wybrane_indeks <- DanePL_indeks %>% select (Region) %>% distinct()
wybrane_indeks <- wybrane_indeks[c(6, 2, 3, 4, 7, 1, 5), ]

legenda_indeks <- wybrane_indeks
legenda_indeks <- ifelse(substr(legenda_indeks, 1, 5) == "Miast", legenda_indeks, paste("Region", legenda_indeks))




png(filename = paste("wykresy\\", Sys.Date()," ", "PKB regionu w stosunku do PKB regionu z roku 2000" , ".png", sep=""),
    bg="white", width = 10, height = 8, units = 'in', res = 150)
    plot(
      ggplot(data=DanePL_indeks, aes(x=time, y=indeks, colour=reorder(Region, Kolejnosc))) +
        geom_line(data=DanePL_indeks, aes(linetype = WRO), size = 0.8)  +
        geom_point()  + 
        labs(title = "PKB w stosunku do PKB z 2000 roku dla poszczegÛlnych regionÛw",
             x = "Rok",
             y = "Procent PKB regionu z 2000 roku",
             caption = "Autor: WroData | èrÛd≥o: Eurostat" ) +
        scale_colour_manual(
          values = c("Miasto Wroclaw"     = "#800000", 
                     "Miasto Poznan"      = "#ffa400", 
                     "Miasto KrakÛw"      = "#3f1f09", 
                     "Region Trojmiejski" = "#ad7d5a", 
                     "Region Katowicki"   = "#00cc66", 
                     "Miasto LÛdz"        = "#808080",
                     "Miasto Warszawa"    = "black"),
          #labels = Kolejnosc$Region,
          name = " ",
          drop = FALSE, 
          guide = guide_legend(
            direction = "horizontal"))   + 
        scale_x_continuous(breaks = 2000:2014) +
        scale_y_continuous(labels = scales::percent) +
        scale_size(guide = 'none') +
        scale_linetype(guide = 'none') +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text    = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
              axis.title   = element_text(family = "Ubuntu", size = 15, color = "#22211d"),
              text = element_text(family = "Ubuntu", color = "#22211d"),
              panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
              plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
              panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
              legend.background = element_rect(fill = "#f5f5f2", color = NA),
              legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
              legend.position   = "bottom",
              legend.title      = element_blank(),
              plot.title    = element_text(family = "Ubuntu", size = 20,  hjust = 0.5,  color = "#4e4d47"),
              plot.subtitle = element_text(family = "Ubuntu", size = 15,  hjust = 0.8,  face = "italic", color = "#4e4d47"),
              plot.caption  = element_text(family = "Ubuntu", size = 12,  hjust = 0.99, color = "#4e4d47"),
              panel.border = element_blank()
        ) 
      ) 
dev.off()     



############################################################################
#wzkres % pkb pc Warszawy

PKB_pc <- get_eurostat(id="nama_10r_3gdp", time_format="num")
names(PKB)[4] <- "PKBPC"
PKB_pc_NUTS3 <- PKB[PKB$unit == "EUR_HAB", ] %>% 
  mutate(geo = as.character(geo)) %>%
  filter(nchar(geo) == 5) %>%
  merge(opis, by.x = "geo", by.y = "ID", all.x = T)  %>%
  filter(Kraj == "PL")

PKB_pc_Wawa <- PKB_pc_NUTS3 %>% filter(Region == "Miasto Warszawa") %>%
  mutate(PKBPC_WAWA = PKBPC) %>%
  select(time, PKBPC_WAWA) 

PKB_pc_NUTS3_W <- PKB_pc_NUTS3 %>%
  merge(PKB_pc_Wawa, by = "time") %>%
  merge(wybrane, by = "Region")  %>%
  mutate(PKB_PC_indeks = PKBPC/ PKBPC_WAWA) %>%
  arrange(time, desc(PKB_PC_indeks)) %>%
  mutate(WRO = ifelse(Region == "Miasto Wroclaw", "dashed", "solid"))  %>%
  merge(Kolejnosc, by = "Region") %>%
  arrange(Kolejnosc)
  
PKB_pc_NUTS3_W %>% select(Region) %>% distinct()


  
magma(8, alpha = 0.8)[2:7] 


png(filename = paste("wykresy\\", Sys.Date()," ", "PKB per capita w stosunku do PKB per capita Warszawy 2 " , ".png", sep=""),
    bg="white", width = 10, height = 8, units = 'in', res = 150)
    plot(
        ggplot(data=PKB_pc_NUTS3_W, aes(x=time, y=PKB_PC_indeks, colour=reorder(Region, Kolejnosc) )) +
          geom_line(data=PKB_pc_NUTS3_W, aes(linetype = WRO), size = 0.8)  +
          geom_point()  + 
          labs(title = "PKB per capita w stosunku do PKB per capita Warszawy",
               x = "Rok",
               y = "Procent PKB per capita Warszawy",
               caption = "Autor: WroData | èrÛd≥o: Eurostat" ) +
          scale_colour_manual(
            values = c("Miasto Wroclaw"     = "#800000", 
                       "Miasto Poznan"      = "#ffa400", 
                       "Miasto KrakÛw"      = "#3f1f09", 
                       "Region Trojmiejski" = "#ad7d5a", 
                       "Region Katowicki"   = "#00cc66", 
                       "Miasto LÛdz"        = "#808080",
                       "Miasto Warszawa"    = "black"), 
            #labels = Kolejnosc$Region,
            name = " ",
            drop = FALSE, 
            guide = guide_legend(
              direction = "horizontal"))   + 
          scale_x_continuous(breaks = 2000:2014) +
          scale_y_continuous(labels = scales::percent) +
          scale_size(guide = 'none') +
          scale_linetype(guide = 'none') +
          theme(panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.title.x = element_blank(),
                axis.text    = element_text(family = "Ubuntu", size = 13, color = "#22211d"),
                axis.title   = element_text(family = "Ubuntu", size = 15, color = "#22211d"),
                text = element_text(family = "Ubuntu", color = "#22211d"),
                panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                plot.background  = element_rect(fill = "#f5f5f2",  color = NA), 
                panel.background = element_rect(fill = "#f5f5f2",  color = NA), 
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.text       = element_text(family = "Ubuntu", size = 13, hjust = 0, color = "#22211d"),
                legend.position   = "bottom",
                legend.title      = element_blank(),
                plot.title    = element_text(family = "Ubuntu", size = 20,  hjust = 0.5,  color = "#4e4d47"),
                plot.subtitle = element_text(family = "Ubuntu", size = 15,  hjust = 0.8,  face = "italic", color = "#4e4d47"),
                plot.caption  = element_text(family = "Ubuntu", size = 12,  hjust = 0.99, color = "#4e4d47"),
                panel.border = element_blank()
                ) 
    ) 
dev.off()   