library(tidyverse)

prof.all = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))


pdf("./figures/heatmaps/profile heatmaps all variables 2022 and 2024 2025-01-08.pdf", width = 9, height = 5, onefile = TRUE)

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "temp"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "temp", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "phyco_cells"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "phycocyanin", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "log10(phyco_cells)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "log10(phyco)", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))


ggplot(prof.all %>% filter(depth <= 8 & doy), aes_string("as.factor(doy)", "depth", fill = "chl_ugL"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "chl_ugL", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "log10(chl_ugL)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "log(chl)", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))


ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "pH"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "pH", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))


ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "do_percent"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "DO %", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "SPC"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year)+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "SPC", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))



dev.off()

# interpolate over depth


# save the dataset




#### LTER Symposium plots ###

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "(phyco_cells)"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(lake~year,
             labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                 year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "log10(phyco)", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(162, 172, 180, 190, 199, 208, 218, 227)))



png("./figures/NTL LTER symposium figures/do percent heatmap.png", height = 11, width = 8, units = "in", res = 300)

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "do_percent"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_bw()+
  scale_y_reverse()+
  facet_grid(lake~year,
             labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                 year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "DO %", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(160, 175, 190, 205, 220, 235)))+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.title = element_text(size = 20), strip.text = element_text(size = 20), legend.position = "top",
        legend.text = element_text(size = 14))

dev.off()




png("./figures/NTL LTER symposium figures/phycocyanin heatmap.png", height = 11, width = 8, units = "in", res = 300)

ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "log10(phyco_cells + min(phyco_cells))"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_bw()+
  scale_y_reverse()+
  facet_grid(lake~year,
             labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                 year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
  labs(x = "day of year", y = "depth (m)")+
  #scale_fill_distiller(palette = "BrBG")
  scale_fill_gradientn(name = "Phycocyanin", colors = hcl.colors(20, "Spectral"), trans = "reverse")+
  scale_x_discrete(breaks =as.character(c(160, 175, 190, 205, 220, 235)))+
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.title = element_text(size = 20), strip.text = element_text(size = 20), legend.position = "top",
        legend.text = element_text(size = 10))

dev.off()



colors = hcl.pals()

pdf("./figures/NTL LTER symposium figures/phyco all colors log.pdf", height = 11, width = 8.5)

for(i in 1:length(colors)){
  cur.color = colors[i]
  
  
  print(ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "log10(phyco_cells + min(phyco_cells))"))+
          geom_tile()+
          #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
          theme_bw()+
          scale_y_reverse()+
          facet_grid(lake~year,
                     labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                         year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
          labs(x = "day of year", y = "depth (m)", title = i)+
          #scale_fill_distiller(palette = "BrBG")
          scale_fill_gradientn(name = "Phycocyanin", colors = hcl.colors(20, cur.color), trans = "reverse")+
          scale_x_discrete(breaks =as.character(c(160, 175, 190, 205, 220, 235)))+
          theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
                legend.title = element_text(size = 20), strip.text = element_text(size = 20), legend.position = "top",
                legend.text = element_text(size = 10)))
  
  
  print(ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "log10(phyco_cells + min(phyco_cells))"))+
          geom_tile()+
          #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
          theme_bw()+
          scale_y_reverse()+
          facet_grid(lake~year,
                     labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                         year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
          labs(x = "day of year", y = "depth (m)", title = paste(i, "trans"))+
          #scale_fill_distiller(palette = "BrBG")
          scale_fill_gradientn(name = "Phycocyanin", colors = hcl.colors(20, cur.color))+
          scale_x_discrete(breaks =as.character(c(160, 175, 190, 205, 220, 235)))+
          theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
                legend.title = element_text(size = 20), strip.text = element_text(size = 20), legend.position = "top",
                legend.text = element_text(size = 10)))
  
  
}

dev.off()






pdf("./figures/NTL LTER symposium figures/phyco all colors no log.pdf", height = 11, width = 8.5)

for(i in 1:length(colors)){
  cur.color = colors[i]
  
  
  print(ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "(phyco_cells + min(phyco_cells))"))+
          geom_tile()+
          #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
          theme_bw()+
          scale_y_reverse()+
          facet_grid(lake~year,
                     labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                         year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
          labs(x = "day of year", y = "depth (m)", title = i)+
          #scale_fill_distiller(palette = "BrBG")
          scale_fill_gradientn(name = "Phycocyanin", colors = hcl.colors(20, cur.color), trans = "reverse")+
          scale_x_discrete(breaks =as.character(c(160, 175, 190, 205, 220, 235)))+
          theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
                legend.title = element_text(size = 20), strip.text = element_text(size = 20), legend.position = "top",
                legend.text = element_text(size = 10)))
  
  
  print(ggplot(prof.all %>% filter(depth <= 8), aes_string("as.factor(doy)", "depth", fill = "(phyco_cells + min(phyco_cells))"))+
          geom_tile()+
          #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
          theme_bw()+
          scale_y_reverse()+
          facet_grid(lake~year,
                     labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                         year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
          labs(x = "day of year", y = "depth (m)", title = paste(i, "trans"))+
          #scale_fill_distiller(palette = "BrBG")
          scale_fill_gradientn(name = "Phycocyanin", colors = hcl.colors(20, cur.color))+
          scale_x_discrete(breaks =as.character(c(160, 175, 190, 205, 220, 235)))+
          theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
                legend.title = element_text(size = 20), strip.text = element_text(size = 20), legend.position = "top",
                legend.text = element_text(size = 10)))
  
  
}

dev.off()

