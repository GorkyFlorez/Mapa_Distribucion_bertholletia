#------------------------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, rnaturalearth, rnaturalearthdata, sf, 
               raster, reticulate,  maptools,maps, ggpubr, gridExtra,
               ggplot2, ggspatial,rgeos, tmap,grid, rgbif, 
               rgrass7, sp, mapr, rgdal, RColorBrewer, cowplot)
#------------------------------------------------------------------------
Sur_America     <- st_read ("SHP/SurAmerica.shp")  
SurAmerica_utm  <- st_transform(Sur_America ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Per            <- getData('GADM', country='Peru', level=1) %>%st_as_sf()  
Peru            <- getData('GADM', country='Peru', level=3) %>%st_as_sf()  
MDD             <- subset(Peru , NAME_1 == "Madre de Dios")
Marco_MDD = st_as_sfc(st_bbox(MDD))

Berth_Exc    <- occ_search(scientificName="bertholletia excelsa")
Castaña      <- Berth_Exc$data
Castaña$image <- "PNG/Castaña1.png"

img <- readPNG("PNG/Castaña1.png", FALSE)
g <- rasterGrob(img, x = unit(0.1, "npc"),y = unit(0.7, "npc"), width = unit(0.2, "npc"))

Map=ggplot()+ 
  geom_sf(data= SurAmerica_utm, fill="gray90", color="white")+
  geom_sf(data=Per , fill="gray81", color="white")+
  geom_sf(data=MDD, fill="gray81", color="white")+
  geom_sf_text(data = st_as_sf(Per), aes(label =  NAME_1), size = 4, color="blue",family="serif") +
  geom_image(data = Castaña, aes( x=decimalLongitude, y = decimalLatitude, image = image), size = 0.04)+
  geom_sf_text(data = st_as_sf(MDD), aes(label =  NAME_3), size = 2.5,family="serif") +
  annotate(geom = "text", x = -69, y = -10.5, label = "Brasil", family="serif", color = "grey22", size = 5)+
  annotate(geom = "text", x = -68.7, y = -12, label = "Bolivia", family="serif", color = "grey22", size = 5)+
  annotate(geom = "text", x = -70, y = -13.5, label = "Puno", family="serif", color = "blue", size = 4)+
  annotate(geom = "text", x = -72.8, y = -11.9, label = "CASTAÑA \nBertholletia Excelsa ", family="serif", color = "lightblue4", size = 4)+
  coord_sf(xlim = c(-73,-68.65311), ylim = c(-13.36179,-9.879849))+
  annotation_custom(g)+
  theme_bw()+
  geom_vline(xintercept = c(-73,-72,-71,-70,-69), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(-13.5,-13,-12.5,-12,-11.5,-11,-10.5,-10), color = "gray50",linetype = "dashed", size = 0.05)+
  scale_x_continuous(breaks = seq(-73,-69, by = 1))+
  scale_y_continuous(breaks = seq(-13.5 ,-10, by = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        panel.border = element_rect(size = 2))+
  labs(x = NULL, y = NULL)+
  annotate(geom = "text", x = -70, y = -10, label = "MAPA de DISTRIBUCIÓN \nde", 
           family="serif", color = "indianred4", size = 3, fontface = "bold")+
  annotate(geom = "text", x = -70, y = -10.1, label = "BERTHOLLETIA EXCELSA en ", 
           fontface = "italic", family="serif",color = "indianred4", size =4, fontface = "bold")+
  annotate(geom = "text", x = -70, y = -10.2, label = "MADRE DE DIOS", 
           family="serif", color = "indianred4", size = 3, fontface = "bold")+
  annotate(geom = "text", x = -72.8, y = -13.3, label = "Ing.Gorky \nFlorez Castillo", 
           family="serif", color = "orangered4", size = 3)
  
ggsave(plot = Map ,"MAPAS/Mapa de Castaña1.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico


  
  
  
  