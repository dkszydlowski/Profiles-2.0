## create a normalized map of pigments with chlorophyll dominance shown as green 
# and phycocyanin dominance shown as blue

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(ggnewscale)) install.packages('ggnewscale')
library(ggnewscale)

if (!require(rLakeAnalyzer)) install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)

# read in the profiles data
profiles = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))

# normalize pigments
profiles = profiles %>% select(lake, doy, depth, year, chl_ugL, phyco_cells)

prof.norm = profiles %>% 
  group_by(lake, year) %>% 
  mutate(chl_ugL = log10(chl_ugL), phyco_cells = log10(phyco_cells)) %>% 
  mutate(chl_ugL = replace(chl_ugL, is.infinite(chl_ugL), NA),
         phyco_cells = replace(phyco_cells, is.infinite(phyco_cells), NA)) %>% 
  mutate(chl_ugL = (chl_ugL - mean(chl_ugL, na.rm = TRUE))/sd(chl_ugL, na.rm = TRUE),
         phyco_cells = (phyco_cells - mean(phyco_cells, na.rm = TRUE))/sd(phyco_cells, na.rm = TRUE))

# create column indicating which is greater
prof.norm = prof.norm %>% mutate(pigment = case_when(chl_ugL > phyco_cells ~ "chlorophyll",
                                                     phyco_cells >= chl_ugL ~ "phycocyanin"))


ggplot(prof.norm, aes_string("doy", "depth", fill = "pigment"))+
  geom_tile()+
  #labs(title = if(lake_picked == "T") {"Tuesday"}else if(lake_picked == "L"){"Paul"} else if (lake_picked == "R"){"Peter"})+
  theme_classic()+
  scale_y_reverse()+
  facet_grid(year~lake)
  #scale_fill_distiller(palette = "BrBG")
  #scale_fill_gradientn(colors = hcl.colors(20, "Spectral"), trans = "reverse"))




# Define color scales
phycocyanin_colors <- colorRampPalette(c("blue", "cyan"))(100)
chlorophyll_colors <- colorRampPalette(c("green", "yellowgreen"))(100)

# Prepare data with dominant pigment and corresponding value
prof.norm <- prof.norm %>%
  mutate(
    pigment = ifelse(phyco_cells > chl_ugL, "phycocyanin", "chlorophyll"),
    dominant_value = ifelse(phyco_cells > chl_ugL, phyco_cells, chl_ugL)
  )

# Heatmap with custom color scale
ggplot(prof.norm, aes(x = doy, y = depth, fill = (dominant_value))) +
  geom_tile() +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  theme_classic() +
  scale_fill_gradientn(
    colors = c(phycocyanin_colors, chlorophyll_colors),
    na.value = "grey90",
    name = "Pigment Intensity"
  ) +
  labs(
    title = "Pigment Dominance Heatmap",
    x = "Day of Year (DOY)",
    y = "Depth (m)"
  )




ggplot(prof.norm, aes(x = doy, y = depth)) +
  geom_tile(aes(fill = dominant_value)) +
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  theme_classic() +
  scale_fill_gradientn(
    name = "Pigment",
    colors = c("blue", "cyan", "green", "yellowgreen"),
    values = c(0, 0.5, 0.5, 1), # Breakpoint at 0.5 for the two ramps
    breaks = c(0, 0.5, 1),
    labels = c("Chl (low)", "Neutral", "Blue!"))


library(ggplot2)
library(dplyr)
library(ggnewscale)  # To reset fill scales

# Normalize the values for consistency
prof.norm <- prof.norm %>%
  mutate(
    phyco_norm = (phyco_cells - min(phyco_cells, na.rm = TRUE)) / (max(phyco_cells, na.rm = TRUE) - min(phyco_cells, na.rm = TRUE)),
    chl_norm = (chl_ugL - min(chl_ugL, na.rm = TRUE)) / (max(chl_ugL, na.rm = TRUE) - min(chl_ugL, na.rm = TRUE)),
    pigment = ifelse(phyco_cells > chl_ugL | is.na(chl_norm), "phycocyanin", "chlorophyll"),
    pigment = replace(pigment, is.na(pigment), "chlorophyll"),
    fill_value = ifelse(pigment == "phycocyanin", phyco_norm, chl_norm)
  )

# Plot with separate color gradients
ggplot() +
  # Layer 1: Phycocyanin with blue-cyan gradient
  geom_tile(
    data = filter(prof.norm, pigment == "phycocyanin"),
    aes(x = doy, y = depth, fill = fill_value)
  ) +
  scale_fill_gradientn(
    colors = c("blue", "cyan"),
    name = "Phycocyanin"
  ) +
  new_scale_fill() +  # Reset the fill scale
  
  # Layer 2: Chlorophyll with green-yellowgreen gradient
  geom_tile(
    data = filter(prof.norm, pigment == "chlorophyll"),
    aes(x = doy, y = depth, fill = fill_value)
  ) +
  scale_fill_gradientn(
    colors = c("green", "yellowgreen"),
    name = "Chlorophyll"
  ) +
  
  # Formatting and labels
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  theme_classic() +
  labs(
    title = "Pigment Dominance Heatmap",
    x = "Day of Year (DOY)",
    y = "Depth (m)"
  )





ggplot() +
  # Layer 1: Phycocyanin with blue-cyan gradient
  geom_tile(
    data = filter(prof.norm, pigment == "phycocyanin"),
    aes(x = doy, y = depth, fill = fill_value)
  ) +
  scale_fill_gradientn(
    colors = c("#070D17", "cyan"),
    limits = c(0, 1),  # Ensure full range of normalization
    name = "Phycocyanin",
    na.value = "#070D17"
  ) +
  new_scale_fill() +  # Reset the fill scale
  
  # Layer 2: Chlorophyll with green-yellowgreen gradient
  geom_tile(
    data = filter(prof.norm, pigment == "chlorophyll"),
    aes(x = doy, y = depth, fill = fill_value)
  ) +
  scale_fill_gradientn(
    colors = c("#011800", "chartreuse"),
    limits = c(0, 1),  # Ensure full range of normalization
    name = "Chlorophyll",
    na.value = "#011800"
  ) +
  
  # Formatting and labels
  scale_y_reverse() +
  facet_grid(year ~ lake) +
  theme_classic() +
  labs(
    title = "Pigment Dominance Heatmap",
    x = "Day of Year (DOY)",
    y = "Depth (m)"
  )








### interpolated version ###
# prof.norm.interp = prof.norm %>% ungroup() %>% 
#   mutate(phyco_norm = replace(phyco_norm, is.na(phyco_norm), 0),
#           chl_norm = replace(chl_norm, is.na(chl_norm), 0))
# 
# prof.norm.interp <- prof.norm.interp %>%
#   mutate(
#     pigment = ifelse(phyco_norm > chl_norm | is.na(chl_norm), "phycocyanin", "chlorophyll"),
#     fill_value = ifelse(pigment == "phycocyanin", phyco_norm, chl_norm)
#   )




##### COMBINED HEATMAP RED + BLUE MAKE PURPLE ######

# read in the profiles data
profiles = get(load("./data/formatted data/cleaned profiles interpolated over depth and time.RData"))

# normalize pigments
profiles = profiles %>% select(lake, doy, depth, year, chl_ugL, phyco_cells)


prof.norm <- profiles %>%
  group_by(lake, year) %>%
  mutate(
    chl_ugL = log10(chl_ugL),
    phyco_cells = log10(phyco_cells)) %>%
  mutate(chl_ugL = replace(chl_ugL, is.infinite(chl_ugL), NA), 
         phyco_cells = replace(phyco_cells, is.infinite(phyco_cells), NA)) %>%
  mutate(chl_ugL = (chl_ugL - min(chl_ugL, na.rm = TRUE)) / (max(chl_ugL, na.rm = TRUE) - min(chl_ugL, na.rm = TRUE)),
    phyco_cells = (phyco_cells - min(phyco_cells, na.rm = TRUE)) / (max(phyco_cells, na.rm = TRUE) - min(phyco_cells, na.rm = TRUE)))


prof.norm = prof.norm %>% mutate(phyco.transparency = (phyco_cells - min(phyco_cells, na.rm = TRUE)) / (max(phyco_cells, na.rm = TRUE) - min(phyco_cells, na.rm = TRUE)) )
prof.norm = prof.norm %>% mutate(chl.transparency = (chl_ugL - min(chl_ugL, na.rm = TRUE)) / (max(chl_ugL, na.rm = TRUE) - min(chl_ugL, na.rm = TRUE)) )


ggplot(prof.norm, aes(x = doy, y = depth)) +
  # First layer: Chlorophyll
  geom_tile(aes(fill = "red", alpha = chl.transparency)) +
  scale_y_reverse() +
  # Add facets
  facet_wrap(year~lake) +
  # Separate scales for Phycocyanin
  new_scale_fill() +
  geom_tile(aes(fill = "blue", alpha = phyco.transparency))


  
  
  ggplot(prof.norm, aes(x = doy, y = depth)) +
    # First layer: Chlorophyll
    geom_tile(aes(fill = chl.transparency, alpha = chl.transparency)) +
    scale_y_reverse() +
    scale_fill_gradient(low = "white", high = "blue", name = "Chlorophyll") +
    # Add facets
    facet_wrap(year ~ lake) +
    # Separate scales for Phycocyanin
    new_scale_fill() +
    geom_tile(aes(fill = phyco.transparency, alpha = phyco.transparency)) +
    scale_fill_gradient(low = "white", high = "red", name = "Phycocyanin") +
    # Ensure alpha scales work across layers
    scale_alpha(range = c(0, 0.8), guide = "none") +
    theme_classic()
  
  ### problem is that I need to scale phyco clls so that the fill is between 0 and 1

  
  ggplot(prof.norm, aes(x = doy, y = depth)) +
    # First layer: Chlorophyll
    geom_tile(aes(fill = chl_ugL)) +
    scale_y_reverse() +
    scale_fill_gradientn(
      colors = c("#FF7D7D", "#640000"),
      name = "Chlorophyll"
    ) +
    # Add facets
    facet_wrap(year~lake) +
    # Separate scales for Phycocyanin
    new_scale_fill() +
    geom_tile(aes(fill = phyco_cells, alpha = phyco_cells, fill = "#002E50"))
  theme_classic()
  
  
  
  
  ggplot(prof.norm, aes(x = doy, y = depth)) +
    # First layer: Chlorophyll
    geom_tile(aes(fill = chl_ugL)) +
    scale_y_reverse() +
    scale_fill_gradientn(
      colors = c("#FF7D7D", "#640000"),
      name = "Chlorophyll"
    ) +
    # Add facets
    facet_wrap(year ~ lake) +
    # Separate scales for Phycocyanin
    new_scale_fill() +
    geom_tile(aes(alpha = phyco_cells, fill = phyco_cells)) +
    scale_alpha_continuous(range = c(0, 0.8)) +  # Scale alpha so max is 0.8
    theme_classic()



ggplot(prof.norm, aes(x = doy, y = depth)) +
  # First layer: Chlorophyll with custom breaks
  geom_tile(aes(fill = chl_ugL)) +
  scale_y_reverse() +
  scale_fill_gradientn(
    colors = c("white", "white", "red4"),
    values = c(0, 0.2, 1),  # Define gradient breaks
    name = "Chlorophyll"
  ) +
  # Add facets
  facet_wrap(year~lake) +
  # Separate scale for Phycocyanin
  new_scale_fill() +
  geom_tile(aes(fill = phyco_cells), alpha = 0.6) +
  scale_fill_gradientn(
    colors = c("white", "white", "blue4"),
    values = c(0, 0.2, 1),  # Define gradient breaks
    name = "Phycocyanin"
  )+
  theme_classic()



ggplot(prof.norm, aes(x = doy, y = depth)) +
  # First layer: Chlorophyll with red scale
  geom_tile(aes(fill = chl_ugL), alpha = 0.7) +
  scale_y_reverse() +
  scale_fill_gradientn(
    colors = c("white", "red4"),
    name = "Chlorophyll"
  ) +
  # Add facets
  facet_wrap(~lake) +
  # Separate scale for Phycocyanin
  new_scale_fill() +
  geom_tile(aes(fill = phyco_cells), alpha = 0.7) +
  scale_fill_gradientn(
    colors = c("white", "blue4"),
    name = "Phycocyanin"
  )+
  theme_classic()



### NTL LTER symposium figure ###

png("./figures/NTL LTER symposium figures/combined pigment heatmap.png", height = 11, width = 8, units = "in", res = 300)

ggplot(prof.norm, aes(x = doy, y = depth)) +
  theme_bw()+
  geom_tile(aes(fill = chl.transparency)) +
  scale_y_reverse() +
  scale_fill_gradient(low = "white", high = "blue", name = "Chlorophyll",   breaks = c(0, 0.5, 1)) +
  # Add facets
  facet_grid(lake~year,
             labeller = labeller(lake = c("L" = "Reference Lake", "R" = "Nutrients + Dye", "T" = "Nutrients"), 
                                 year = c("2022" = "Reference Year", "2024" = "Experimental Year")))+
  labs(y = "depth (m)", x = "day of year")+
  # Separate scales for Phycocyanin
  new_scale_fill() +
  geom_tile(aes(fill = phyco.transparency, alpha = phyco.transparency)) +
  scale_fill_gradient(low = "white", high = "red", name = "Phycocyanin",   breaks = c(0, 0.5, 1)) +
  # Ensure alpha scales work across layers and remove alpha legend if needed
  scale_alpha(range = c(0, 0.6), guide = "none") +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 24),
        legend.title = element_text(size = 14), strip.text = element_text(size = 20), legend.position = "top",
        legend.text = element_text(size = 10))

dev.off()



