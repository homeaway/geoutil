HomeAwayYellow = "#FED863"
HomeAwayBlue = "#2A6EBB"
HomeAwayGreen = "#839E73"
WebRed = "#FF5A60"

pointsInLocality = function(world_geo, candidates_df){
  require(dplyr)
  require(data.table)
  
  meanLat = mean(candidates_df$lat)
  sdLat = sd(candidates_df$lat)
  meanLon = mean(candidates_df$lon)
  sdLon = sd(candidates_df$lon)
  
  # now go back and scoop up all the points from the world_geo. This will help to 
  # catch misspellings and the like.
  
  ha_df = world_geo %>% filter(datasource=="Internal") %>% 
    filter(lat >= meanLat - 3*sdLat) %>%
    filter(lat <= meanLat + 3*sdLat) %>%
    filter(lon >= meanLon - 3*sdLon) %>%
    filter(lon <= meanLon + 3*sdLon)
  
  # The data may find all point tagged with the locality, but there could be points inside 
  # of a bounding box defined by the locality that are not correctly tagged, so pick them up
  localLonMax = max(ha_df$lon) 
  localLonMin = min(ha_df$lon)
  localLatMax = max(ha_df$lat)
  localLatMin = min(ha_df$lat)
  
  locality_df = world_geo %>% filter(datasource=="External") %>%
    filter(lon<=localLonMax) %>% filter(lon>=localLonMin) %>%
    filter(lat<=localLatMax) %>% filter(lat>=localLatMin)
  
  points = as.data.frame(rbindlist(list(ha = ha_df,
                web = locality_df)))
  
}

visualizeLocality = function(geo_df, zoom=10, color="bw") {
  require(ggmap)
 
  local_geo=geo_df
  
  theMap = get_googlemap(center=c(lon = mean(local_geo$lon), lat=mean(local_geo$lat)), 
                         maptype="roadmap", 
                         color = color,   ## "color" or "bw" (black & white)
                         zoom=zoom,
                         scale=4)
  
  
  local_geo$datasource = factor(local_geo$datasource)
  
  ggmap(theMap, extent="device", legend="topleft") + 
    geom_point(data = local_geo, aes(x=lon, y=lat, colour=datasource, order=desc(datasource)), alpha=0.5) +
    scale_color_manual(name = "Data", values = colorRampPalette(c(HomeAwayBlue, "#000000", WebRed))(length(unique(local_geo$datasource))))+
    guides(colour = guide_legend(override.aes = list(size=4, shape=19, alpha=1))) +
    theme(legend.key=element_blank()) 
}