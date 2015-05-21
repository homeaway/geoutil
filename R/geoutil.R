HomeAwayYellow = "#FED863"
HomeAwayBlue = "#2A6EBB"
HomeAwayGreen = "#839E73"
WebRed = "#FF5A60"

pointsInLocality = function(geo_df, candidates_df, stdDevs=3){
  require(dplyr)
  require(data.table)
  
  meanLat = mean(candidates_df$lat)
  sdLat = sd(candidates_df$lat)
  meanLon = mean(candidates_df$lon)
  sdLon = sd(candidates_df$lon)
  
  # now go back and scoop up all the points from the geo_df. This will help to 
  # catch misspellings and the like.
  pointsInBox(geo_df, 
              meanLon - stdDevs*sdLon,
              meanLon + stdDevs*sdLon,
              meanLat - stdDevs*sdLat,
              meanLat + stdDevs*sdLat)
}

pointsInBox = function(geo_df, minlon, maxlon, minlat, maxlat) {
  geo_df %>% #filter(datasource=="Internal") %>% 
    filter(lat >= minlat) %>%
    filter(lat <= maxlat) %>%
    filter(lon >= minlon) %>%
    filter(lon <= maxlon)
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

mapLocality = function(geo_df) {
  require(leaflet)
  
  fac = colorFactor(c(HomeAwayBlue, WebRed), levels=unique(geo_df$datasource))
  
  attr <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  template <- 'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png'
  opts = tileOptions(subdomains='abcd', minZoom=0, maxZoom=20)
  leaflet(geo_df) %>% addTiles(urlTemplate=template, attribution=attr, options=opts) %>% 
    addCircleMarkers(geo_df$lon, geo_df$lat, color = ~fac(datasource), radius=2.5)
  
}