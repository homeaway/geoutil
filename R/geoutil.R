HomeAwayYellow = "#FED863"
HomeAwayBlue = "#2A6EBB"
HomeAwayGreen = "#839E73"
WebRed = "#FF5A60"


#' Finds the observations in geo_df based on candidates_df
#'
#' @param geo_df 
#' @param candidates_df 
#' @param stdDevs 
#'
#' @return a data frame consisting of all points in the geo_df, based on a bounding box formed from the
#' average lon/lat of the points in candidates_df, +/- the number of stdDevs
#' @export
#'
#' @examples pointsInLocality(data, data, 2)
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

#' Returns all observation inside the bounding box defined by lon/lat
#'
#' @param geo_df 
#' @param minlon 
#' @param maxlon 
#' @param minlat 
#' @param maxlat 
#'
#' @return data frame containing all obs inside the bounding box
#' @export
#'
#' @examples pointsInBox(df, 1,2,-1,1)
pointsInBox = function(geo_df, minlon, maxlon, minlat, maxlat) {
  geo_df %>% #filter(datasource=="Internal") %>% 
    filter(lat >= minlat) %>%
    filter(lat <= maxlat) %>%
    filter(lon >= minlon) %>%
    filter(lon <= maxlon)
}

#' Wrapper around a ggmap-based static map. Uses google maps.
#'
#' @param geo_df 
#' @param zoom 
#' @param color 
#'
#' @return the output of ggmap
#' @export
#'
#' @examples visualizeLocality(df)
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

#' Leaflet-mapping of geo_df
#'
#' @param geo_df 
#'
#' @return the leaflet map, for easy insertion into ggvis, if desired
#' @export
#'
#' @examples mapLocality(df)
mapLocality = function(geo_df) {
  require(leaflet)
  
  fac = colorFactor(c(HomeAwayBlue, WebRed), levels=unique(geo_df$datasource))
  
  attr <- 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
  template <- 'http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png'
  opts = tileOptions(subdomains='abcd', minZoom=0, maxZoom=20)
  leaflet(geo_df) %>% addTiles(urlTemplate=template, attribution=attr, options=opts) %>% 
    addCircleMarkers(geo_df$lon, geo_df$lat, color = ~fac(datasource), radius=2.5)
  
}