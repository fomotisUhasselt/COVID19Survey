library(classInt)
library(rgdal)
library(RColorBrewer)

### Belgium map
belgium2 <- read_sf("shapeFiles/BE_SB_TF_PD_STATDIS_2014.shp", quiet = TRUE)

#compute area per province
belgium_province  <- belgium2 %>% group_by(Prov_nl) %>% 
  summarise(Province_area = sum(Shape_Area))

#compute area per Gemeente
belgium_gemeente  <- belgium2 %>% group_by(Gemeente, Nis_012011, Prov_nl, Reg_nl) %>% 
  summarise(Gemeente_area = sum(Shape_Area))


#convert it
belgium_province_conv <- st_transform(belgium_province, '+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')
belgium_gemeente_conv <- st_transform(belgium_gemeente, '+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')

#obtain respondents per province and mere with data
province_data <- lapply(surv_sums, function(x) {
  
  
  respondents_group(belgium_province_conv, 
                    x$pdata_province, 
                    by.x = "Prov_nl", by.y = "Province")
  
})

#### make the function for leaflet plot for the predicted probabilities
map.shp <- readOGR(dsn="shapeFiles",layer="Belgium_NIS (1)")

pred.symp <- lapply(list.files("spatialPredictions", pattern = ".txt", 
                               full.names = TRUE), 
                    function(x) {
                      
                      read.table(x, header = T)[,1]
                    }) #%>% do.call(cbind.data.frame, .)
#names(pred.symp) <- paste0("predictionSurvey", 2:3)


spatial_car_maps <- function(pred, nclr = 8, map.shp) {
  
  map.shp2 <- st_as_sf(map.shp) %>% 
    mutate(Prediction = pred) 
  
  map.shp3 <-  merge(belgium_gemeente, 
                     data.frame(
                       SP_ID = map.shp2$SP_ID,
                       NIS = map.shp2$NIS,
                       Prediction = map.shp2$Prediction,
                       stringsAsFactors = FALSE
                     ),
                     by.x = "Nis_012011",
                     by.y = "SP_ID"
  )
  map.shp4 <- st_transform(map.shp3, '+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')
  
  nclr <- nclr       #### number of colors to be used
  plotclr <- rev(brewer.pal(nclr,"RdYlGn")) 
  breaks <- c(round(min(map.shp4$Prediction),4)-0.0001,
              round(as.numeric(quantile(map.shp4$Prediction,probs = seq(0, 1, 0.125))),4)[2:8],
              round(max(map.shp4$Prediction),4)+0.0001)
  class <- classIntervals(map.shp4$Prediction, nclr, style = "fixed",fixedBreaks = breaks)
  color_bins <- class$brks
  color_pal <- colorBin( 
    plotclr, domain = belgium_gemeente_conv$Gemeente, 
    bins = color_bins
  )
  
  leaflet(data = map.shp4) %>% 
    
    addLegend("bottomleft", pal = color_pal, 
              values = ~ pred, 
              title = "<small>Predicted probability of having at least 1 
            COVID-19 symptom <br /> (mean, CAR convolution model, 
            <br /> corrected for age and gender)</small>") %>% 
    
    addPolygons(
      data = map.shp4,
      color = "#444444",
      weight = 2,
      #stroke = FALSE,
      smoothFactor = 0.5, 
      opacity = 1.0,
      fillOpacity = 0.5,
      fillColor = ~color_pal(map.shp4$Prediction),
      highlightOptions = highlightOptions(color = "white", weight = 1,
                                          bringToFront = TRUE),
      label = sprintf("<strong>%s (Prob = %g )</strong>", 
                      map.shp4$Gemeente, 
                      map.shp4$Prediction) %>% lapply(htmltools::HTML)
    )
  
}



#make the leaflet plot for te number of respondents
maps_plot <- function(plotdata) {
  
  ss <- seq(0, 500, 50)
  mm <- max(plotdata$Number)/1000
  ss_mm <- ss[which(mm <= ss)[1]]
  color_bins <- seq(0, ss_mm, by = 50)
  color_pal <- colorBin("YlOrRd", domain = plotdata$Prov_nl, 
                        bins = color_bins)
  
  leaflet(data = plotdata) %>% 
    addLegend("bottomright", pal = color_pal, values = ~province_data$Number/1000,
              title = "<small>Respondents per 1,000</small>")  %>% 
    
    addPolygons(
      data = plotdata,
      color = "#444444",
      weight = 2,
      #stroke = FALSE,
      smoothFactor = 0.5, 
      opacity = 1.0,
      fillOpacity = 0.5,
      fillColor = ~color_pal(plotdata$Number / 1000),
      highlightOptions = highlightOptions(color = "white", weight = 5,
                                          bringToFront = TRUE),
      label = sprintf("<strong>%s (%g respondents)</strong>", 
                      plotdata$Prov_nl, 
                      plotdata$Number) %>% lapply(htmltools::HTML)
    ) 
  
}