library(shiny)
library(leaflet)
library(raster)
library(DT)
library(rgdal)
library(spatial)
library(sp)
library(sf)
library(rmapshaper)
library(lwgeom)
library(dplyr)
library(purrr)
library(units)
library(tidyr)
library(glue)
library(reshape2)
library(igraph)
library(shinyWidgets)
library(shinydashboard)

#load data 
counties <- rgdal::readOGR(dsn="./data", layer = "mn_county_boundaries_500")
counties_latlon <- sp::spTransform(counties, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#read in .csv of centroids
utm15nCRS <- crs(counties_latlon)
MNcentroids <- read.csv("./data/MNcentroids")
centroids_sp <- SpatialPointsDataFrame(coords = MNcentroids[,2:3], data = MNcentroids, proj4string = utm15nCRS)
trueCentroids <- sp::spTransform(centroids_sp, sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


#define user interface
ui <- dashboardPage(skin = "black",
                    
                    dashboardHeader(title = "Connectivity App"),
                    
                    #create the sidebar tabs      
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Map", tabName = "Map", icon = icon("far", class = "far fa-map", lib = "font-awesome")),
                        menuItem("Metrics info", tabName = "metricsInfo", icon = icon("fas", class = "fas fa-table", lib = "font-awesome")),
                        menuItem("Dispersal info", tabName = "dispInfo", icon  = icon("fab",class = "fab fa-pagelines", lib = "font-awesome")),
                        menuItem("App info", tabName = "appInfo", icon = icon("fas", class = "fas fa-info", lib = "font-awesome")),
                        menuItem("Download the app", tabName = "download", icon = icon("fas", clas = "fas fa-file-download", lib = "font-awesome")),
                        tags$img(src='ENRTFlogo.png', width = 150, style="display: block; margin-left: auto; margin-right: auto;")
                      )
                    ),
                    
                    
                    #create the content to be displayed --> navigated to from the sidebar tabs
                    dashboardBody(
                      tabItems(
                        #the first tab is the map page
                        tabItem(
                          tabName = "Map",
                          fluidRow(
                            #devote the majority of the page to the interactive map
                            column(9, box(width = NULL, solidHeader = TRUE,
                                          leafletOutput("map")),
                                   tags$style(
                                     HTML(".shiny-notification {
                                          position:fixed;
                                          top: -1%;;
                                          left: -1%;;
                                          right: 0%;;
                                          width: 100%;;
                                          height: 100%;;
                                          font-size: 20px;;
                                          text-align:center
                                          }")
                                   )),
                            #add the user inputs to the right of the map
                            column(3, box(width = NULL, status = "warning",
                                          selectInput(inputId = "dispersal", label = "Set dispersal distance",
                                                      choices = c("50m"=50,"500m"=500,"1000m"=1000,"1500m"=1500,"2000m"=2000), width = 102), 
                                          radioButtons(inputId = "buttons", label = "Choose connectivity scenario", 
                                                       choiceNames = c("Current landscape", "Add a restoration", "Assess habitat loss"),
                                                       choiceValues = c("current","add","loss"))
                            ) 
                            )
                                     ),
                          fluidRow(
                            #add the two metric data tables below the map and the user inputs
                            column(6, 
                                   box(width = NULL, height = NULL, 
                                       DT::dataTableOutput("table1"))),
                            column(6, 
                                   box(width = NULL, height = NULL,
                                       DT::dataTableOutput("table2"))),
                            column(3))
                                   ),
                        #the second tab page is information on connectivity metrics
                        tabItem(
                          tabName = "metricsInfo",
                          tags$h3("Connectivity metrics information"),
                          #information on connectivity in general
                          tags$p("There are many aspects of connectivity (i.e. the degree to which organisms can move between habitat patches). We quantify these
                                 different aspects with different metrics. Different metrics might be more important to think about under different circumstances."), 
                          tags$br(),
                          #add in images that illustrate different aspects of connectivity
                          #save images in a folder named 'www' in your project directory - then refer to the picture by name using the tags$img command
                          tags$img(src = 'total.png' , height = 150, width = 350),
                          tags$p(tags$strong("Total patches"), "is the number of grassland patches in your network."),
                          tags$br(),
                          tags$img(src = 'nocomp.png' , height = 150, width = 350),
                          tags$p(tags$strong("Number of components"), "quantifies how many discrete sets of patches are connected to each other in the network. A more connected 
                                 network will have fewer components within it, as this means that many patches are connected to one another. The number of components in
                                 a network indicates something about the genetic substructure of populations, as components represent the effective range of
                                 different populations. Because components are isolated from one another, genetic flow does not occur between populations in different components."),
                          tags$br(),
                          tags$img(src = 'avgcomp.png' , height = 150, width = 350),
                          tags$p(tags$strong("Average component size"), "is the mean number of patches that make up components. As a network becomes more connected this 
                                 metrics increases because more and more patches are included in a component, on average."),
                          tags$br(),
                          tags$img(src = 'maxcomp.png' , height = 150, width = 350),
                          tags$p(tags$strong("Maximum component size"), "is the number of patches that make up the largest component in the network. The largest component 
                                 in a network is valuable betcause it represents the largest space a population can occupy."),
                          tags$br(),
                          tags$img(src = 'trans.png' , height = 150, width = 350),
                          tags$p(tags$strong("Transitivity"), "quantifies the extent to which a network's patches are well-connected. Transitivity ranges from 0 to 1, and as it increases
                                 it means that more patches in a component are connected to the same patches its neighbors are connected to. More simply, you can think of higher transitivity
                                 as indicating that there are more triangle-shaped connections within the nextwork. These triangle-shaped connections make a network more robust to habitat loss,
                                 because losing a single patch from such a cluster will not disrupt the ability of orgamisms to move between the other patches."),
                          tags$br(),
                          tags$img(src = 'iso.png' , height = 150, width = 350),
                          tags$p(tags$strong("Isolated patches"), "is the number of patches that are not connected to any other patch. A network with many isolated patches 
                                 indicates that there are many populations contained to a single patch with nowhere to disperse to and no new migrants arriving.
                                 Isolated patches are particularly vulnerable to extirpation; if a population
                                 in an isolated patch is lost it cannot be recolonized from a nearby population of the same species."),
                          tags$br(),
                          tags$img(src = 'net.png' , height = 150, width = 350),
                          tags$p("We can demonstrate all of these metrics  at once with this", tags$strong("example network."), align = "left"),
                          tags$p(tags$strong("Total patches:"), "9", align = "left"),
                          tags$p(tags$strong("Number of components:"), "2", align = "left"),
                          tags$p(tags$strong("Average component size:"), "4.5", align = "left"),
                          tags$p(tags$strong("Maximum component size:"), "5", align = "left"),
                          tags$p(tags$strong("Transitivity:"), "0.58", align = "left"),
                          tags$p(tags$strong("Isolated fragments:"), "0", align = "left")
                          ),
                        #the third tab page is information on dispersal
                        tabItem(
                          tabName = "dispInfo",
                          tags$h3("Dispersal information"),
                          tags$br(),
                          tags$p("Different organisms have different dispersal abilities - think of the distance a monarch butterfly can fly versus the distance an echinacea seed can travel by wind. That's why it's important to calculate connectivity for a range of different dispersal distances; we want to get a good sense of how different species are able to move through the landscape. As you change the dispersal distance in the app we want you to be able to think about what species might be moving those different distances. Here are some examples to help you out."),
                          tags$h5(tags$strong("Short")),
                          tags$p(tags$a(href="https://www.wildflower.org/plants/result.php?id_plant=denu2", "Nuttal's Larkspur"), tags$i("(Delphinium nuttallianum)"), 
                                 ": The pollen of these larkspurs has been measured to move only", tags$strong("1 meter"), "on average", tags$a(href="https://www.nature.com/articles/277294a0","(Price & Waser 1979).")),
                          tags$p(tags$a(href="https://www.minnesotawildflowers.info/flower/narrow-leaved-purple-coneflower", "Narrow-leaved purple coneflower"), tags$i("(Echinacea angustifolia)"),
                                 ": On average, the pollen of purple coneflowers has been found to disperse", 
                                 tags$strong(" 1 meter"), tags$a(href="https://bsapubs.onlinelibrary.wiley.com/doi/full/10.3732/ajb.1300065","(Ison et al. 2014).")),
                          tags$p(tags$a(href="https://en.wikipedia.org/wiki/Western_harvest_mouse", "Western Harvet Mouse"), tags$i("(Reithrodontomys megalotis)"),
                                 ": In one study, the median distance male mice taveled from their home sites was found to be", tags$strong("33.7 meters"), 
                                 tags$a(href="https://academic.oup.com/jmammal/article/76/2/358/958242", "(Skupiski, 1995).")),
                          tags$h5(tags$strong("Medium")),
                          tags$p(tags$a(href="https://www.allaboutbirds.org/guide/Dickcissel/lifehistory#", "Dickcissel"), tags$i("(Spiza Americana)"),
                                 ": Dickcissel birds disperse, on average,", tags$strong("222 meters"), 
                                 "from one nest to their next nest, although female Diskissels that fail to hatch eggs at one nest will travel over 10,000 meters
                                 to start a new nest", tags$a(href="https://academic.oup.com/auk/article/121/4/1250/5147522", "(Walk et al. 2004)")),
                          tags$p(tags$a(href="https://en.wikipedia.org/wiki/Bobolink", "Bobolink"), tags$i("(Dolichonyx oryzivorus)"),
                                 ": Bobolinks have been found to disperse a median distance of", tags$strong("975 meters"), 
                                 tags$a(href="https://academic.oup.com/auk/article/126/2/310/5148314","(Fajardo et al. 2009)"), "from their birth site to breeding site."),
                          tags$h5(tags$strong("Long")),
                          tags$p(tags$a(href="https://en.wikipedia.org/wiki/Red_fox", "Red Fox"), tags$i("(Vulpes vulpes)"),
                                 ": Red foxes can cover quite great distances! Juvenile male foxes have been found to disperse", 
                                 tags$strong("31,000 meters"), "on average from their home sites", tags$a(href="https://www.jstor.org/stable/3830425?seq=1#page_scan_tab_contents",
                                                                                                          "(Storm et al. 1996).")),
                          tags$p(tags$a(href="https://en.wikipedia.org/wiki/Burrowing_owl", "Burrowing Owl"), tags$i("(Athene cunicularia)"),
                                 ": Burrowing owls have been shown to disperse", 
                                 tags$strong("2,802 meters"), "(on average) from their nests", tags$a(href="https://www.jstor.org/stable/20491306?seq=1#page_scan_tab_contents",
                                                                                                      "(Catlin & Rosenberg 2008)."))
                          ),
                        #the final tab page is general information about the app - what it's used for and who made it
                        tabItem(
                          tabName = "appInfo",
                          tags$h3("App information"),
                          tags$br(),
                          tags$strong("Overview"),
                          tags$p("This app is meant to facilitate exploration of connectivity in Minnesota's 
                                 grasslands. By clicking on a county you see its current network of  
                                 grasslands. If an organism can disperse from one grassland patch to another 
                                 then those two patches are considered connected. All these connected
                                 grasslands on a landscape make up a network, and these networks look different 
                                 when considering different organisms. With this app, you can explore connectivity in Minnesota's grasslands 
                                 by clicking on counties and setting the dispersal distance. This allows you 
                                 to see how these networks look throughout the state and for organisms with 
                                 different dispersal abilities."),
                          tags$strong("Modeling future connectivity scenarios"),
                          tags$p("Grassland restoration has the potential to help increase connectivity by 
                                 creating more patches for organisms to disperse into. However, some locations 
                                 might have a larger impact on connectivity than others. Switch the connectivity 
                                 scenario to ''add'' and then click within your network to project how adding a grassland in that 
                                 location would change connectivity."),
                          tags$p("Despite the efforts of agencies, non-profits, and concerned citizens to protect
                                 grassland habitat, habitat loss continues to happen. Therefore, it is important
                                 to understand the consequences of losing habitat for connectivity. To project 
                                 how losing an exisiting patch of grassland would impact connectivity, switch the connectivity
                                 scenario to ''loss'', and then click on an exisiting patch within the network."),
                          tags$br(),
                          tags$strong("Data"),
                          tags$p("Two data sets are used to construct the interactive map."), 
                          tags$p("For grassland patch centroids we used a comprehensive grassland database for the region, developed by 
                                 Steve Chaplin and Rich Johnson in 2015 at the The Nature Conservancy; please contact prairieconnectivty@gmail.com
                                 for a full description of the data. The database consisted of all grassland types in the region, 
                                 including native remnant prairie, reconstructed grasslands, and hay/pasture fields. 
                                 These patches represent suitable habitat for grassland species."),
                          tags$p("Minnesota county boundary shapefiles were aqquired from the MN geospatial commons.", 
                                 tags$a(href="https://gisdata.mn.gov/dataset/bdry-counties-in-minnesota", "Click here to see the source page.")),
                          tags$br(),
                          tags$strong("Funding"),
                          tags$p("This work was funded by the LCCMR ENRTF Grant (M.L. 2016, Chp. 186, Sec. 2, Subd. 08b)"),
                          tags$img(src='ENRTFlogo.png', width = 100),
                          tags$br(),
                          tags$br(),
                          tags$p("To learn more about our research visit", tags$a(href="https://allisonkshaw.weebly.com/", "our lab's website"), 
                                 "and read", tags$a(href="https://onlinelibrary.wiley.com/doi/abs/10.1111/rec.12999", "our paper"), "on apps and 
                                 restoration in", tags$em("Restoration Ecology.") ),
                          tags$p((icon("fas", class = "fas fa-seedling", lib = "font-awesome")), tags$i("App created by Katie Sperry - prairieconnectivity@gmail.com")),
                          tags$p(tags$i("Last updated 06/26/2019"))
                          ),
                        tabItem(
                          tabName = "download",
                          tags$h3("Download the app"),
                          tags$br(),
                          tags$p("We host this app online to enhance its accessibility, but due to server limitations the app's performance can be slow online.
                                 Downloading the app and running it locally with R Studio will likely enhance its performance. 
                                 Please note, however, that some networks take a while to compute and display. You can download the app on github 
                                 by clicking the icon below.",
                                 tags$a(href="https://github.com/prairieconnectivity/ConnectivityApp", tags$h2(icon("fab", class = "fab fa-github", lib = "font-awesome"))))
                        )
                        )
                      
                        )
                    
                        )



# Define server logic 
server <- function(input, output) {
  
  
  #A function that takes in a distance matric and a dispersal distance and returns a data frame of network metrics 
  metricsUtility <- function(distanceMatrix, dispersalDistance){
    #convert the matrix to a datafram
    df <- data.frame(as.matrix(distanceMatrix))
    #create unique names for all collumns
    names(df) <- as.factor(as.character(seq(1, nrow(df))))
    #add these same names as a collumn to the data frame, so that collumns and rows match up
    df$id <- as.factor(as.character(seq(1, nrow(df))))
    #re-cast the data so each row is every collumn/row id combination - this gives a df with all distances in their own row
    df_melt <- reshape2::melt(df, id="id")
    disp_dist <- seq(1, 2000, by=1)
    #create an igraph network from the re-cast df, including only distances that are equal to or less than the user-specified dispersal distance
    g <- igraph::graph.edgelist(as.matrix(subset(df_melt, value <= disp_dist[as.numeric(dispersalDistance)])[,1:2]), directed=FALSE )
    g <- igraph::simplify(g)  
    #start extracting connectivity data from this network
    #the number of grasslands in your network is the number of rows
    no_grasslands <- nrow(df)
    #find number of components with count_components()
    no_comp <- igraph::count_components(g, mode = c("weak", "strong"))
    #components() gives you the different components of the network
    comp <- igraph::components(g, mode = c("weak", "strong"))
    #these components can be sorted by their number of nodes - you can find out the largest size or the average size using base r functions
    maxcomp <- max(comp$csize)
    avgsize <- base::mean(comp$csize)
    #find average transitivity
    trans <- mean(igraph::transitivity(g, type="local"), na.rm=TRUE)
    #isolated components, aka the number of components that are a single node
    isolated <- length(comp$csize[comp$csize==1])
    #compile these connectivity metrics from your network into the dataframe that the function produces
    network_results <- data.frame(no_grasslands,no_comp,round(avgsize,digits=4),maxcomp,round(trans,digits=4),isolated) %>% 
      'colnames<-'(c("Total number of patches:","Number of components:","Avg patches per component:","Max component size:","Transitivity:","Number of isolated patches:"))
    return(network_results)
  }
  
  #A function that takes in a distance, a distance matric, a spatial object of your network's nodes, and the vector of unique IDs for each node, 
  #and creates lines representing the calculated network
  linesUtility <- function(distanceThreshold, distanceMatrix, sf_prairies, grassnames){
    #filter out distances from your distance matrix greater than the slider bar dictates 
    #ultimately, this first function creates a df where each row is the beggining and end of each line in your network, 
    #and the distance of that line
    
    dist_checker_t <- function(origin){
      distanceMatrix %>% 
        dplyr::select(origin, name) %>% 
        'colnames<-'(c('dis','name')) %>% 
        transform(dis=as.numeric(dis)) %>% 
        dplyr::filter(dis < distanceThreshold) %>% 
        dplyr::filter(!is.na(dis)) %>% 
        cbind(prairie = origin) %>% 
        'colnames<-'(c('Distance','Target','Origin')) %>% 
        return()
    }
    
    #the next three functions take the information about which nodes have lines between them and how long, and 
    #creates spatially explict line objects --> the final product can actually be mapped using leaflet
    linestring_func_t <- function(origin, target){
      sf_prairies %>% 
        dplyr::filter(Names == origin | Names == target) %>% 
        sf::st_coordinates() %>% 
        sf::st_linestring() %>% 
        return()}
    
    locs <- grassnames %>% 
      purrr::map(~(.x) %>% dist_checker_t) %>% 
      purrr::invoke(rbind,.) %>% 
      dplyr::mutate(ls = map2(Origin,Target,linestring_func_t))
    
    lines <- sf::st_sf(origin = locs$Origin,
                       target = locs$Target,
                       distance = locs$Distance,
                       geometry = sf::st_sfc(locs$ls),
                       dim = 'XY')
    return(lines)}
  
  #Draws an unreactive map of Minnesota counties - this never changes with user input
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = counties_latlon, layerId = ~CTYONLY_ID, label = ~CTY_NAME, color = "ivory", weight = 2, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.1) %>% 
      setView(lat = 46.392410, lng= -94.636230, zoom = 7) 
    
  })
  
  
  #Reactively selects a county from a click
  county <- eventReactive(input$map_shape_click, {
    #name the event of clicking on the shapes and extract lattitude and longitude from the click
    event <- isolate(input$map_shape_click)
    lt <- event$lat
    ln <- event$lng
    #create a point object from these coordinates
    newcoords <- as.data.frame(cbind(ln, lt))
    pt <- sp::SpatialPoints(newcoords)
    proj4string(pt) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    #subset the county from the larger dataset that this point resides in
    county <- counties_latlon[pt,]
    
  })
  
  #Reactively selects the grasslands that are in the selected county
  grasses <- eventReactive({county()
  }, {
    grasses <- raster::intersect(trueCentroids, county())
    
  })
  
  
  #Reactively creates connections (lines) for the map that represent the network given the grasses and the dispersal distance
  connectionsOriginal <- eventReactive({
    input$dispersal
    clickLogIN()
  }, {
    #having the function react to but then isolating the click log ensures that the creation of lines matches intuitively with user clicks
    Log <- isolate(clickLogIN())
    i <- nrow(Log)
    withProgress(message = "Please wait:", detail = "creating baseline network", value = 0,{
      #check that the county has grasslands in it
      if((length(grasses())>0)){
        #create a df of the unique IDs and coordinates of all the grasslands in the county
        glng <- grasses()@coords[,"x"]
        glat <- grasses()@coords[,"y"]
        incProgress(0.1)
        gnames <- as.character(seq.int(nrow(grasses()@coords)))
        prairies <- data.frame(gnames, glat, glng, stringsAsFactors = F) %>% 
          'colnames<-'(c("Names","Lat","Lng"))
        incProgress(0.1)
        #use this df to make a spatial object - essentially a list of grassland IDs and coordinates that have a projection system
        p_geo <- sf::st_as_sf(prairies, coords = c('Lng','Lat'), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        incProgress(0.1)
        #create a spatially explict distance matrix from this spatial object - calcualtes the distances between all grasslands within the projection system
        dist_matrix <- as.data.frame(sf::st_distance(p_geo,p_geo))
        incProgress(0.1)
        #because this is a symmetric matrix, get rid of the upper triangle to reduce redundancies
        upper.tri(dist_matrix)
        dist_matrix[upper.tri(dist_matrix)] <- NA
        incProgress(0.1)
        #create an igraph network and calculate connectivity metrics using the metricsUtility function described above
        metrics_matrix_current <- metricsUtility(dist_matrix, input$dispersal)
        incProgress(0.1)
        #save these metrics in the universal metric logs
        old_metrics_current <- metricsLogCurrent()
        new_metrics_current <- rbind(old_metrics_current,metrics_matrix_current)
        metricsLogCurrent(new_metrics_current)
        incProgress(0.1)
        #add unique ID collumn and row names to prep the matric for the linesUtility function
        dist_matrix <- dist_matrix %>% 
          'colnames<-'(gnames) %>% 
          cbind(name = gnames)
        incProgress(0.25)
        #create line objects that can be reactively mapped
        connectionsOriginal <- linesUtility(as.numeric(input$dispersal),dist_matrix,p_geo,gnames)
      }
    })
  })
  
  #reactively create expanded grassland objects when testing how restoration would impact connectivity
  grassesAdded <- eventReactive({
    county()
    input$map_click
    clickLogIN()
  }, {
    #isolate the click log
    Log <- isolate(clickLogIN())
    i <- nrow(Log)
    #ensure that the scenario selected is "add a restoration"
    if(isTRUE((Log$add[i])=="1")){
      #Subset the current grasslands by selected county
      grassesA <- raster::intersect(trueCentroids, county())
      #create a point where the user clicked
      clickAdd <- input$map_click
      pt = sp::SpatialPoints(cbind(clickAdd$lng,clickAdd$lat))
      proj4string(pt) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
      #Check whether the clicked point resides in the selected county before adding and displaying it
      check_location <- raster::intersect(pt, county())
      #If it is in the selected county, add it to the set of grasses
      if(length(check_location)>0){
        grassesAdded <- sp::rbind.SpatialPoints(grassesA,pt) 
        #If it is not, add the last point that was in the selected county ("make the map look the same")
      }else{
        lngLAST <- Log()$LON[i] 
        latLAST <- Log()$LAT[i]
        ptLAST = sp::SpatialPoints(cbind(lngLAST,latLAST))
        proj4string(ptLAST) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        grassesAdded <- sp::rbind.SpatialPoints(grassesA,ptLAST)
      }
    }
  })
  
  #reactively create lines for grasses added - follows the same format at connectionsOriginal() 
  connectionsAdded <- eventReactive({
    #grassesAdded()
    input$dispersal
    clickLogIN()
  }, {
    Log <- shiny::isolate(clickLogIN())
    i <- nrow(Log)
    withProgress(message = "Please wait:", detail = "creating expanded network", value = 0,{
      if(isTRUE((Log$add[i])=="1")){
        glng <- grassesAdded()@coords[,"x"]
        glat <- grassesAdded()@coords[,"y"]
        gnames <- as.character(seq.int(nrow(grassesAdded()@coords)))
        incProgress(0.2)
        prairies <- data.frame(gnames, glat, glng, stringsAsFactors = F) %>% 
          'colnames<-'(c("Names","Lat","Lng"))
        incProgress(0.2)
        p_geo <- sf::st_as_sf(prairies, coords = c('Lng','Lat'), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        incProgress(0.1)
        dist_matrix <- as.data.frame(sf::st_distance(p_geo,p_geo))
        incProgress(0.2)
        upper.tri(dist_matrix)
        dist_matrix[upper.tri(dist_matrix)] <- NA
        metrics_matrix_projected <- metricsUtility(dist_matrix, input$dispersal)
        incProgress(0.05)
        old_metrics_projected <- metricsLogProjected()
        new_metrics_projected <- rbind(old_metrics_projected,metrics_matrix_projected)
        metricsLogProjected(new_metrics_projected)
        incProgress(.05)
        dist_matrix <- dist_matrix %>% 
          'colnames<-'(gnames) %>% 
          cbind(name = gnames)
        incProgress(0.2)
        connectionsAdded <- linesUtility(as.numeric(input$dispersal),dist_matrix,p_geo,gnames)
        
      }
    })
  })
  
  #reactively creates a reduced grassland object for when testing habitat loss
  grassesSub <- eventReactive({input$map_marker_click
    county()
    grasses()}, {
      Log <- shiny::isolate(clickLogIN())
      i <- nrow(Log)
      #make sure that scenario selected is 'loss'
      if((!is.null(input$map_marker_click))&&(isTRUE(Log$add[i]=="2"))&&(length(grasses())>0)){
        #remove the selected grassland from the dataset
        sub <- input$map_marker_click
        grassesA <- raster::intersect(trueCentroids, county())
        grassesSub <- grassesA[grassesA@coords[,"y"]!=sub$lat,]
      }
      
    })
  
  #reactively create lines for grassesSub - follows the same protocol as connectionsOriginal()
  connectionsSub <- eventReactive({
    input$dispersal
    clickLogIN()
  }, {
    Log <- shiny::isolate(clickLogIN())
    i <- nrow(Log)
    withProgress(message = "Please wait:", detail = "creating reduced network", value = 0,{
      if((!is.null(input$map_marker_click))&&(isTRUE(Log$add[i]=="2"))&&(length(grasses())>0)){
        glng <- grassesSub()@coords[,"x"]
        glat <- grassesSub()@coords[,"y"]
        gnames <- as.character(seq.int(nrow(grassesSub()@coords)))
        incProgress(0.2)
        prairies <- data.frame(gnames, glat, glng, stringsAsFactors = F) %>% 
          'colnames<-'(c("Names","Lat","Lng"))
        incProgress(0.2)
        p_geo <- sf::st_as_sf(prairies, coords = c('Lng','Lat'), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        incProgress(0.1)
        dist_matrix <- as.data.frame(sf::st_distance(p_geo,p_geo))
        incProgress(0.2)
        upper.tri(dist_matrix)
        dist_matrix[upper.tri(dist_matrix)] <- NA
        metrics_matrix_sub <- metricsUtility(dist_matrix, input$dispersal)
        incProgress(0.05)
        old_metrics_sub <- metricsLogSub()
        new_metrics_sub <- rbind(old_metrics_sub,metrics_matrix_sub)
        metricsLogSub(new_metrics_sub)
        incProgress(.05)
        dist_matrix <- dist_matrix %>% 
          'colnames<-'(gnames) %>% 
          cbind(name = gnames)
        incProgress(0.2)
        connectionsSub <- linesUtility(as.numeric(input$dispersal),dist_matrix,p_geo,gnames)
      }
    }) 
  })
  
  #map different reactive objects depedning on what scenario is chosen
  observe({
    Log <- isolate(clickLogIN())
    i <- nrow(Log)
    #when add=0 display current connectvity scenario
    if(isTRUE((Log$add[i])=="0")){
      leafletProxy("map") %>% 
        clearGroup("selected_area") %>% 
        clearGroup("added_grasses") %>% 
        clearGroup("added_connections") %>% 
        clearGroup("added_restoration") %>% 
        clearGroup("drawn_connections") %>% 
        clearGroup("grasses_sub") %>% 
        clearGroup("connections_sub") %>%  
        clearGroup("selected_grasses") %>% 
        addPolygons(data=req(county()), color="lightsteelblue", group="selected_area") %>% 
        addPolylines(data=req(connectionsOriginal()), color = "orange", weight = 3, group = "drawn_connections") %>% 
        addCircleMarkers(data=req(grasses()), color="royalblue", stroke = TRUE, opacity = 1, weight = 1, 
                         fill = TRUE, fillColor = "royalblue", fillOpacity = 0.15, radius=4, group="selected_grasses") 
    }else{
      #when add=1 display restoration addition scenario
      if(isTRUE(Log$add[i]=="1")){
        leafletProxy("map") %>% 
          clearGroup("selected_area") %>% 
          clearGroup("selected_grasses") %>% 
          clearGroup("drawn_connections") %>% 
          clearGroup("grasses_sub") %>% 
          clearGroup("connections_sub") %>% 
          addPolygons(data=req(county()), color="lightsteelblue", group="selected_area") %>% 
          clearGroup("added_connections") %>% 
          addPolylines(data=req(connectionsAdded()), color = "orange", weight = 3, group = "added_connections") %>% 
          clearGroup("added_restoration") %>% 
          clearGroup("added_grasses") %>% 
          addCircleMarkers(data=req(grassesAdded()[1:length(grassesAdded())-1]), color="royalblue", stroke = TRUE, opacity = 1, weight = 1, 
                           fill = TRUE, fillColor = "royalblue", fillOpacity = 0.15, radius=4, group="added_grasses") %>% 
          addCircleMarkers(data=req(grassesAdded()[length(grassesAdded())]), color = "black", stroke = TRUE, opacity = 1, weight = 1, 
                           fill = TRUE, fillColor = "black", fillOpacity = 0.15, radius=4, group = "added_restoration")
        
      }else{
        #if add doesn't equal 0 or 1 it equals 2: display habitat loss scenario
        leafletProxy("map") %>% 
          clearGroup("selected_area") %>% 
          clearGroup("selected_grasses") %>% 
          clearGroup("drawn_connections") %>% 
          clearGroup("grasses_sub") %>% 
          clearGroup("connections_sub") %>% 
          addPolygons(data=req(county()), color="lightsteelblue", group="selected_area") %>% 
          clearGroup("added_restoration") %>% 
          clearGroup("added_grasses") %>% 
          clearGroup("added_connections") %>% 
          addPolylines(data=req(connectionsSub()), color = "orange", weight = 3, group = "connections_sub") %>% 
          addCircleMarkers(data=req(grassesSub()), color = "royalblue", stroke = TRUE, opacity = 1, weight = 1, 
                           fill = TRUE, fillColor = "royalblue", fillOpacity = 0.15, radius=4, group = "grasses_sub") 
        
      }
    }
  }) 
  
  #Observe grasses to calculate and update metrics in the datatable
  observe({
    metricsLogCurrent <<- reactiveVal(data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE))
    metricsLogProjected <<- reactiveVal(data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE))
    metricsLogSub <<- reactiveVal(data.frame(matrix(ncol = 6, nrow = 0),stringsAsFactors = FALSE))
    
  })
  
  #display metrics on the tables below the map
  observe({
    Log<-clickLogIN()
    k <- nrow(Log)
    if(isTRUE((Log$add[k])=="0")){
      output$table2 <- NULL
      if(length(grasses())>0){
        currentMetrics <- metricsLogCurrent()
        i <- nrow(currentMetrics)
        output$table1 <- renderDataTable(t(as.data.frame(currentMetrics[i,])), colnames = NULL, 
                                         caption = tags$strong("Current Connectivity"), options = list(dom = 't',bSort=FALSE))
      }else{
        network_results <- as.data.frame(c(0,0,0,0,0,0), ncol = 1, byrow = TRUE) %>% 
          'colnames<-'(c(" ")) %>% 
          'rownames<-'(c("Total Grasslands:","Number of Components:","Avg Component Size:","Max Component Size:","Transitivity:",
                         "Isolated Fragments:"))
        output$table1 <- renderDataTable(network_results, colnames = NULL, caption = tags$strong("Current Connectivity"), 
                                         options = list(dom = 't',bSort=FALSE))}
    }else{
      if(isTRUE(Log$add[k]=="1")){
        if(length(grasses())>0){
          currentMetrics <- metricsLogCurrent()
          i <- nrow(currentMetrics)
          output$table1 <- renderDataTable(t(as.data.frame(currentMetrics[i,])), colnames = NULL, 
                                           caption = tags$strong("Current Connectivity"), options = list(dom = 't',bSort=FALSE))
          j <- nrow(metricsLogProjected())
          output$table2 <- renderDataTable(t(as.data.frame(metricsLogProjected()[j,])),colnames = NULL, 
                                           caption = tags$strong("Projected Connectivity with Restoration"), options = list(dom = 't',bSort=FALSE))
        }else{
          network_results <- as.data.frame(c(0,0,0,0,0,0), ncol = 1, byrow = TRUE) %>% 
            'colnames<-'(c(" ")) %>% 
            'rownames<-'(c("Total Grasslands:","Number of Components:","Avg Component Size:","Max Component Size:","Transitivity:",
                           "Isolated Fragments:"))
          output$table1 <- renderDataTable(network_results, colnames = NULL, caption = tags$strong("Current Connectivity"), 
                                           options = list(dom = 't',bSort=FALSE))
          j <- nrow(metricsLogProjected())
          output$table2 <- renderDataTable(t(as.data.frame(metricsLogProjected()[j,])),colnames = NULL, 
                                           caption = tags$strong("Projected Connectivity with Restoration"), 
                                           options = list(dom = 't',bSort=FALSE))}
        
      }else{
        if(length(grasses())>0){
          i <- nrow(metricsLogCurrent())
          output$table1 <- renderDataTable(t(as.data.frame(metricsLogCurrent()[i,])), colnames = NULL, 
                                           caption = tags$strong("Current Connectivity"), options = list(dom = 't',bSort=FALSE))
          j <- nrow(metricsLogSub())
          output$table2 <- renderDataTable(t(as.data.frame(metricsLogSub()[j,])),colnames = NULL, 
                                           caption = tags$strong("Projected Connectivity with Habitat Loss"), 
                                           options = list(dom = 't',bSort=FALSE))
        }else{
          network_results <- as.data.frame(c(0,0,0,0,0,0), ncol = 1, byrow = TRUE) %>% 
            'colnames<-'(c(" ")) %>% 
            'rownames<-'(c("Total Grasslands:","Number of Components:","Avg Component Size:","Max Component Size:","Transitivity:",
                           "Isolated Fragments:"))
          output$table1 <- renderDataTable(network_results, colnames = NULL, caption = tags$strong("Current Connectivity"), 
                                           options = list(dom = 't',bSort=FALSE))
          j <- nrow(metricsLogSub())
          output$table2 <- NULL}
      }
      
    }
    
  })
  
  #Evaluate whether an area clicked is in or outside of the selected area 
  #and if in adds it to the reactive dataframe
  clickLogIN <<- reactiveVal(data.frame(data.frame(matrix(ncol = 8, nrow = 0)),stringsAsFactors = FALSE))
  
  observeEvent({input$map_click
    county()
    input$buttons
  },{
    clickAdd <- isolate(input$map_click)
    lngAdd <- clickAdd$lng 
    latAdd <- clickAdd$lat
    pt = SpatialPoints(cbind(clickAdd$lng,clickAdd$lat))
    proj4string(pt) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
    
    #Check whether the clicked point resides in the selected county before adding and displaying it
    check_location <- raster::intersect(pt, county())
    i <- NROW(clickLogIN())
    #If it is in the selected county, add it
    if(length(check_location)>0){
      
      switch_button <- switch(input$buttons,
                              current = "0",
                              add = "1",
                              loss = "2")
      old_values_in <- clickLogIN()
      clicked_in <- clickAdd
      id_new_in <- (NROW(clickLogIN())+1)
      lat_new_in <- clicked_in$lat
      lon_new_in <- clicked_in$lng
      county_name <- county()$CTY_NAME
      if(i==0){
        last_name <- "first click"
        last_button <- "first click"
      }else{
        last_name <- old_values_in$county_name[NROW(old_values_in)]
        last_button <- old_values_in$switch_button[NROW(old_values_in)]
      }
      if((switch_button=="0")||(last_name!=county_name)||(last_button!=switch_button)){
        add <- "0"
      }else if ((switch_button=="1")&&(last_name==county_name)&&(last_button==switch_button)){
        add <- "1"
      }else if ((switch_button=="2")){
        add <- "2"
      }
      new_values_in <- data.frame(ID=id_new_in,LAT=lat_new_in,LON=lon_new_in,county_name,last_name,switch_button,last_button,add)
      new_df_in <- rbind(old_values_in,new_values_in)
      clickLogIN(new_df_in)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
