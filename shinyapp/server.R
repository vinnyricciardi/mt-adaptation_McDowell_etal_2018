library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
# library(htmltools)
# library(htmlwidgets)
library(ggplot2)
# library(cowplot)
library(ggridges)
library(shinyjs)
library(logging)
library(stringr)
library(splitstackshape)
library(rsconnect)

basicConfig()

options(shiny.error = function() {
  logging::logerror(sys.calls() %>% as.character %>% paste(collapse = ", ")) })

# Read data
df <- read.csv("data.csv", stringsAsFactors = FALSE)
# df <- unique(df[ , 2:13])  # make sure to do this in pre-processing (need lookup column)
df <- df[complete.cases(df[, c('lats', 'longs')]), ]

df$StudyID <- str_replace_all(df$StudyID, "dx.doi.org/", "")
df$StudyID <- str_replace_all(df$StudyID, "doi.org/", "")


df$Link <- ifelse(
  substring(df$StudyID, first = 1, last = 3) == "10.",
  paste0("https://doi.org/", df$StudyID),
  paste0("http://www.google.com/search?q=", df$StudyTitle)
)
df$Link <- str_replace_all(df$Link, 'No abstract available', '')
df$Link <- str_replace_all(df$Link, ' ', '+')


df$Scale_colors <- ifelse(df$Type_B == 'Regional', '#FFFFFF', '#000000')
df$Year <- as.numeric(df$Year)
df$StudyID <- c(1:nrow(df))
df <- na.omit(df)

df$lookup <- df$StudyAuthors


server <- function(input, output) {
  
  # Ridge plot year and type
  output$plot1 <- renderPlot({

    # Reactivity
    year <- reactive(input$year)
    type <- reactive(input$type_A)
    freetext <- reactive(input$freeText)
    author <- reactive(input$author)

    if (length(author()) > 0 & length(type()) > 0){
      
      sites <- data.frame()
      for (i in author()){
        tmp <- df %>% filter(grepl(i, StudyAuthors, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      for (i in type()){
        tmp <- sites %>% filter(grepl(i, Type_A, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      sites <- sites %>%
        filter(
          Year %in% c(year()[1]:year()[2]) &
            grepl(freetext(), freeTextLookup, ignore.case = T)
        )
    }
    
    else if (length(author()) > 0 & length(type()) == 0){
      
      sites <- data.frame()
      for (i in author()){
        tmp <- df %>% filter(grepl(i, StudyAuthors, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      sites <- sites %>%
        filter(
          Year %in% c(year()[1]:year()[2]) &
            grepl(freetext(), freeTextLookup, ignore.case = T)
        )
    }
    
    else if (length(author()) == 0 & length(type()) > 0){
      
      sites <- data.frame()
      for (i in type()){
        tmp <- df %>% filter(grepl(i, Type_A, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      sites <- sites %>%
        filter(
          Year %in% c(year()[1]:year()[2]) &
            grepl(freetext(), freeTextLookup, ignore.case = T)
        )
    }
    
    else {
      
      sites <- df %>% filter(
        Year %in% c(year()[1]:year()[2]) &
          grepl(freetext(), freeTextLookup, ignore.case = T)
      )
    }
    
    sites <- unique(sites[ , c(2:7, 9, 10, 11, 13, 14)])
    sites <-  na.omit(sites)
    
    tmp <- df[c('Year', 'Type_A', 'StudyAuthors')]
    tmp <-  na.omit(tmp)

    tmp$Type_A <- ifelse(tmp$Type_A == '', 'Other', tmp$Type_A)
    sites$Type_A <- ifelse(sites$Type_A == '', 'Other', sites$Type_A)
    
    tmp <- cSplit(tmp, "Type_A", sep = ",", direction = "long")
    tmp2 <- cSplit(sites, "Type_A", sep = ",", direction = "long")
    tmp2 <- na.omit(tmp2)
    
    tmp <- data.frame(tmp %>% group_by(Year, Type_A) %>% summarise(count = n()))
    tmp2 <- data.frame(tmp2 %>% group_by(Year, Type_A) %>% summarise(count = n()))

    tmp <- tmp[order(tmp$Type_A, decreasing = T),]
    tmp2 <- tmp2[order(tmp$Type_A, decreasing = T),]
    

    plot1 <- ggplot() +
      geom_density_ridges(data = tmp2,
                          aes(x = Year, y = Type_A),
                          scale = .9,
                          fill = 'red',
                          alpha = 0.3) +
      geom_density_ridges(data = tmp,
                          aes(x = Year, y = Type_A),
                          scale = .9,
                          fill = 'black',
                          alpha = 0.3) +
      theme_bw() +
      xlab('') +
      ylab('') +
      scale_y_discrete(limits = rev(unique(sort(tmp$Type_A)))) +
      ggtitle('\nAdaptation Type Over Time\n') +

      theme(axis.line = element_blank(),
            # axis.text.x=element_blank(),
            # axis.text.y=element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 22),
            # axis.title.x=element_blank(),
            # axis.title.y=element_blank(),
            # legend.position="none",
            # panel.background=element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
            # plot.background=element_blank()
            )

    print(plot1)

  })
  
  # Map output
  output$MapPlot1 <- renderLeaflet({
    
    leaflet() %>% 
      
      # Basemap
      addProviderTiles(
        "Stamen.TerrainBackground",
        options = tileOptions(minZoom = 2,
                              maxZoom = 20,
                              continuousWorld = F
                              )
        ) %>% 
      
      # Add zoom to max button
      addEasyButton(
        easyButton(
          icon = "fa-globe", 
          title = "Zoom to Full Map",
          onClick = JS("function(btn, map){ map.setZoom(1); }"
                     )
          )
        ) %>%
      
      # Set zoom
      setView(
        lng = 0, 
        lat = 0, 
        zoom = 2
        ) %>%
      
      addSearchOSM(
        searchOptions(
          zoom = 6,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE,
          textCancel = TRUE
          )
        ) %>%
      
      # addSearchGoogle(
      #   apikey = 'AIzaSyB0NqMRf7PoB7KOqz3aGL06S6KYn41q0dQ',
      #   searchOptions(
      #     zoom = 6,
      #     autoCollapse = TRUE, 
      #     hideMarkerOnCollapse = TRUE
      #   )
      # ) %>%
      
      # Add legend
      addLegend('bottomleft', 
                colors = c('#FFFFFF', '#000000'), 
                labels = c('Regional', 'Local'),
                values = c('Regional', 'Local'),
                title = 'Scale of Study',
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
                )
    
  })
  
  # Map clear marker clusters when year is changed
  observeEvent(
    reactive(input$year), {
      leafletProxy("MapPlot1") %>% clearMarkerClusters() #%>% removeSearchOSM()
    })
  
  # Map clear marker clusters when type is changed
  observeEvent(
    reactive(input$type_A), {
      leafletProxy("MapPlot1") %>% clearMarkerClusters() #%>% removeSearchOSM()
    })
  
  # Map clear marker clusters when type is changed
  observeEvent(
    reactive(input$freetext), {
      leafletProxy("MapPlot1") %>% clearMarkerClusters() #%>% removeSearchOSM()
    })
  
  # Map clear marker clusters when author is changed
  observeEvent(
    reactive(input$author), {
      leafletProxy("MapPlot1") %>% clearMarkerClusters() #%>% removeSearchOSM()
    })
  
  observe({
    
    # Reactivity for map content
    leafletProxy("MapPlot1") %>% clearMarkerClusters() #%>% removeSearchOSM()
    
    year <- reactive(input$year)
    type <- reactive(input$type_A)
    freetext <- reactive(input$freeText)
    author <- reactive(input$author)
    
    if (length(author()) > 0 & length(type()) > 0){
      
      sites <- data.frame()
      for (i in author()){
        tmp <- df %>% filter(grepl(i, StudyAuthors, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      for (i in type()){
        tmp <- sites %>% filter(grepl(i, Type_A, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      sites <- sites %>%
        filter(
          Year %in% c(year()[1]:year()[2]) &
            grepl(freetext(), freeTextLookup, ignore.case = T)
        )
    }
    
    else if (length(author()) > 0 & length(type()) == 0){
      
      sites <- data.frame()
      for (i in author()){
        tmp <- df %>% filter(grepl(i, StudyAuthors, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      sites <- sites %>%
        filter(
          Year %in% c(year()[1]:year()[2]) &
            grepl(freetext(), freeTextLookup, ignore.case = T)
        )
    }
    
    else if (length(author()) == 0 & length(type()) > 0){
      
      sites <- data.frame()
      for (i in type()){
        tmp <- df %>% filter(grepl(i, Type_A, ignore.case = T))
        sites <- rbind(sites, tmp)
      }
      
      sites <- sites %>%
        filter(
          Year %in% c(year()[1]:year()[2]) &
            grepl(freetext(), freeTextLookup, ignore.case = T)
        )
    }
    
    else {
      
      sites <- df %>% filter(
        Year %in% c(year()[1]:year()[2]) &
          grepl(freetext(), freeTextLookup, ignore.case = T)
      )
    }
    
    
    sites <- unique(sites[ , c(3:7, 9, 13, 14, 15)])
    sites <-  na.omit(sites)
    
    content <- paste0(
      "<b>Location:</b> ",  sites$StudyLoc, "<br/>",
      "<b>Title: </b>",     sites$StudyTitle, "<br/>",
      "<b>Author(s): </b>", sites$StudyAuthors, "<br/>",
      "<b>Year: </b>",      sites$Year, "<br/>",
      "<b>DOI: </b>",       paste0(
                                "<a href = '", 
                                sites$Link, 
                                "' target='_blank'>Link to Article</a>"
                                ), 
      "<br/><br/>",
      "<b>Abstract:</b><br/>",  sites$Abstract, "<br/>"
    )

    leafletProxy("MapPlot1") %>%
      
      # clearMarkers() %>%
      addCircleMarkers(
        lng     = sites$longs,
        lat     = sites$lats,
        popup   = content,
        color   = sites$Scale_colors,
        clusterOptions = markerClusterOptions(showCoverageOnHover = F),
        clusterId = "cluster1"
        )

  })
}




##################### TEST #####################
