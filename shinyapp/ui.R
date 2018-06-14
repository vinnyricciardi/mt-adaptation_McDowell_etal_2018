library(shiny)
library(leaflet)
library(shinydashboard)
library(markdown)
library(shinyjs)

# Read data
df <- read.csv("data.csv", stringsAsFactors = FALSE)
# df <- unique(df[ , 2:13])

sum_txt <- "### <left>Summary</left>
Climate change is disrupting the lives of the 915 million people living in mountain areas as well as the socio-ecological relationships that sustain livelihoods in fragile mountain environments. This map shows a recent effort to synthesize the scientific literature on how people in mountain regions are adapting. 170 articles documenting over 690 discrete adaptations are represented. Explore the map, links to the literature, and the different types of methods used in this body of literature. The underlying data and accompanying article can be found here. 
<br><br>McDowell, G., Huggel, C., Frey, H., Wang, F., Cramer, R., Ricciardi, V. (in review) Adaptation action and research in glaciated mountain systems: Are they enough to meet the challenge of climate change?
"

tmp <- df['StudyAuthors']

author_list <- sort(unique(na.omit(df$StudyAuthors)))


header <- dashboardHeader(
  title = "Adaptation action and research in glaciated mountain systems",
  titleWidth = "100%"
)

body <- dashboardBody(

  tags$head(
    tags$style(
      HTML('
        .main-header .logo {
          font-size: 24px;
          text-align: left;
        }
        .content-wrapper,
        .right-side {
           background-color: #ffffff;
        }
        .box.box-warning{
          border-top-color: #ffffff;
        }
        .box {
	        -webkit-box-shadow: none;
          -moz-box-shadow: none;
          box-shadow: none;
          border-top: #ffffff;
        }
        .header main-header{
          border-right: #ffffff;
          border-bottom: #ffffff;
        }
        .shiny-output-error{
          visibility: hidden;
        }
        .shiny-output-error:before{
          visibility: hidden;
        }
           ')
      )
    ),
  
  fluidRow(
    
    column(
      width = 9,
      box(
        width = NULL, 
        solidHeader = FALSE,
        leafletOutput(
          "MapPlot1", 
          height = 700
          )
        )
      ),
    
    column(
      width = 3,
      box(
        width = NULL, 
        solidHeader = FALSE,
        status = "warning",
        uiOutput("summaryText"),
        HTML(
          markdownToHTML(
            fragment.only=TRUE,
            text=c(sum_txt)
            )
          )
        )
    ),
    
    column(
      width = 3,
      tabsetPanel(type = "tabs",
                  tabPanel("Search", 
                           textInput("freeText", 
                                     label = h5("For location use the magnifying glass on the map."),
                                     placeholder = 'Keywords, titles, authors, methods, etc.',
                                     value = ""
                                     )
                           ),
                  
                  tabPanel("Advanced Search", 
                           # Year select
                           sliderInput(
                             "range",
                             inputId = "year",
                             label   = "Select year(s)",
                             min     = min(df$Year, na.rm = T),
                             max     = max(df$Year, na.rm = T),
                             value   = c(2000, 2018),
                             step    = 1,
                             sep     = '',
                             ticks   = F),
                           
                           # Author select
                           selectInput(
                             inputId  = "author",
                             label    = "Select author(s)",
                             choices  = author_list,
                             selected = "",
                             multiple = T
                           ),
                           
                           # Type A select
                           selectInput(
                             inputId  = "type_A",
                             label    = "Select adaptation type(s)",
                             choices  = sort(unique(unlist(strsplit(unique(df$Type_A), ',')))),
                             selected = "",
                             multiple = T
                             ),
                           
                           # Type B select
                           selectInput(
                             inputId  = "type_B",
                             label    = "Select scale of adaptation",
                             choices  = sort(unique(unlist(strsplit(unique(df$Type_B), ',')))),
                             selected = "",
                             multiple = T
                           ),
                           
                           # Type C select
                           selectInput(
                             inputId  = "type_C",
                             label    = "Select status of adaptation",
                             choices  = sort(unique(unlist(strsplit(unique(df$Type_C), ',')))),
                             selected = "",
                             multiple = T
                           )
                  )
      )
    ),
    
    column(
      width = 3,
      plotOutput("plot1", height = "350px")
      ),
    
    column(
      width = 12,
      tags$a(
        href = "https://github.com/vinnyricciardi/mt-adaptation_McDowell_etal_2018", 
        "Fork our GitHub repo to make your own map",
        target = "_blank"
      )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,
  skin = 'black' 
)







## This is a ui for a whole screen version, but it's buggy and overlaps
# ui <- bootstrapPage(
#   div(class="outer",
# 
#   useShinyjs(),
# 
#   # Set body css to make map background
#   # tags$style(
#   #   type = "text/css",
#   #          "html,
#   #           body {width:100%;height:100%}
#   #           #input_control {background-color: #FFFFFF}"
#   #   ),
#   tags$style(
#     type = "text/css", 
#     ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"
#     ),
# 
#   # Plot map
#   
#   leafletOutput(
#     "MapPlot1",
#     width  = "80%",
#     height = "100%"
#     ),
# 
#   absolutePanel(
#     top = 10,
#     right = 30,
#     width = 300,
#     height = "90%",
#     fixed = T,
#     div(
#       style="padding: 8px;
#       background: #FFFFFF;",
#       HTML(
#         markdownToHTML(
#           fragment.only=TRUE,
#           text=c(strrep("<br>", 35))
#           )
#         )
#       )
#     ),
# 
#   absolutePanel(
#     top = 10,
#     right = 30,
#     width = 300,
#     height = "90%",
#     fixed = T,
#     div(
#       style="padding: 8px;
#       background: #FFFFFF;",
#       tabsetPanel(
#         tabPanel(
#           "Summary",
#           HTML(
#             markdownToHTML(
#               fragment.only=TRUE,
#               text=c(sum_txt)
#               )
#             )
#           ),
#         
#         tabPanel("Plots",
#                  plotOutput('plot1', height="200px"),
#                  plotOutput('plot2', height="200px"),
#                  plotOutput('plot3', height="200px")),
#         
#         # Year slider
#         sliderInput(
#           "range",
#           inputId = "year",
#           label   = "Year and Type of Study",
#           min     = 2000,
#           max     = 2018,
#           value   = c(2000, 2018),
#           step    = 1,
#           sep     = '',
#           ticks   = F),
#         
#         # Type select
#         selectInput(inputId  = "type_B",
#                     label    = "",
#                     choices  = sort(unique(df$Type_B)),
#                     selected = sort(unique(df$Type_B)),
#                     multiple = T)
#         
#       )
#     )
#   )
# )
# )
