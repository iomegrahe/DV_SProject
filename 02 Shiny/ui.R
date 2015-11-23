library(shiny)

navbarPage(
  title = "Project 6 - Shiny",
  tabPanel(title = "Scatter Plot",
           sidebarPanel(
             sliderInput("width1", "Width:", min=100, max=2000,  value=650),
             sliderInput("height1", "Height:", min=100, max=2000,  value=500)
           ),
           
           mainPanel(plotOutput("Scatterplot")
           )
  ),
  tabPanel(title = "Crosstab",
           sidebarPanel(
             sliderInput("KPI1", "KPI Low Max value:", 
                         min = 1, max = 15,  value = 7),
             sliderInput("KPI2", "KPI Medium Max value:", 
                         min = 1, max = 15,  value = 10),
             sliderInput("width2", "Width:", min=100, max=2000,  value=650),
             sliderInput("height2", "Height:", min=100, max=2000,  value=500),
             sliderInput("labelsize2","Label Size:", min=1, max=10, value=3),
             actionButton(inputId = "clicks2",  label = "Produce Crosstab")
           ),
           
           mainPanel(plotOutput("Crosstab")
           )
  ),
  tabPanel(title = "Barchart",
           sidebarPanel(
             sliderInput("labelsize3","Label Size:", min=1, max=10, value=3),
             sliderInput("width3", "Width:", min=100, max=2000,  value=650),
             actionButton(inputId = "light", label = "Light"),
             actionButton(inputId = "dark", label = "Dark")
           ),
           
           mainPanel(plotOutput("Barchart")
           )
  ),
  tabPanel(title = "Blending 2 Data Sources",
           sidebarPanel(
             sliderInput("width4", "Width:", min=100, max=2000,  value=600),
             sliderInput("height4", "Height:", min=100, max=2000,  value=300)
           ),
           
           mainPanel(plotOutput("Blending")
           )        
  )
)
