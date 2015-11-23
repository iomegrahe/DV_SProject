# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {
  
  #############Code for Scatterplot#############
  
  KPI_Low_Max_value <- reactive({input$KPI1})
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  labelsize2 <- reactive({input$labelsize2})
  labelsize3 <- reactive({input$labelsize3})
  
  output$Scatterplot <- renderPlot(height=reactive({input$height1}), width=reactive({input$width1}),{
    
    df1 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from males"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_continuous() +
      scale_y_continuous() +
      labs(title='Males education and wage for each occupation') +
      labs(x="SCHOOL", y=paste("WAGE")) +
      layer(data=df1, 
            mapping=aes(x=as.numeric(as.character(SCHOOL)), y=as.numeric(as.character(WAGE)), color=OCCUPATION), 
            stat="identity", 
            stat_params=list(), 
            geom="point",
            geom_params=list(), 
            position=position_jitter(width=0.3, height=.01)
      )
    
    # End your code here.
    plot
  }) # output$distPlot
  
  #############Code for Crosstab#############
  
  df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select OCCUPATION, ETHN, ROUND(average_exper, 2) as AVERAGE_EXPER, INDUSTRY, KPI as RATIO,     
                                                                                 case
                                                                                 when kpi < "p1" then \\\'03 Low\\\'
                                                                                 when kpi < "p2" then \\\'02 Medium\\\'
                                                                                 else \\\'01 High\\\'
                                                                                 end kpi
                                                                                 from (select OCCUPATION, ETHN, AVG(EXPER) as average_exper, INDUSTRY, 
                                                                                 AVG(EXPER) as KPI
                                                                                 from Males
                                                                                 group by INDUSTRY, ETHN, OCCUPATION);"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value(), p2=KPI_Medium_Max_value()), verbose = TRUE)))
  })
  
  output$Crosstab <- renderPlot(height=reactive({input$height2}), width=reactive({input$width2}),{             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      facet_wrap(~OCCUPATION, ncol=1) +
      labs(title='Average experience broken down by industry, occupation, and ethnicity') +
      labs(x=paste("INDUSTRY"), y=paste("OCCUPATION/ETHNICITY")) +
      layer(data=df2(), 
            mapping=aes(x=INDUSTRY, y=ETHN, label=round(AVERAGE_EXPER,2)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", size=labelsize2()), 
            position=position_identity()
      ) +
      layer(data=df2(), 
            mapping=aes(x=INDUSTRY, y=ETHN, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=rv$alpha), 
            position=position_identity()
      ) +
      scale_fill_manual(values=c("green", "blue", "red")
      )
    plot
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  
  #############Code for Barchart#############
  
  output$Barchart <- renderPlot(width=reactive({input$width3}),{
    
    df3 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                     "select OCCUPATION, HEALTH, AVERAGE_WAGE, AVG(AVERAGE_WAGE) 
                                                     OVER (PARTITION BY HEALTH) as AGG_AVG_WAGE
                                                     from (select OCCUPATION, HEALTH, AVG(WAGE) as AVERAGE_WAGE
                                                     from Males
                                                     group by HEALTH, OCCUPATION) order by OCCUPATION;"
                                                     ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
    
    df3 <- df3 %>% mutate(TOTAL_AVG = mean(AGG_AVG_WAGE))
    
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      facet_wrap(~OCCUPATION, ncol=9) +
      labs(title="Average Wage of Each Occupation, Health problems vs no problems") +
      labs(x=paste("OCCUPATION/HEALTH"), y=paste("AVERAGE WAGE")) +
      layer(data=df3, 
            mapping=aes(x=HEALTH, y=AVERAGE_WAGE, fill=HEALTH), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(alpha=rv$alpha),
            position=position_identity()
      ) +
      layer(data=df3, 
            mapping=aes(x=HEALTH, y=AVERAGE_WAGE, label=round(AVERAGE_WAGE,3)),
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=0.5, vjust = -1, size=labelsize3()), 
            position=position_identity()
      ) +
      layer(data=df3, 
            mapping=aes(yintercept = TOTAL_AVG), 
            geom="hline",
            geom_params=list(colour="BLACK")
      )
    
    # End your code here.
    return(plot)
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  #############Code for Blending#############
  output$Blending <- renderPlot(height=reactive({input$height4}), width=reactive({input$width4}),{
    
    df4 <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select YEAR || \\\'   Avg Wage\\\' as measure_names, avg(WAGE) as measure_values from MALES
group by YEAR
union all
select YEAR || \\\'   Avg Hours Worked\\\' as measure_names, avg(lnhr) as measure_values from LABORSUPPLY
where YEAR > 1979 and YEAR < 1988
group by YEAR
order by 1;"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_continuous() +
      labs(title='Blending MALES and LABORSUPPLY') +
      labs(x=paste("Year"), y=paste("Wage and Hours Worked")) +
      layer(data=df4, 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
            stat="identity", 
            stat_params=list(), 
            geom="bar",
            geom_params=list(fill="orange"), 
            position=position_identity()
      ) + coord_flip() +
      layer(data=df4, 
            mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES,3)), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=1.1), 
            position=position_identity()
      )
    
    # End your code here.
    return(plot)
  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
})
