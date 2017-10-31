library(ggplot2)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(
  fluidRow(
    column(width = 8, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 6,
                    plotOutput("plot1", height = 300,
                               click = "plot_click",
                               dblclick = "plot_dblclick",
                               hover = "plot_hover",
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 6,
                    plotOutput("plot2", 
                               height = 300)
             )
           )
    ),
    verbatimTextOutput("info")
    
  ),
  sidebarPanel(
    selectInput("var1", "Choose x variable", names(mpg)[c(3,4,5,8,9)]),
    selectInput("var2", "Choose y variable", names(mpg)[c(3,4,5,8,9)])
  )
  
)

server <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(mpg) +
      geom_point(aes(x = mpg[,input$var1], 
                     y = mpg[,input$var2])) +
      xlab(input$var1) +
      ylab(input$var2)
    
  })
  
  output$plot2 <- renderPlot({
    ggplot(mpg) +
      geom_point(aes(x = mpg[,input$var1], 
                     y = mpg[,input$var2])) +
      xlab(input$var1) +
      ylab(input$var2)+
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  #delete
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
}

shinyApp(ui, server)