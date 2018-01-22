source("00_libraries.R", local = T)
source("01_clean_data.R", local = T)
options(spinner.color.background="#F5F5F5")

ui <- fluidPage(
  
  navbarPage(  
    h2("Produce 101", style = "font-family: 'Lobster', cursive;
      font-weight: 500; line-height: 0.7; color: #505050;"),
  navbarMenu(
  h4("About", style = "font-family: 'Lobster', cursive;
    font-weight: 500; line-height: 1.1; color: #505050;"),
  tabPanel(
    h5("The website", style = "font-family: 'Didot', cursive;
      font-weight: 500; line-height: 0.5; color: #505050;"),
    #tags$img(src = "produce101_s1_s2_long.jpg", height = 180),
    HTML('<center><img src="produce101_s1_s2_long.jpg" width="1000"></center>'),
    h1(" "),
    includeMarkdown("pd101_text_intro_2.md")
  ),
  tabPanel(
    h5(
      "Kpop",
      style = "font-family: 'Didot', cursive;
      font-weight: 500; line-height: 0.5; color: #505050;"
    ),
    includeMarkdown("pd101_text_intro.md"),
    tags$img(src = "Kpop_Logos.jpg", height = 380)
  ),
  tabPanel(
    h5(
      "Produce 101",
      style = "font-family: 'Didot', cursive;
      font-weight: 500; line-height: 0.5; color: #505050;"
    ),
    includeMarkdown("pd101_text_intro_3.md"),
    tags$img(src = "produce101_s1_s2.jpg", height = 500)
  ),
  tabPanel(
    h5(
      "Videos",
      style = "font-family: 'Didot', cursive;
      font-weight: 500; line-height: 0.5; color: #505050;"
    ),
    verbatimTextOutput("video1"),
    HTML(
      '<iframe width="560" height="315" src="https://www.youtube.com/embed/BiorIyrjTHc" frameborder="0" allowfullscreen></iframe>'
    ),
    verbatimTextOutput("video2"),
    HTML(
      '<iframe width="560" height="315" src="https://www.youtube.com/embed/NIld_iEc67s" frameborder="0" allowfullscreen></iframe>'
    )
  ),
  tabPanel(
    h5(
      "References",
      style = "font-family: 'Didot', cursive;
      font-weight: 500; line-height: 0.5; color: #505050;"
    ),
    includeMarkdown("pd101_text_references.md"),
    h1(" "),
    tags$img(src = "produce101_s1_s2_long_.jpg", height = 200)
  )
),

#source("02_webpage1.R", local = T),

  navbarMenu(
    h4("Trainee Info",style = "font-family: 'Lobster', cursive;
      font-weight: 500; line-height: 1.1;color: #505050;"),
    tabPanel(
      h5("Season 1 trainees",style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.7; color: #505050;"),
      h5("Brush a point to see trainee's information."),
      tabPanel(
        h5("Single variable", style = "font-family: 'Didot', cursive;
           font-weight: 500; line-height: 0.6; color: #505050;"),
        fluidRow(
          column(12, align="center",
                 plotOutput("plot_height_age_s1", width = 600, height = 400,
                            brush = brushOpts(id = "plot1_brush"))
          )
        ),
        hr(),
        verbatimTextOutput("brush_info_s1"),
        h1( ),
        DT::dataTableOutput("pd101_s1_trainee")
        )
      ),
    
    tabPanel(
      h5("Season 2 trainees",style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.7; color: #505050;"),
      h5("Brush a point to see trainee's information."),
      fluidRow(
        column(12, align="center",
               plotOutput("plot_height_age_s2", width = 600, height = 400,
                          brush = brushOpts(id = "plot2_brush"))
        )
      ),
      hr(),
      verbatimTextOutput("brush_info_s2"),
      h1(),
      DT::dataTableOutput("pd101_s2_trainee")
    ),
    tabPanel(
      h5(
        "Full data table(download)",
        style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.7; color: #505050;"
      ),
      DT::dataTableOutput("df")
    )
  ),


  #buttons, get variable
  #https://rstudio.github.io/DT/003-tabletools-buttons.html
  navbarMenu(
    h4("Explore",style = "font-family: 'Lobster', cursive;
      font-weight: 500; line-height: 1.1; color: #505050;"),
    
    tabPanel(
      h5("Single variable", style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.6; color: #505050;"),
      sidebarLayout(
        sidebarPanel(
          selectInput("var1_exp", "Choose variable (x-axis)",
                      names(df)[c(3,22, 25, 26)])
        ),
        mainPanel(
          withSpinner(plotlyOutput(paste0("exploreplot1")),type=7),
          verbatimTextOutput("mean1"),
          verbatimTextOutput("mean2")
        )
      )
    ),
    
    #http://rstudio.github.io/shiny/tutorial/#building-inputs
    tabPanel(
      h5("Correlations", style = "font-family: 'Didot', cursive; font-weight: 500; line-height: 0.6;
         color: #505050;"),
      sidebarLayout(
        sidebarPanel(
          selectizeInput("vars", "Choose x and y variables", names(df)[c(3,4,5,6,22,25,26)], 
                         selected = NULL, multiple = FALSE,
                         options = list(maxItems = 2, placeholder = 'Please select')),
          #https://deanattali.com/blog/advanced-shiny-tips/
          textInput("bin_exp2", "Number of boxes", value = "5", width = NULL, placeholder = NULL)
        ),
        mainPanel(
          verbatimTextOutput("warning"),
          withSpinner(plotOutput(paste0("exploreplot2")),type=7)
        )
      )
    ),
    tabPanel(
      h5("Customize graph", style = "font-family: 'Didot', cursive; font-weight: 500; line-height: 0.6;
         color: #505050;"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("graphtype", "Choose graph type",c("Scatter plot", "Boxplot")),
          selectInput("exp3_var1", "Choose x variable",names(df)[c(3,6:20,22,25,26)]),
          selectInput("exp3_var2", "Choose y variable",names(df)[c(3,6:20,22,25,26)]),
          checkboxGroupInput("g_options", "",
                             choiceNames =
                               list("Linear model", "Seperated by gender"),
                             choiceValues =
                               list("lm", "sg")
          )
        ),
        #x&y, plot type, cuztomize, equation, model, filter
        mainPanel(
          verbatimTextOutput("error_plot3"),
          plotOutput("exploreplot3")
        )
      )
    )
  )
))


server <- function(input, output) {
  
  #Introduction_videos
  output$video1 <- renderText({
    "Season 1 trailor"
  })
  output$video2 <- renderText({
    "Season 2 trailor"
  })
  #Trainee Info_plot
  output$plot_height_age_s1 <- renderPlot({
    ggplot(pd101_s1_height_age, aes(Age, Height)) +
      geom_hex(aes(color = Ranking_final))
  })
  output$brush_info_s1 <- renderPrint({
    brushedPoints(pd101_s1_height_age,
                  input$plot1_brush)
  })
  output$plot_height_age_s2 <- renderPlot({
    ggplot(pd101_s2_height_age, aes(Age, Height)) +
      geom_hex(aes(color = Ranking_final))
  })
  output$brush_info_s2 <- renderPrint({
    brushedPoints(pd101_s2_height_age,
                  input$plot2_brush)
  })
  #Trainee info_tables
  output$pd101_s1_trainee <- DT::renderDataTable({
    pd101_s1_trainee %>%
      arrange(Ranking_final)
  },
  options = list(scrollX = TRUE))
  output$pd101_s2_trainee <- DT::renderDataTable({
    pd101_s2_trainee %>%
      arrange(Ranking_final)
  },
  options = list(scrollX = TRUE))
  
  output$df <- DT::renderDataTable({
    df %>%
      arrange(Company)
  },
  extensions = 'Buttons',
  options = list(
    lengthMenu = list(c(10,-1), c("10", "All")),
    pageLength = 10,
    scrollX = TRUE,
    dom = 'lfrtipB',
    buttons =
      list(
        'copy',
        'print',
        list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )
      )
    
  ))
  
  #Data comparison_age
  source("02_plots.R", local = T)
  #add hover
}

# Run the application
shinyApp(ui = ui, server = server)

#look at standard fonts chrome/safari have...
#user enter equation
#delete click for graphs
#seperate into r files
#add description of variables
#add more variables in explore
#https://shiny.rstudio.com/articles/layout-guide.html


#filter data
