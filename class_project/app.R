#look at nas for girl height n stuff

library(shiny)
library(ggplot2)
library(readxl)
library(rvest)
library(tidyverse)
library(gridExtra)
library(stringr)
library(plotly)
library(DT)

## 

source("clean_data.R")

ui <- fluidPage(navbarPage(
  h2(
    "Produce 101",
    style = "font-family: 'Lobster', cursive;
    font-weight: 500; line-height: 0.7;
    color: #505050;"
  ),
  navbarMenu(
    h4(
      "About",
      style = "font-family: 'Lobster', cursive;
      font-weight: 500; line-height: 1.1;
      color: #505050;"
    ),
    tabPanel(
      h5(
        "The website",
        style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.5; color: #505050;"
      ),
      tags$img(src = "produce101_s1_s2_long.jpg", height = 180),
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
  navbarMenu(
    h4(
      "Trainee Info",
      style = "font-family: 'Lobster', cursive;
      font-weight: 500; line-height: 1.1;color: #505050;"
    ),
    tabPanel(
      h5(
        "Season 1 trainees",
        style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.7; color: #505050;"
      ),
      plotOutput(
        "plot_height_age_s1",
        height = 500,
        click = "plot1_click",
        brush = brushOpts(id = "plot1_brush")
      ),
      verbatimTextOutput("click_info_s1"),
      verbatimTextOutput("brush_info_s1"),
      DT::dataTableOutput("pd101_s1_trainee")
    ),
    tabPanel(
      h5(
        "Season 2 trainees",
        style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.7; color: #505050;"
      ),
      plotOutput(
        "plot_height_age_s2",
        height = 500,
        click = "plot1_click",
        brush = brushOpts(id = "plot1_brush")
      ),
      verbatimTextOutput("click_info_s2"),
      verbatimTextOutput("brush_info_s2"),
      DT::dataTableOutput("pd101_s2_trainee")
    ),
    tabPanel(
      h5(
        "Full trainee list",
        style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.7; color: #505050;"
      ),
      DT::dataTableOutput("df")
    )
  ),
  #buttons, get variable
  #https://rstudio.github.io/DT/003-tabletools-buttons.html
  navbarMenu(
    h4(
      "Data",
      style = "font-family: 'Lobster', cursive;
      font-weight: 500; line-height: 1.1;
      color: #505050;"
    ),
    tabPanel(
      h5(
        "Explore",
        style = "font-family: 'Didot', cursive;
        font-weight: 500; line-height: 0.6;
        color: #505050;"
      ),
      selectInput("var1_exp", "Choose variable",
                  names(df)[c(3, 22, 25, 26)]),
      plotlyOutput("exploreplot1"),
      verbatimTextOutput("mean1"),
      verbatimTextOutput("mean2"),
      
      selectInput("var1_exp2", "Choose variable",
                  names(df)[c(22, 25, 26)]),
      selectInput("binnum_exp2", "Choose number of bins",
                  c(1, 2, 3)),
      plotOutput("exploreplot3")
      
      #selectInput("var2_exp", "Choose x variable",
      #names(df)),
      #selectInput("var3_exp", "Choose y variable",
      #names(df)), plotlyOutput("exploreplot2")
      
      
    ),
    #http://rstudio.github.io/shiny/tutorial/#building-inputs
    #display average
    tabPanel(
      h5("Age", style = "font-family: 'Didot', cursive; font-weight: 500; line-height: 0.6;
         color: #505050;"),
      verbatimTextOutput("agetext1"),
      plotlyOutput("plotage_s12"),
      verbatimTextOutput("agetext2"),
      plotOutput("plotage")
    ),
    tabPanel(
      h5("Height", style = "font-family: 'Didot', cursive; font-weight: 500; line-height: 0.6;
         color: #505050;")
    ),
    tabPanel(
      h5("Training length", style = "font-family: 'Didot', cursive; font-weight: 500; line-height: 0.6;
         color: #505050;")
    )
  )
  # h4("sdfsdf")
  # h2("sdfsdf")
  # tags$li("sdf")
  # includeMarkdown()
))


server <- function(input, output) {
  output$dimension_display <- renderText({
    paste(input$dimension[1],
          input$dimension[2],
          input$dimension[2] / input$dimension[1])
  })
  output$imagee <- renderImage({
    list(
      src = "www/produce101_s1_s2.jpg",
      contentType = "image/jpg",
      height = 500,
      alt = "Pic"
    )
  })
  
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
  output$click_info_s1 <- renderPrint({
    nearPoints(pd101_s1_height_age, input$plot1_click, addDist = TRUE)
  })
  output$brush_info_s1 <- renderPrint({
    brushedPoints(pd101_s1_height_age,
                  input$plot1_brush)
  })
  output$plot_height_age_s2 <- renderPlot({
    ggplot(pd101_s2_height_age, aes(Age, Height)) +
      geom_hex(aes(color = Ranking_final))
  })
  output$click_info_s2 <- renderPrint({
    nearPoints(pd101_s2_height_age, input$plot1_click, addDist = TRUE)
  })
  output$brush_info_s2 <- renderPrint({
    brushedPoints(pd101_s2_height_age,
                  input$plot1_brush)
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
  output$agetext1 <-
    renderText({
      "Left: distribution of age for Season 1 (female trainees).
      Average age: 19.5
      Right: distribution of age for Season 2 (male trainees).
      Average age: 21.8"
    })
  output$agetext2 <-
    renderText({
      "Comparison of age between female trainees and male trainees.
      - Female trainees are generally younger"
    })
  output$mean1 <-
    renderText({
      paste(
        "Average",
        str_to_lower(input$var1_exp, locale = "en"),
        "of female trainees: ",
        mean(pd101_s1_trainee[, input$var1_exp])
      )
    }) #, na.rm = T
  output$mean2 <-
    renderText({
      paste(
        "Average",
        str_to_lower(input$var1_exp, locale = "en"),
        "of male trainees: ",
        mean(pd101_s2_trainee[, input$var1_exp])
      )
    })
  output$plotage_s12 <- renderPlotly({
    p1 <- plot_ly(data = pd101_s1_trainee) %>%
      add_histogram(
        x = ~ Age,
        color = ~ Gender,
        mode = "markers",
        marker = list(color = "#ff82ab")
      )
    p2 <- plot_ly(data = pd101_s2_trainee) %>%
      add_histogram(
        x = ~ Age,
        color = ~ Gender,
        mode = "markers",
        marker = list(color = "#5cacee")
      )
    subplot(p1, p2)
  })
  output$exploreplot1 <- renderPlotly({
    p1 <-
      plot_ly(
        df,
        x = ~ df[, input$var1_exp],
        color = ~ Gender,
        type = "box",
        colors = c("#ff82ab", "#5cacee")
      )
    p2 <- plot_ly() %>%
      #https://plot.ly/r/reference/
      add_histogram(
        x = ~ pd101_s1_trainee[, input$var1_exp],
        color = ~ pd101_s1_trainee$Gender,
        mode = "markers",
        marker = list(color = "#ff82ab")
      ) %>%
      add_histogram(
        x = ~ pd101_s2_trainee[, input$var1_exp],
        color = ~ pd101_s2_trainee$Gender,
        mode = "markers",
        marker = list(color = "#5cacee")
      ) %>%
      layout(barmode = "group")
    subplot(p1, p2)
    #https://plot.ly/ipython-notebooks/color-scales/
  })
  
  output$exploreplot3 <- renderPlot({
    ggplot(df, aes(Age, df[, input$var1_exp2])) +
      geom_boxplot(aes(group = cut_width(
        Age, (range(Age)[2] - range(Age)[1]) / as.numeric(input$binnum_exp2)
      ))) +
      facet_wrap( ~ as.factor(Gender), nrow = 1) +
      ylab(input$var1_exp2)
  })
  
  output$exploreplot2 <- renderPlot({
    plot_ly(df, x = ~ df[, input$var2_exp], y = ~ df[, input$var3_exp])
    #+
    #geom_jitter(aes(color = Gender))
    #+
    #xlab(input$var2_exp) +
    #ylab(input$var3_exp) +
    #scale_color_manual(values=c("#ff82ab", "#5cacee"))
  })
  
  output$plotage <- renderPlot({
    ggplot(df) +
      geom_freqpoly(mapping = aes(x = Age, color = Gender)) +
      scale_color_manual(values = c("#ff82ab", "#5cacee"))
  })
  #add hover
}

# Run the application
shinyApp(ui = ui, server = server)
