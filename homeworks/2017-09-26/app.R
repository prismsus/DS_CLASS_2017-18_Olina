library(shiny)
library(ggplot2)
library(readxl)
library(rvest)
library(tidyverse)
library(gridExtra)
library(stringr)

#data
#s1
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_Produce_101_contestants")
tbls_s1 <- webpage %>%
  html_nodes("table")%>%
  .[1] %>%
  html_table(fill = TRUE)
#table 1
pd101_s1_ <- tbls_s1[[1]]
pd101_s1_$Company <- str_replace_all(pd101_s1_$Company, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s1_$Name <- str_replace_all(pd101_s1_$Name, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "") 
pd101_s1_$Name <- gsub("[[:digit:]]", "", pd101_s1_$Name)
names(pd101_s1_) <-  make.names(names(pd101_s1_), unique = T)
pd101_s1_tbl1 <- 
  filter(pd101_s1_, Age>10&Age<50) %>%
  mutate(Age = type.convert(Age),
         Ranking = type.convert(Ranking),
         Judges.evaluation = type.convert(Judges.evaluation),
         evaluation1_num = as.numeric(Judges.evaluation),
         Judges.evaluation.1 = type.convert(Judges.evaluation.1),
         evaluation2_num = as.numeric(Judges.evaluation.1)) %>%
  mutate(Name = str_trim(Name))
#s2
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_Produce_101_Season_2_contestants")
tbls_s2 <- webpage %>%
  html_nodes("table")%>%
  .[1] %>%
  html_table(fill = TRUE)
#table 1
pd101_s2_ <- tbls_s2[[1]]
pd101_s2_$Company <- str_replace_all(pd101_s2_$Company, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s2_$Name <- str_replace_all(pd101_s2_$Name, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "") 
pd101_s2_$Name <- gsub("[[:digit:]]", "", pd101_s2_$Name)
names(pd101_s2_) <-  make.names(names(pd101_s2_), unique = T)
pd101_s2_tbl1 <- 
  filter(pd101_s2_, Age>10&Age<50) %>%
  mutate(Age = type.convert(Age),
         Ranking = type.convert(Ranking),
         Judges.evaluation = type.convert(Judges.evaluation),
         evaluation1_num = as.numeric(Judges.evaluation),
         Judges.evaluation.1 = type.convert(Judges.evaluation.1),
         evaluation2_num = as.numeric(Judges.evaluation.1)) %>%
  mutate(Name = str_trim(Name))
pd101_s2_height <- read_csv("pd101_heights.csv")
pd101_s2_tbl1 <- pd101_s2_tbl1 %>% 
  full_join(pd101_s2_height, by = "Name")

pd101_s2_height_age <- pd101_s2_tbl1 %>%
  select(Name, Company, Age, Height, Ranking, Judges.evaluation)


ui <- fluidPage(
  
  navbarPage("Produce 101",
             navbarMenu("Introduction",
                        tabPanel("About Produce 101", 
                                 verbatimTextOutput("Introduction"),
                                 tags$img(src="produce101_s1_s2.jpg")
                                 #tags$img(src="season1.jpg"),
                                 #tags$img(src="season2.jpg")
                                 ),
                        tabPanel("Videos", verbatimTextOutput("video1"),
                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/BiorIyrjTHc" frameborder="0" allowfullscreen></iframe>'),
                                 verbatimTextOutput("video2"),
                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NIld_iEc67s" frameborder="0" allowfullscreen></iframe>')),
                        tabPanel("Others")),
             navbarMenu("Trainee Info",
                        tabPanel("Plot height vs. age",
                                 plotOutput("plot_height_age", height = 500,
                                            click = "plot1_click",
                                            brush = brushOpts(
                                              id = "plot1_brush"
                                            )),
                                 verbatimTextOutput("click_info"),
                                 verbatimTextOutput("brush_info")),
                        tabPanel("Season 1 trainee list", tableOutput("pd101_s1_tbl1")),
                        tabPanel("Season 2 trainee list", tableOutput("pd101_s2_tbl1"))),
             navbarMenu("Data",
                        tabPanel("Age", 
                                 verbatimTextOutput("agetext1"), plotOutput("plotage_s1"),
                                 verbatimTextOutput("agetext2"), plotOutput("plotage_s2"),
                                 verbatimTextOutput("agetext3"), plotOutput("plotage")),
                        tabPanel("Height"),
                        tabPanel("Training length"))
  )
)


server <- function(input, output) {
  #intro
  output$Introduction <- renderText({ "Produce 101 is a korean survival reality show on Mnet. 
It is a large-scale project in which the public (called 'national producers') \"produces\" a unit group by choosing 11 members among 101 trainees from over 50 entertainment companies.
The show has 11 episodes.
In the first two episodes, the trainees' are ranked by judges from A to E based on their perfromance which determines their position in the first trailor(see \"video\" tab).
And in each episode, their ranking are calculated by votings by viewers, the bottom ones are eliminated. 
Season 1 consist of all girls while season 2 is all boys.
    
This project is to explore which kind of trainees are more likely to debut." })
  #Introduction_videos
  output$video1 <- renderText({"Season 1 trailor"})
  output$video2 <- renderText({"Season 2 trailor"})
  #Trainee Info_plot
  output$plot_height_age <- renderPlot({
    ggplot(pd101_s2_height_age, aes(Age, Height)) + 
      geom_point(aes(color = Ranking))
  })
  output$click_info <- renderPrint({
    nearPoints(pd101_s2_height_age, input$plot1_click, addDist = TRUE)
  })
  output$brush_info <- renderPrint({
    brushedPoints(pd101_s2_height_age, 
                  input$plot1_brush)
  })
  #Trainee info_graphs
  output$pd101_s1_tbl1 <- renderTable({
    pd101_s1_tbl1 %>%
      arrange(Ranking)
  })
  output$pd101_s2_tbl1 <- renderTable({
    pd101_s2_tbl1 %>%
      arrange(Ranking)
  })
  #Data comparison_age
  output$agetext1 <- renderText({"Distribution of age for Season 1 (female trainees).
    Average age:"})
  output$agetext2 <- renderText({"Distribution of age for Season 2 (male trainees).
    Average age: 21.8"})
  output$agetext3 <- renderText({"Comparison of age between female trainees(pink) and male trainees(blue)."})
  output$plotage_s1 <- renderPlot({
    ggplot(data = pd101_s1_tbl1) +
      geom_histogram(mapping = aes(x = Age), bins = 10, color = "palevioletred1")
  })
  output$plotage_s2 <- renderPlot({
    ggplot(data = pd101_s2_tbl1) +
      geom_histogram(mapping = aes(x = Age), bins = 10, color = "steelblue2")
  })
  output$plotage <- renderPlot({
    ggplot() +
      geom_freqpoly(data = pd101_s1_tbl1, mapping = aes(x = Age), color = "palevioletred1") +
                      geom_freqpoly(data = pd101_s2_tbl1, mapping = aes(x = Age), color = "steelblue2")
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
