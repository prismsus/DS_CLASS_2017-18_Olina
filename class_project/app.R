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

#get data from web
#s1
webpage1 <- read_html("https://en.wikipedia.org/wiki/List_of_Produce_101_contestants")
#table 1
tbls_s1 <- webpage1 %>%
  html_nodes("table")%>%
  .[1] %>%
  html_table(fill = TRUE)
#s2
webpage2 <- read_html("https://en.wikipedia.org/wiki/List_of_Produce_101_Season_2_contestants")
#table 1
tbls_s2 <- webpage2 %>%
  html_nodes("table")%>%
  .[1] %>%
  html_table(fill = T)
#s1
pd101_s1_trainee <- tbls_s1[[1]]
#clean korean characters
pd101_s1_trainee$Company <- str_replace_all(pd101_s1_trainee$Company, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s1_trainee$Name <- str_replace_all(pd101_s1_trainee$Name, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "") 
pd101_s1_trainee$Name <- gsub("[[:digit:]]", "", pd101_s1_trainee$Name)
names(pd101_s1_trainee) <-  make.names(names(pd101_s1_trainee), unique = T)
#clean first few rows
pd101_s1_trainee <- 
  filter(pd101_s1_trainee, Age>10&Age<50) %>%
  #convert types
  mutate(Age = type.convert(Age), 
         Ranking = type.convert(Ranking),
         Judges.evaluation = type.convert(Judges.evaluation),
         evaluation1_num = as.numeric(Judges.evaluation),
         Judges.evaluation.1 = type.convert(Judges.evaluation.1),
         evaluation2_num = as.numeric(Judges.evaluation.1)-1, 
         Ranking = as.numeric(Ranking), #ep1 ranking
         Ranking.1 = as.numeric(Ranking.1),#ep2 ranking
         Ranking.12 = as.numeric(Ranking.12),
         Gender = "F") 
#delete empty space after names
pd101_s1_trainee <- pd101_s1_trainee %>%
  mutate(Name = str_trim(Name))
#rename variables
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking"] <- "Ranking_ep1"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.1"] <- "Ranking_ep2"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.2"] <- "Ranking_ep3"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.3"] <- "Ranking_ep5"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.4"] <- "Votes_ep5"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.5"] <- "Ranking_ep6"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.6"] <- "Ranking_ep8"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.7"] <- "Votes_ep8"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.8"] <- "Ranking_ep10"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.9"] <- "Votes_ep10"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.10"] <- "Ranking_ep11"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.11"] <- "Votes_ep11"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Ranking.12"] <- "Ranking_final"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Judges.evaluation"] <- "Evaluation1"
colnames(pd101_s1_trainee)[colnames(pd101_s1_trainee)=="Judges.evaluation.1"] <- "Evaluation2"
#s2
pd101_s2_trainee <- tbls_s2[[1]]
#clean korean characters
pd101_s2_trainee$Company <- str_replace_all(pd101_s2_trainee$Company, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "")
pd101_s2_trainee$Name <- str_replace_all(pd101_s2_trainee$Name, "[^[:ascii:]]", "") %>%
  str_replace_all("\\(.*\\)", "") 
pd101_s2_trainee$Name <- gsub("[[:digit:]]", "", pd101_s2_trainee$Name)
names(pd101_s2_trainee) <-  make.names(names(pd101_s2_trainee), unique = T)
#clean first few rows
pd101_s2_trainee <- 
  filter(pd101_s2_trainee, Age>10&Age<50) %>%
  #convert types
  mutate(Age = type.convert(Age), 
         Ranking = type.convert(Ranking),
         Judges.evaluation = type.convert(Judges.evaluation),
         evaluation1_num = as.numeric(Judges.evaluation),
         Judges.evaluation.1 = type.convert(Judges.evaluation.1),
         evaluation2_num = as.numeric(Judges.evaluation.1), 
         Ranking = as.numeric(Ranking), #ep1 ranking
         Ranking.1 = as.numeric(Ranking.1),#ep2 ranking
         Ranking.12 = as.numeric(Ranking.12),
         Gender = "M") 

#delete empty space after names
pd101_s2_trainee <- pd101_s2_trainee %>%
  mutate(Name = str_trim(Name))
#rename variables
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking"] <- "Ranking_ep1"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.1"] <- "Ranking_ep2"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.2"] <- "Ranking_ep3"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.3"] <- "Ranking_ep5"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.4"] <- "Votes_ep5"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.5"] <- "Ranking_ep6"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.6"] <- "Ranking_ep8"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.7"] <- "Votes_ep8"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.8"] <- "Ranking_ep10"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.9"] <- "Votes_ep10"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.10"] <- "Ranking_ep11"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.11"] <- "Votes_ep11"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Ranking.12"] <- "Ranking_final"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Judges.evaluation"] <- "Evaluation1"
colnames(pd101_s2_trainee)[colnames(pd101_s2_trainee)=="Judges.evaluation.1"] <- "Evaluation2"
#combine
pd101_s1_heights <- read_csv("pd101_heights_s1.csv")
pd101_s2_heights <- read_csv("pd101_heights_s2.csv")
pd101_s1_trainee <- pd101_s1_trainee %>% 
  left_join(pd101_s1_heights, by = "Name") %>%
  mutate(`Training time (months)` = Training_time_month + Training_time_years*12)
pd101_s2_trainee <- pd101_s2_trainee %>% 
  left_join(pd101_s2_heights, by = "Name") %>%
  mutate(`Training time (months)` = Training_time_month + Training_time_years*12)
df <- rbind(pd101_s1_trainee, pd101_s2_trainee)

pd101_s1_height_age <- pd101_s1_trainee %>%
  select(Name, Company, Age, Height, Ranking_final, Evaluation1) 
pd101_s2_height_age <- pd101_s2_trainee %>%
  select(Name, Company, Age, Height, Ranking_final, Evaluation1)


ui <- fluidPage(
  
  navbarPage("Produce 101",
             navbarMenu("About",
                        tabPanel("Produce 101", 
                                 verbatimTextOutput("Introduction"),
                                 tags$img(src="produce101_s1_s2.jpg")
                                 ),
                        tabPanel("Videos", verbatimTextOutput("video1"),
                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/BiorIyrjTHc" frameborder="0" allowfullscreen></iframe>'),
                                 verbatimTextOutput("video2"),
                                 HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NIld_iEc67s" frameborder="0" allowfullscreen></iframe>')),
                        tabPanel("References",
                                 verbatimTextOutput("References"))),
             navbarMenu("Trainee Info",
                        tabPanel("Season 1 trainees",
                                 plotOutput("plot_height_age_s1", height = 500,
                                            click = "plot1_click",
                                            brush = brushOpts(
                                              id = "plot1_brush"
                                            )),
                                 verbatimTextOutput("click_info_s1"),
                                 verbatimTextOutput("brush_info_s1"),
                                 DT::dataTableOutput("pd101_s1_trainee")),
                                 
                        tabPanel("Season 2 trainees",
                                 plotOutput("plot_height_age_s2", height = 500,
                                            click = "plot1_click",
                                            brush = brushOpts(
                                              id = "plot1_brush"
                                            )),
                                 verbatimTextOutput("click_info_s2"),
                                 verbatimTextOutput("brush_info_s2"),
                                 DT::dataTableOutput("pd101_s2_trainee")),
                        tabPanel("Full trainee list", DT::dataTableOutput("df"))),
             #buttons, get variable
             #https://rstudio.github.io/DT/003-tabletools-buttons.html
             navbarMenu("Data",
                        tabPanel("Explore", 
                                 selectInput("var1_exp", "Choose variable",
                                             names(df)[c(3, 22, 25, 26)]), 
                                 plotlyOutput("exploreplot1"),
                                 verbatimTextOutput("mean1"),
                                 verbatimTextOutput("mean2"),
                                 selectInput("var2_exp", "Choose x variable",
                                                        names(df)), 
                                 selectInput("var3_exp", "Choose y variable",
                                       names(df)), plotlyOutput("exploreplot2")),
                        #http://rstudio.github.io/shiny/tutorial/#building-inputs
                        #display average
                        tabPanel("Age", 
                                 verbatimTextOutput("agetext1"),plotlyOutput("plotage_s12"),
                                 verbatimTextOutput("agetext2"), plotOutput("plotage")),
                        tabPanel("Height"),
                        tabPanel("Training length"))
             # h4("sdfsdf")
             # h2("sdfsdf")
             # tags$li("sdf")
             # includeMarkdown()
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
    
My project is to explore which kind of trainees are more likely to debut." })
  #Introduction_videos
  output$video1 <- renderText({"Season 1 trailor"})
  output$video2 <- renderText({"Season 2 trailor"})
  #Introduction_References
  output$References <- renderText({"Data from wikipedia:
https://en.wikipedia.org/wiki/List_of_Produce_101_contestants
https://en.wikipedia.org/wiki/List_of_Produce_101_Season_2_contestants
Data about height, weight and training time:
https://www.wattpad.com/207570307-produce-101-profiles-season-1-produce-101
https://www.kpopmap.com/produce-101-season-2-kpop-profile/
https://www.kpopmap.com/produce-101-season-2-kpop-profile-part-2/
Videos:
https://www.youtube.com/watch?v=BiorIyrjTHc
https://www.youtube.com/watch?v=NIld_iEc67s
All published by Mnet.
    "})
  #Trainee Info_plot
  output$plot_height_age_s1 <- renderPlot({
    ggplot(pd101_s1_height_age, aes(Age, Height)) + 
      geom_point(aes(color = Ranking_final))
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
      geom_point(aes(color = Ranking_final))
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
  options = list(scrollX = TRUE))
  #Data comparison_age
  output$agetext1 <- renderText({"Left: distribution of age for Season 1 (female trainees).
    Average age: 19.5
Right: distribution of age for Season 2 (male trainees).
    Average age: 21.8"})
  output$agetext2 <- renderText({"Comparison of age between female trainees and male trainees.
    - Female trainees are generally younger"})
  output$mean1 <- renderText({paste("Average", input$var1_exp, "of female trainees: ",
                                   mean(pd101_s1_trainee[, input$var1_exp], na.rm = T))})
  output$mean2 <- renderText({paste("Average", input$var1_exp, "of male trainees: ",
                                    mean(pd101_s2_trainee[, input$var1_exp], na.rm = T))})
  output$plotage_s12 <- renderPlotly({
    p1 <- plot_ly(data = pd101_s1_trainee) %>%
      add_histogram(x = ~Age, color = ~Gender,
                    mode = "markers", marker=list(color="#ff82ab"))
    p2 <- plot_ly(data = pd101_s2_trainee) %>%
      add_histogram(x = ~Age, color = ~Gender,
                    mode = "markers", marker=list(color="#5cacee"))
    subplot(p1, p2)
  })
  output$exploreplot1 <- renderPlotly({
    p1 <- plot_ly(df, x = ~df[, input$var1_exp], color = ~Gender, type = "box",
                  colors = c("#ff82ab", "#5cacee")
                  )
    p2 <- plot_ly() %>%
      #https://plot.ly/r/reference/
      add_histogram(x = ~pd101_s1_trainee[, input$var1_exp], 
                    color = ~pd101_s1_trainee$Gender, 
                    mode = "markers", marker=list(color="#ff82ab")) %>%
      add_histogram(x = ~pd101_s2_trainee[, input$var1_exp], 
                    color = ~pd101_s2_trainee$Gender,
                    mode = "markers", marker=list(color="#5cacee")) %>%
      layout(barmode = "group")
    subplot(p1, p2)
    #https://plot.ly/ipython-notebooks/color-scales/
  })
  
  output$exploreplot2 <- renderPlotly({
    plot_ly(df, x = ~df[, input$var2_exp], y = ~df[, input$var3_exp]) 
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
      scale_color_manual(values=c("#ff82ab", "#5cacee"))
  })
  #add hover
  }

# Run the application 
shinyApp(ui = ui, server = server)
