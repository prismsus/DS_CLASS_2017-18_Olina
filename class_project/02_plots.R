output$mean1 <-
  renderText({
    paste(
      "Average",
      str_to_lower(input$var1_exp, locale = "en"),
      "of female trainees: ",
      signif(mean(pd101_s1_trainee[, input$var1_exp]), 4)
    )
  }) #, na.rm = T
output$mean2 <-
  renderText({
    paste(
      "Average",
      str_to_lower(input$var1_exp, locale = "en"),
      "of male trainees: ",
      signif(mean(pd101_s2_trainee[, input$var1_exp]), 4)
    )
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
output$warning <- renderText({
  if((!is.null(input$vars))&&(!is.na(input$vars[2]))){
    if(is.integer(type.convert(input$bin_exp2))&&as.numeric(input$bin_exp2)>0){
      paste("")
    }
    else{
      paste("Default graph displayed, please input a positive integer for \"number of boxes\".")
    }
  }
    else
    {
      paste("Default graph displayed, please choose two variables.")
    }
})

output$exploreplot2 <- renderPlot({
  if((!is.null(input$vars))&&(!is.na(input$vars[2]))){
    if(is.integer(type.convert(input$bin_exp2))&&as.numeric(input$bin_exp2)>0){
      ggplot(df, aes(df[, input$vars[1]], df[, input$vars[2]], color = Gender)) +
      geom_boxplot(aes(group = cut_interval(df[, input$vars[1]], as.numeric(input$bin_exp2)))) +
      facet_wrap( ~ as.factor(Gender), nrow = 1) +
      ylab(input$vars[2]) +
      xlab(input$vars[1]) +
      scale_color_manual(values = c("#ff82ab", "#5cacee"))
    }
    else
    {
      ggplot(df, aes(df[, input$vars[1]], df[, input$vars[2]], color = Gender)) +
      geom_boxplot(aes(group = cut_interval(df[, input$vars[1]], 5))) +
      facet_wrap( ~ as.factor(Gender), nrow = 1) +
      ylab(input$vars[2]) +
      xlab(input$vars[1]) +
      scale_color_manual(values = c("#ff82ab", "#5cacee"))
    }
  }
  else
  {
    #h4("Default graph displayed")
    ggplot(df, aes(Age, Weight, color = Gender)) +
      geom_boxplot(aes(group = cut_interval(Age, 5))) +
      facet_wrap( ~ as.factor(Gender), nrow = 1) +
      ylab("Weight") +
      xlab("Age") +
      scale_color_manual(values = c("#ff82ab", "#5cacee"))
  }
})

output$exploreplot3 <- renderPlot({
  ggplotRegression <- function (fit) {
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() +
      stat_smooth(method = "lm", col = "grey") +
      labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                         "Intercept =",signif(fit$coef[[1]],5 ),
                         " Slope =",signif(fit$coef[[2]], 5),
                         " P =",signif(summary(fit)$coef[2,4], 5)))
  }
  
  if(input$graphtype == "Boxplot"){
    output$error_plot3 <-
      renderText({
        ""
      })
    if(length(input$g_options)==0){
      ggplot(df, aes(df[,input$exp3_var1], df[,input$exp3_var2])) +
        geom_boxplot(aes(group = cut_interval(df[,input$exp3_var1], 5))) +
        labs(x = input$exp3_var1, y = input$exp3_var2)
    }
    else if(input$g_options[1]=="lm"){
      if(length(input$g_options)==1){
        ggplot(df, aes(df[,input$exp3_var1], df[,input$exp3_var2])) +
          geom_boxplot(aes(group = cut_interval(df[,input$exp3_var1], 5))) +
          geom_smooth(method = "lm", se=T, color="grey", aes(group=1)) +
          labs(x = input$exp3_var1, y = input$exp3_var2)
      }
      else if(input$g_options[2]=="sg"){
        p1 <- ggplot(pd101_s1_trainee, aes(pd101_s1_trainee[,input$exp3_var1], pd101_s1_trainee[,input$exp3_var2])) +
          geom_boxplot(aes(group = cut_interval(pd101_s1_trainee[,input$exp3_var1], 5)), color = "#ff82ab") +
          geom_smooth(method = "lm", se=T, color="grey", aes(group=1)) +
          labs(x = input$exp3_var1, y = input$exp3_var2)
        p2 <- ggplot(pd101_s2_trainee, aes(pd101_s2_trainee[,input$exp3_var1], pd101_s2_trainee[,input$exp3_var2])) +
          geom_boxplot(aes(group = cut_interval(pd101_s2_trainee[,input$exp3_var1], 5)), color = "#5cacee") +
          geom_smooth(method = "lm", se=T, color="grey", aes(group=1)) +
          labs(x = input$exp3_var1, y = input$exp3_var2)
        grid.arrange(p1,p2,ncol = 1)
      }
    }
    else if(input$g_options[1]=="sg"){
      p1 <- ggplot(pd101_s1_trainee, aes(pd101_s1_trainee[,input$exp3_var1], pd101_s1_trainee[,input$exp3_var2])) +
        geom_boxplot(aes(group = cut_interval(pd101_s1_trainee[,input$exp3_var1], 5)), color = "#ff82ab") +
        labs(x = input$exp3_var1, y = input$exp3_var2)
      p2 <- ggplot(pd101_s2_trainee, aes(pd101_s2_trainee[,input$exp3_var1], pd101_s2_trainee[,input$exp3_var2])) +
        geom_boxplot(aes(group = cut_interval(pd101_s2_trainee[,input$exp3_var1], 5)), color = "#5cacee") +
        labs(x = input$exp3_var1, y = input$exp3_var2)
      grid.arrange(p1,p2,ncol = 1)
    }

  }
  
  else if(input$graphtype == "Scatter plot"){
    if(length(input$g_options)==0){
      output$error_plot3 <-
        renderText({
          ""
        })
      ggplot(df, aes(df[,input$exp3_var1], df[,input$exp3_var2])) +
        geom_point() +
        labs(x = input$exp3_var1, y = input$exp3_var2)
    }
    else if(input$g_options[1]=="lm"){
      output$error_plot3 <-
        renderText({
          "Errors might exist for some linear models because of missing value, try other variables (age, evaluation, height, weight or training time) or uncheck the linear model."
        })
      if(length(input$g_options)==1){
        ggplotRegression(lm(df[,input$exp3_var2] ~ df[,input$exp3_var1], data = df)) +
          labs(x = input$exp3_var1, y = input$exp3_var2)
      }
      else if(input$g_options[2]=="sg"){
        p1 <- ggplotRegression(lm(pd101_s1_trainee[,input$exp3_var2] ~ pd101_s1_trainee[,input$exp3_var1], data = pd101_s1_trainee)) +
          geom_point(color = "#ff82ab") +
          labs(x = input$exp3_var1, y = input$exp3_var2)
        p2 <- ggplotRegression(lm(pd101_s2_trainee[,input$exp3_var2] ~ pd101_s2_trainee[,input$exp3_var1], data = pd101_s2_trainee)) +
          geom_point(color = "#5cacee")+
          labs(x = input$exp3_var1, y = input$exp3_var2)
        grid.arrange(p1,p2,ncol = 1)
      }
    }
    else if(input$g_options[1]=="sg"){
      output$error_plot3 <-
        renderText({
          ""
        })
      p1<- ggplot(pd101_s1_trainee, aes(pd101_s1_trainee[,input$exp3_var1], pd101_s1_trainee[,input$exp3_var2])) +
        geom_point(color = "#ff82ab") +
        labs(x = input$exp3_var1, y = input$exp3_var2)
      p2<- ggplot(pd101_s2_trainee, aes(pd101_s2_trainee[,input$exp3_var1], pd101_s2_trainee[,input$exp3_var2])) +
        geom_point(color = "#5cacee") +
        labs(x = input$exp3_var1, y = input$exp3_var2)
      grid.arrange(p1,p2,ncol = 1)
    }
  }
})


#signif(x,3)