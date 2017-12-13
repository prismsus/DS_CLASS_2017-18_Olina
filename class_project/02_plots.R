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
  ggplot(df, aes(Age, Weight, color = Gender)) +
    geom_boxplot(aes(group = cut_interval(Age, 5)))
})