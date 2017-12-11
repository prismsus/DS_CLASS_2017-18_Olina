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