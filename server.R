library(shiny)
library(shinydashboard)
library(DT)
library(pwr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
##### Two proportions #####
  ##### Fast calculation #####
  n_2props <- reactive({
    req(as.numeric(input$p1_2props)>0&
          as.numeric(input$p2_2props)>0&
          as.numeric(input$alpha_2props)>0&
          as.numeric(input$power_2props)>0, 
        cancelOutput = TRUE)
    pwr <- pwr.2p.test(h = ES.h(p1 = as.numeric(input$p1_2props), p2 = as.numeric(input$p2_2props)),
                       sig.level = as.numeric(input$alpha_2props), 
                       power = as.numeric(input$power_2props))
    pwr$n
  })
  n1_2props <- reactive({
    req(input$k_2props>=1, cancelOutput = TRUE)
    big_n <- 2*n_2props()*(1+input$k_2props)^2/(4*input$k_2props)
    big_n/(1+input$k_2props)
  })
  output$n1_2props <- renderValueBox({
    valueBox(
      value = ceiling(n1_2props()),
      subtitle = "Nhóm 1",
      icon = icon("capsules"),
      color = "green",
    )
  })
  output$n2_2props <- renderValueBox({
    valueBox(
      value = input$k_2props*ceiling(n1_2props()),
      subtitle = "Nhóm 2",
      icon = icon("tablets"),
      color = "orange",
    )
  })

  ##### Input file #####
  # Make an example
  temp_2props <- reactive({
    data.frame(Outcome = c("Live birth", "Ongoing pregnancy"), 
               Treatment = c(0.513, 0.565), 
               Control = c(0.233, 0.288))
  })
  # Download template
  output$temp_2props <- downloadHandler(
    filename = "template_2props.csv",
    content = function(con) {
      write.csv(temp_2props(), row.names = F, con)
    }
  )
  # Estimate sample size
  df_2props <- reactive({
    req(as.numeric(input$alpha_df_2props)>0&
          as.numeric(input$power_df_2props)>0&
          input$k_df_2props>=1,
        cancelOutput = TRUE)
    if (is.null(input$upload_2props)) {
      df <- temp_2props()
    } else {
      df <- read.csv(file = input$upload_2props$datapath, 
                     header = T, stringsAsFactors = F, check.names = F, fill = T)
    }
    for (i in 1:nrow(df)) {
      pwr <- pwr.2p.test(h = ES.h(p1 = df$Treatment[i], p2 = df$Control[i]),
                         sig.level = as.numeric(input$alpha_df_2props), 
                         power = as.numeric(input$power_df_2props))
      df$n[i] <- pwr$n
    }
    df$big_n <- 2*df$n*(1+input$k_df_2props)^2/(4*input$k_df_2props)
    df$n1 <- ceiling(df$big_n/(1+input$k_df_2props))
    df$n2 <- input$k_df_2props*df$n1
    df$Total <- df$n1 + df$n2
    df$n <- NULL
    df$big_n <- NULL
    df
  })
  # Display text
  output$text_df_2props <- renderText({
    req(as.numeric(input$alpha_df_2props)>0&
          as.numeric(input$power_df_2props)>0&
          input$k_df_2props>=1,
        cancelOutput = TRUE)
    paste0("alpha = ", 
           input$alpha_df_2props, ", power = ", input$power_df_2props, 
           " và k = ", input$k_df_2props)
  })
  # Display sample size dataframe
  output$ss_df_2props <- DT::renderDataTable({
    df_2props()
  })
  # Download sample size dataframe
  output$download_df_2props <- downloadHandler(
    filename = "df_2props.csv",
    content = function(con) {
      write.csv(df_2props(), row.names = F, con)
    }
  )

##### Two means #####
  n_2means <- reactive({
    req(as.numeric(input$m1_2means)>0&
          as.numeric(input$sd1_2means)>0&
          as.numeric(input$m2_2means)>0&
          as.numeric(input$sd2_2means)>0&
          as.numeric(input$alpha_2means)>0&
          as.numeric(input$power_2means)>0, 
        cancelOutput = TRUE)
    numerator <- abs(as.numeric(input$m1_2means)-as.numeric(input$m2_2means))
    denominator<- sqrt(((as.numeric(input$sd1_2means)^2)+(as.numeric(input$sd2_2means)^2))/2)
    d <- numerator/denominator
    pwr <- pwr.t.test(d = d, 
                      sig.level = as.numeric(input$alpha_2means), 
                      power = as.numeric(input$power_2means), 
                      type = "two.sample",
                      alternative = "two.sided")
    pwr$n
  })
  n1_2means <- reactive({
    req(input$k_2means>=1, cancelOutput = TRUE)
    big_n <- 2*n_2means()*(1+input$k_2means)^2/(4*input$k_2means)
    big_n/(1+input$k_2means)
  })
  output$n1_2means <- renderValueBox({
    valueBox(
      value = ceiling(n1_2means()),
      subtitle = "Nhóm 1",
      icon = icon("capsules"),
      color = "green",
    )
  })
  output$n2_2means <- renderValueBox({
    valueBox(
      value = input$k_2means*ceiling(n1_2means()),
      subtitle = "Nhóm 2",
      icon = icon("tablets"),
      color = "orange",
    )
  })
  
##### Correlation #####
  n_corr <- reactive({
    req(as.numeric(input$r_corr)>0&
          as.numeric(input$alpha_corr)>0&
          as.numeric(input$power_corr)>0, 
        cancelOutput = TRUE)
    pwr <- pwr.r.test(r = as.numeric(input$r_corr), 
                      sig.level = as.numeric(input$alpha_corr), 
                      power = as.numeric(input$power_corr))
    pwr$n
  })
  output$n_corr <- renderValueBox({
    valueBox(
      value = ceiling(n_corr()),
      subtitle = "Cỡ mẫu",
      icon = icon("users"),
      color = "green",
    )
  })
})
