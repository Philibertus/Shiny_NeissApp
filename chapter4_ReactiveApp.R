ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", choices = setNames(products$prod_code, products$title))
           ),
    column(2, selectInput("y", "Y axis", c("rate", "count"))),
    column(2, numericInput("n", "Rows", min = 1, max = 10, value = 5))
    ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("bodypart")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(column(2, actionButton("prev_story", "Previous story")),
           column(2, actionButton("next_story", "Next story")),
           column(10, textOutput("narrative"))
           )
)


server <- function(input,output,session){
  selected <- reactive({
    injuries %>%
      as_tibble() %>%
      dplyr::filter(prod_code == input$code)
  })
  
  output$diag <- renderTable({
    selected() %>%
      mutate(diag = fct_lump(fct_infreq(diag), n = input$n)) %>%
               group_by(diag) %>%
               summarize(n = as.integer(sum(weight)))
    }, width = "100%")
  
  output$bodypart <- renderTable({
    selected() %>%
      mutate(body_part = fct_lump(fct_infreq(body_part), n = input$n)) %>%
               group_by(body_part) %>%
               summarize(n = as.integer(sum(weight)))
  }, width = "100%")
  
  output$location <- renderTable({
    selected() %>%
      mutate(location= fct_lump(fct_infreq(location), n = input$n)) %>%
               group_by(location) %>%
               summarize(n = as.integer(sum(weight)))
  }, width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age,sex,wt = weight) %>%
      dplyr::left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n/population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if(input$y == "rate"){
    summary() %>%
      ggplot(aes(x = age, y = rate, color = sex)) +
      geom_line(na.rm = TRUE) +
      theme_bw() +
      scale_color_manual(values = c("male" = "dodgerblue", "female" = "pink2")) +
      labs(y = "Rate of injuries per 10'000 people")
    } else {
      summary() %>%
        ggplot(aes(x = age, y = n, color = sex)) +
        geom_line(na.rm = TRUE) +
        theme_bw() +
        scale_color_manual(values = c("male" = "dodgerblue", "female" = "pink2")) +
        labs(y = "Estimated number of injuries")
        }
      }, res = 96)
  
  
  # Store the maximum posible number of stories.
  max_no_stories <- reactive({
    length(selected()$narrative)
    })
  
  # Reactive used to save the current position in the narrative list.
  story <- reactiveVal(1)
  
  # Reset the story counter if the user changes the product code. 
  observeEvent(input$code, {
    story(1)
  })
  
  # When the user clicks "Next story", increase the current position in the
  # narrative but never go beyond the interval [1, length of the narrative].
  # Note that the mod function (%%) is keeping `current`` within this interval.
  observeEvent(input$next_story, {
    story(((story() + 0) %% max_no_stories()) + 1)
  })
  
  # When the user clicks "Previous story" decrease the current position in the
  # narrative. Note that we also take advantage of the mod function.
  observeEvent(input$prev_story, {
    story(((story() - 2) %% max_no_stories()) + 1)
  })
  
  output$narrative <- renderText({
    selected()$narrative[story()]
  })

}
shinyApp(ui,server)


#####FUNCTIONS:

count_top <- function(df, var, n = 5){
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarize(n = as.integer(sum(weight)))
}
