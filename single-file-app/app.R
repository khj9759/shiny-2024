# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)

# user interface ----
ui <- fluidPage(
  
  # app title ----
  tags$h1("My App Title"), # alternatively, you can use the `h1()` wrapper function
  
  # app subtitle -----
  h4(strong("Exploring Antarctic Penguin Data")), # alternatively, `tags$h4(tags$strong("text"))`
  
  # body mass slider input ---- 
  sliderInput(inputId = "body_mass_input", 
              label = "Select a range of body masses (g):",
              min = 2700, max = 6300, value = c(3000, 4000)),
  
  
  # body mass plot ouput ----
  plotOutput(outputId = "bodyMass_scatterplot_output"),
  
  # year output
  checkboxGroupInput(inputId = "year_input", label = "Select year(s):",
                     choices = c("2007", "2008", "2009"), # or `unique(penguins$year_input)` | NOTE: update checkbox display name by using "New name" = "observation name" (e.g "The year 2007" = "2007")
                     selected = c("2007", "2008")),
  
  # DT output ----
  DT::dataTableOutput(outputId = "penguin_DT_output")
)

# server instructions ----
server <- function(input, output) {
  
  # filter body masses -----
  body_mass_df <- reactive ({ 
    penguins %>%
      filter(body_mass_g %in% c(input$body_mass_input[1]:input$body_mass_input[2]))
  })
  
  # render penguin scatter plot ----
  output$bodyMass_scatterplot_output <- renderPlot({
    # add ggplot code here
    ggplot(na.omit(body_mass_df()),
           aes(x = flipper_length_mm, y = bill_length_mm,
               color = species, shape = species)) + 
      geom_point() + 
      scale_color_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19, "Chinstrap" = 17, "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", y = "Bill length (mm)", 
           color = "Penguin species", shape = "Penguin species") + 
      theme_minimal() + 
      theme(legend.position = c(0.85, 0.2), 
            legend.background = element_rect(color = "white")) 
  })
  
  # filter for years ----
  years_df <- reactive({
    penguins %>%
      filter(year %in% c(input$year_input)) # return observations where year is "in" the set of options provided by the user via the checkboxGroupInput
  })
  
  # render the DT::datatable ----
  output$penguin_DT_output <- DT::renderDataTable({
    DT::datatable(years_df(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)
