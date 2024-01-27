ui <- navbarPage(
  
  title = "LTER Animal Data Explorer",
  
  # (Page 1) intro tabPanel ----
  tabPanel(title = "About this App",
           
           # intro text fluidRow ----
           fluidRow(
             
             # use columns to create white space on sides
             column(1),
             column(10, includeMarkdown("text/about.md")),
             column(1)
             
           ), # END intro text fluidRow
           
           hr(), # creates light gray horizontal line
           
           # footer text ----
           includeMarkdown("text/footer.md"),
           column(3)
           
  ), # END (Page 1) intro tabPanel
  
  # (Page 2) data viz tabPanel ----
  tabPanel(title = "Explore the Data",
           
           # tabsetPanel to contain tabs for data viz ----
           tabsetPanel(
             
             # trout tabPanel ----
             tabPanel(title = "Trout",
                      
                      # trout plot sidebarLayout ----
                      sidebarLayout(
                        
                        # trout plot sidebarPanel ----
                        sidebarPanel(
                          
                          # channel type pickerInput ----
                          pickerInput(inputId = "channel_type_input", label = "Select channel type(s):",
                                      choices = unique(clean_trout$channel_type),
                                      selected = c("cascade", "pool"),
                                      options = pickerOptions(actionsBox = TRUE),
                                      multiple = TRUE), # END channel type pickerInput
                          
                          # # section checkboxGroupInput ----
                          checkboxGroupButtons(inputId = "section_input", label = "Select a sampling section(s):",
                                               choices = c("clear cut forest", "old growth forest"),
                                               selected = c("clear cut forest", "old growth forest"),
                                               individual = FALSE, justified = TRUE, size = "sm",
                                               checkIcon = list(yes = icon("check", lib = "font-awesome"), 
                                                                no = icon("xmark", lib = "font-awesome"))), # END section checkboxGroupInput
                          
                        ), # END trout plot sidebarPanel
                        
                        # trout plot mainPanel ----
                        mainPanel(
                          
                          plotOutput(outputId = "trout_scatterplot_output") #|>
                            #withSpinner(color = "#4BA4A4", type = 4, size = 2)
                          
                        ) # END trout plot mainPanel
                        
                      ) # END trout plot sidebarLayout
                      
             ), # END trout tabPanel 
             
             # penguin tabPanel ----
             tabPanel(title = "Penguins",
                      
                      # penguin plot sidebarLayout ----
                      sidebarLayout(
                        
                        # penguin plot sidebarPanel ----
                        sidebarPanel(
                          
                          # island pickerInput ----
                          pickerInput(inputId = "penguin_island_input", 
                                      label = "Select an island(s):",
                                      choices = c("Torgersen", "Dream", "Biscoe"),
                                      selected = c("Torgersen", "Dream", "Biscoe"),
                                      options = pickerOptions(actionsBox = TRUE),
                                      multiple = TRUE), # END island pickerInput
                          
                          # bin number sliderInput ----
                          sliderInput(inputId = "bin_num_input", 
                                      label = "Select number of bins:",
                                      value = 25, max = 100, min = 1), # END bin number sliderInput
                          
                        ), # END penguin plot sidebarPanel
                        
                        # penguin plot mainPanel ----
                        mainPanel(
                          
                          # histogram output ----
                          plotOutput(outputId = "flipperLength_histogram_output") #%>%
                            #withSpinner(color = "#006792", type = 1)
                          
                        ) # END penguin plot mainPanel
                        
                      ) # END penguin plot sidebarLayout
                      
             ) # END penguin tabPanel
             
           ) # END tabsetPanel
           
  ) # END (Page 2) data viz tabPanel
  
) # END navbarPage