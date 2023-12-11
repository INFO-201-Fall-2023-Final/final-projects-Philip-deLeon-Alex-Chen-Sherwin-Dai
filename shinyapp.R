

library("stringr")
library(dplyr)
library(shiny)
library(ggplot2)


# Load data for the first app
unemp_df <- read.csv("october2023unemploymentrates.csv")
education_df <- read.csv("State_education_final.csv")
output_df <- read.csv("output.csv")

merge_df <- merge(x = unemp_df, y = education_df, by.x = "State", by.y = "States",
                  all.x = TRUE)

# Load data for the second app
washington_df <- filter(output_df, State == "Washington")
washington1_df <- filter(washington_df, Month == "December")

ui <- fluidPage(
  fluidRow(
    column(12,
           tags$div(
             style = "text-align: center; padding-bottom: 20px;",  # Adjust the padding as needed
             tags$p(
               "Welcome to the Unemployment Analysis App! This app is made by Alex Chen, Sherwin Dai, and Philip de Leon."
             ),
             tags$br(),  # Add a line break for extra space
             tags$p(
               "In our project we decided to focus on the topic of Unemployment throughout the US. So we started by looking for datasets that showed how unemployment in the US changed over time and other aspects of data that might relate to unemployment (i.e. education). From there, we curated our datasets and optimized them in order to visualize them easier. The first data set we curated had unemployment rates for each state in October 2023, so we joined it with a dataset with education statistics per state. Another data set that we curated had statistics for each county in the U.S. from 1990-2016. Because we wanted to focus on a specific state as well (Washington), we curated that data set to only have counties from Washington. After our code, the data set only had the unemployment data from counties in Washington. We decided to use that data set to visualize with an interactive time series plot for each different county depending on the user’s preference in Shiny. We performed the “contrast” analysis that compared the unemployment rate between different counties in Washington state and also the unemployment between different states in 2023.  The results we found were interesting: we found that actually since 1990, most of the counties’ unemployment rates have dropped significantly. For example, Klickitat County’s unemployment rate has dropped from almost 15% in 1990 to about 7% in 2016. Another example is Yakima County where the unemployment rate dropped from about 17.5% in 1993 to 10% in 2015. As a result of the growth of technology in recent years, it is not surprising that the unemployment rates are dropping. One could infer that the unemployment rate should drop as a result of the new jobs that are created from technological change. However, in the majority of counties, there is a huge upwards spike in unemployment around 2008 which was a result of the market crash which is very interesting, but all of the counties hold some sort of downward trend in unemployment.
"
               # Add your lengthy paragraph text here
             ),
             tags$img(src = "bear2.jpg", width = "70%")
           )
    )
  ),
  
  # Tabs for each functionality
  tabsetPanel(
    tabPanel("Unemployment Rates Per State", 
             selectInput(
               inputId = "selected_state",
               label = "Select a State:",
               choices = c("All", state.abb),
               selected = "All"
             ),
             plotOutput("unemployment_plot")
    ),
    
    tabPanel("Time Series Plot", 
             selectInput(
               inputId = "County",
               label = "Select a County:",
               choices = unique(washington1_df$County)
             ),
             plotOutput("timeSeriesPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  df <- read.csv("merged_df.csv")
  
  df <- df[order(df$October_2023_Unemployment_Rate, decreasing = TRUE), ]
  df$State <- state.abb[match(df$State, state.name)]
  df$State <- factor(df$State, levels = df$State)
  # Function for unemployment rates per state
  state_colors <- setNames(rainbow(length(state.abb)), state.abb)
  output$unemployment_plot <- renderPlot({
    selected_state <- input$selected_state
    filtered_df <- if (selected_state == "All") {
      df
    } else {
      df[df$State == selected_state, ]
    }
    ggplot(filtered_df, aes(x = October_2023_Unemployment_Rate, y = State, fill = State)) +
      geom_bar(stat = "identity") +
      labs(x = "Unemployment Rate", y = "State Name",
           title = "Unemployment rates per State of October 2023") +
      geom_vline(xintercept = mean(df$October_2023_Unemployment_Rate), color = "red", linetype = "dashed") +
      coord_flip() +
      scale_x_continuous(limits = c(0, max(df$October_2023_Unemployment_Rate) + 1)) +
      scale_y_discrete(limits = state.abb) +
      theme(legend.position = "none") +
      scale_fill_manual(values = state_colors)
  })
  
  # Function for time series plot
  selected_county_df <- reactive ({
    filter(washington1_df, County == input$County)
  })
  
  # Render the time series plot
  output$timeSeriesPlot <- renderPlot({
    ggplot(selected_county_df(), aes(x = Year, y = Rate)) +
      geom_line(color = "blue") +
      labs(title = "King County Unemployment Rates", x = "Year", y = "Unemployment Rate")
  })
}


shinyApp(ui = ui, server = server)
