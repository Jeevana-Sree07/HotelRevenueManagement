
library(shiny)
library(ggplot2)
library(dplyr)

# Sample hotel data
hotel_data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"),
  Revenue = c(45000, 52000, 48000, 61000, 68000, 75000),
  Occupancy = c(65, 70, 68, 78, 82, 88),
  Rooms = c(40, 40, 40, 40, 40, 40),
  Days = c(31, 28, 31, 30, 31, 30)
)

# Keep months in calendar order
hotel_data$Month <- factor(
  hotel_data$Month,
  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
)

# Illustrative calculations
hotel_data$Available_Room_Nights <- hotel_data$Rooms * hotel_data$Days
hotel_data$Occupied_Room_Nights <- round(
  hotel_data$Available_Room_Nights * hotel_data$Occupancy / 100
)
hotel_data$ADR <- hotel_data$Revenue /
  hotel_data$Occupied_Room_Nights
hotel_data$RevPAR <- hotel_data$Revenue /
  hotel_data$Available_Room_Nights

# Dashboard interface
ui <- fluidPage(
  titlePanel("Hotel Revenue Management Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "month",
        "Select Month:",
        choices = c("All", levels(hotel_data$Month)),
        selected = "All"
      )
    ),
    
    mainPanel(
      h3("Hotel Performance"),
      textOutput("total_revenue"),
      textOutput("occupancy_rate"),
      textOutput("adr"),
      textOutput("revpar"),
      plotOutput("revenue_plot"),
      plotOutput("occupancy_plot")
    )
  )
)

# Server calculations
server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$month == "All") {
      hotel_data
    } else {
      filter(hotel_data, Month == input$month)
    }
  })
  
  output$total_revenue <- renderText({
    paste(
      "Total Revenue: ₹",
      format(sum(filtered_data()$Revenue), big.mark = ",")
    )
  })
  
  output$occupancy_rate <- renderText({
    d <- filtered_data()
    rate <- sum(d$Occupied_Room_Nights) /
      sum(d$Available_Room_Nights) * 100
    paste("Occupancy Rate:", round(rate, 1), "%")
  })
  
  output$adr <- renderText({
    d <- filtered_data()
    rate <- sum(d$Revenue) / sum(d$Occupied_Room_Nights)
    paste("Average Daily Rate (ADR): ₹", round(rate, 2))
  })
  
  output$revpar <- renderText({
    d <- filtered_data()
    rate <- sum(d$Revenue) / sum(d$Available_Room_Nights)
    paste("Revenue per Available Room (RevPAR): ₹", round(rate, 2))
  })
  
  output$revenue_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Month, y = Revenue)) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Monthly Hotel Revenue",
        x = "Month",
        y = "Revenue (₹)"
      ) +
      theme_minimal()
  })
  
  output$occupancy_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Month, y = Occupancy)) +
      geom_col(fill = "darkseagreen") +
      labs(
        title = "Room Occupancy Rate",
        x = "Month",
        y = "Occupancy (%)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
