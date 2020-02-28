library(shiny)
library(shinythemes)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)
library(DT)
library(maps)
library(sf)
library(readr)
king <- read_csv("data/KING COUNTY House Data.csv")


ui <- fluidPage(theme = shinytheme("united"),
                
                navbarPage("King Co.",
                           
                           tabPanel("Interactive Plots",
                                    
                                    titlePanel(h3("King County, WA, USA Housing Data")),
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput(inputId = "dec", label = "Choose decade the houses were built:",
                                                        choices = king$decade,
                                                        selected = character(0),
                                                        multiple = FALSE),
                                            br(),
                                            numericInput(inputId = "num", label = "Type in a maximum price to see which homes fall within your range",
                                                         value = 500000,
                                                         min = ,
                                                         max = 8000000)
                                            
                                        ),
                                        
                                        mainPanel(
                                            # tags$style below is to supress red warning/error messages in app
                                            # for invalid filter values (ex: max price is too low)
                                            tags$style(type="text/css",
                                                       ".shiny-output-error { visibility: hidden; }",
                                                       ".shiny-output-error:before { visibility: hidden; }"),
                                            tabsetPanel(
                                                tabPanel("Average Price by Year (filter by decade only)", plotOutput("individual_dec"),
                                                         h6("*Since this is a line plot you can only
                            filter by decade. Prices are effectively filtered by decade.")),
                                                tabPanel("Map by Price", plotlyOutput("price_point_map"),
                                                         h6("*Filter by 'Decade' and 'Price' available. Hover over the points
                            with your mouse for more information.")),
                                                tabPanel("Price by Zip Code Map", plotlyOutput("zip_code_map"),
                                                         h6("*Filter by 'Decade' and 'Price' available.
                            Setting a max price will display only the zip codes in King County
                            that fall below your specified dollar amount.
                            You may hover over zip code lines with your mouse
                            to get more information."),
                                                         h6(strong("*If the price you typed is too low nothing will show up,
                            simply type in a higher value. There's no need to refresh the page.
                                  You may need to give it a moment to process.")))
                                                
                                                
                                            )
                                        )
                                    )
                           ),
                           tabPanel("Table",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput(inputId = "dec2", label = "Choose decade the houses were built:",
                                                        choices = king$decade,
                                                        selected = character(0),
                                                        multiple = FALSE),
                                            br(),
                                            numericInput(inputId = "num2", label =
                                                             "Type in a maximum price to see which
                      houses fall within your range",
                                                         value = 500000,
                                                         min = 0,
                                                         max = 8000000)
                                        ),
                                        mainPanel(
                                            dataTableOutput("data_table")
                                            
                                            
                                            
                )
            )
        )   
    )
)

server <- function(input, output) {
    # main ggplot for question 1. Line graph will be static only..EXCLUDED IN FINAL VERSION
    #output$lineplot <- renderPlot({
    
    # by_decade <- king %>% group_by(decade) %>% summarize(avg_price_dec = sum(price)/n())
    # 
    # ggplot(by_decade) + aes(x = decade, y = avg_price_dec) +
    #     geom_line(color = "red") +
    #     scale_y_continuous(labels = dollar_format(
    #         scale = 1, accuracy = 1, big.mark = ",")) +
    #     scale_x_continuous(breaks = seq(1900, 2010, 10)) +
    #     ggtitle("Average House Price by Decade Built, King Co., WA, USA") +
    #     theme(plot.title = element_text(hjust = 0.5)) +
    #     labs(x = "Year", y = "Average Price")
    
    #})
    # this visual is created to be interactive with choosing a decade only, prices are averages..
    output$individual_dec <- renderPlot({
        
        by_user_dec <- king %>%
            filter(decade == input$dec) %>%
            group_by(yr_built) %>%
            summarize(avg_yr_price = sum(price)/n())
        
        ggplot(by_user_dec, aes(x = yr_built, y = avg_yr_price)) +
            geom_line(color = "blue") +
            scale_y_continuous(labels = dollar_format(
                scale = 1, accuracy = 1, big.mark = ","))+
            ggtitle("Average Price by Year Built") +
            labs(x = "Year Built", y = "Price") +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(labels =
                                   number_format(accuracy = 1, scale = 1, big.mark = ""), breaks =
                                   (seq(min(by_user_dec$yr_built), max(by_user_dec$yr_built), by = 1)))
        
        
    })
    # interactive map. Filter by price AND decade
    output$price_point_map <- renderPlotly({
        
        
        counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
        
        king_co <- subset(counties, ID == "washington,king")
        
        by__user_max_price <- king %>%
            filter(price <= input$num & decade == input$dec)
        
        ggplotly(ggplot(king_co) + geom_sf() +
                     geom_point(by__user_max_price, mapping = aes(x = long, y = lat,
                                                                  color = price, label = id, label2 = yr_built)) +
                     scale_color_continuous(low = "yellow", high = "red",
                                            labels = dollar_format(prefix = "$"), name = "Price") +
                     ggtitle("Distribution of of Housing Prices by Year Built") +
                     theme(plot.title = element_text(hjust = 0.5)) +
                     labs(x = "Longitude", y = "Latitude") +
                     theme(axis.text.x = element_text(size = 7)))
        
        
    })
    # price by zip code map. Filter by decade and price.
    output$zip_code_map <- renderPlotly({
        
        kc_zip <- st_read("data/SHP/Zipcodes_for_King_County_and_Surrounding_Area__zipcode_area.shp")
        
        joined_zip_data <- king %>%
            group_by(zipcode, decade) %>%
            summarize(average_cost = sum(price)/n()) %>%
            full_join(kc_zip, by = c("zipcode" = "ZIP")) %>%
            filter(COUNTY == "033")
        
        user_filter <- joined_zip_data %>%
            filter(decade == input$dec & average_cost <= input$num)
        
        ggplotly(ggplot(kc_zip) +
                     geom_sf(user_filter, mapping = aes(
                         geometry = geometry, fill = average_cost, label = zipcode)) +
                     scale_fill_continuous(low = "yellow", high = "red",
                                           labels = dollar_format(prefix = "$"), name = "Average Price") +
                     ggtitle("Average Housing Price per Zip Code by Decade Built") +
                     labs(x = "Longitude", y = "Latitude") +
                     theme(axis.text.x = element_text(size = 7)))
        
    })
    
    
    # data table is interactive with selection of decade and price
    output$data_table <- renderDataTable({
        (king %>%
             filter(decade == input$dec2 & price <= input$num2) %>%
             select(id, price, sqft_living, sqft_lot, bathrooms,
                    bedrooms, yr_built, zipcode, long, lat))
    })
}


shinyApp(ui = ui, server = server)
