###Shiny App 
#
#load libraries
library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(rgdal)
library(ggplot2)
load('data.RData')

# data<-data_cleaned%>%dplyr::select(id,listing_url, zipcode,latitude, longitude,neighbourhood_group_cleansed,
#                                    cleaning_fee,price,
#                                    review_scores_rating,room_type,host_is_superhost,
#                                    accommodates,guests_included,extra_people)
# 
min_pax=min(data$accommodates)
max_pax=max(data$accommodates)
min_price=min(data$price)
max_price=max(data$price)

# for(i in min_pax:max_pax){
#     data[,paste0('price_',i,'pax_avg')]<-data%>%
#         rowwise%>%
#         mutate(temp= ifelse(accommodates<i,NA,round((price+max(0,(i-guests_included))*extra_people)/i)))%>%
#         .$temp
# }
sel_dat<-data

ui <- fluidPage(
    sidebarLayout(
        mainPanel( 
            #this will create a space for us to display our map
            tags$style(type = "text/css", "#mymap {height: calc(100vh - 20px) !important;}"),
            leafletOutput(outputId = "mymap")
            ),
        
        sidebarPanel(top = 60, left = 20, 
            h4("Interactive New York Airbnb Listings Map"),
            fluidRow( 
                column(6,
                       numericInput("pax","Persons to Check In",min_pax,min=min_pax,max=max_pax,step=1)
                )
            ),
           sliderInput("price",label="Price per Person per Night",
                         min = floor(min_price/50)*50, max = ceiling(max_price/50)*50,
                         value = c(min_price,max_price), pre = "$", sep = ","),
           fluidRow( 
               column(6,
                        checkboxGroupInput("room_type","Room Type",
                                             c("Entire home/apt","Private room", "Shared room" ,"Hotel room"),
                                             selected = c("Entire home/apt","Private room", "Shared room" ,"Hotel room"))
                          
                                
                      ),
                 column(6,
                        tags$style(type = "text/css", ".Yes {color:blue;}"),
                        checkboxGroupInput("super","Host by Superhost",c("Yes(Red)","No(Blue)"),
                                               selected = c("Yes(Red)","No(Blue)"))
                        
                )

         ),
         
         plotOutput("boxplot")
         
        )

    )

)

server <- function(input, output, session) {
    pal2 <- colorFactor(
        palette = c('blue', 'red'),
        domain = data$host_is_superhost
    )
    
    output$leaf=renderUI({
        leafletOutput('mymap')
    })
    
    observe({
        sel_pax<-input$pax
        
        price_col<-paste0("price_",sel_pax,"pax_avg")
        
        sel_dat<-sel_dat%>%filter(accommodates>=sel_pax)
        
        #update price slider
        updateSliderInput(session, "price", value = c(min(sel_dat[,price_col]),max(sel_dat[,price_col])))
    })
    
    dataFiltered<-reactive({
        sel_pax<-input$pax
        price_col<-paste0("price_",sel_pax,"pax_avg")
        sel_dat%>%mutate(price_check = get(price_col))%>%
            filter(price_check>=input$price[1] & price_check<=input$price[2])%>%
            filter(room_type%in%input$room_type)%>%
            filter(host_is_superhost%in%as.logical(input$super=="Yes(Red)"))%>%
            filter(accommodates>=sel_pax)
    })
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet() %>% 
            setView(-73.90, 40.75, zoom = 11) %>%
            addTiles() %>%
            addCircles(data = dataFiltered(), lat = ~ latitude, lng = ~ longitude, weight = 1, 
                       radius = 20, popup = ~paste0("Price: $", price_check, "<br>",
                                                   "Cleaning Fee: $", cleaning_fee, " per stay<br>",
                                                   "Neighbourhood: ",neighbourhood_group_cleansed, "<br>",
                                                   "Review Score: ", review_scores_rating, "<br>",
                                                   "Superhost: ",host_is_superhost,"<br>",
                                                   "Max Capacity: ", accommodates, "<br>",
                                                   '<a href=',listing_url,'>Link to Listing</a>'),
                                                  
                       label = ~as.character(paste0("Price: $", sep = " ", price_check)), 
                       color = ~pal2(host_is_superhost), fillOpacity = 0.5)
    })
    
    #create boxplpt
    output$boxplot<-renderPlot({
        ggplot()+
            geom_boxplot(data=dataFiltered(),
                         aes(x=fct_rev(fct_infreq(neighbourhood_group_cleansed)),y=price_check),varwidth = T)+
            coord_flip()+
            labs(y = 'Price',
                 x = '',
                 title = 'Distribution of Price per Person per Night')+
            geom_text(data=dataFiltered()%>%group_by(neighbourhood_group_cleansed)%>%
                          count()%>%ungroup()%>%arrange(-n),
                      aes(x=neighbourhood_group_cleansed,
                          y=min(dataFiltered()$price_check),label=n),
                      vjust=-2)+
            geom_text(data=dataFiltered()%>%group_by(neighbourhood_group_cleansed)%>%
                          count()%>%ungroup()%>%arrange(n)%>%slice(1),
                      aes(x=neighbourhood_group_cleansed,
                          y=min(dataFiltered()$price_check),label="Total Listing"),
                      vjust=3,hjust=0.1)+
            theme(axis.text.y = element_text(face="bold",  
                                             size=10))
            
    })
    
}

shinyApp(ui, server)
