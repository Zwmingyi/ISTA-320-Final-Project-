#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(usmap)
library(shiny)
library(tidyverse)
library(statebins)

# Read in data
obesity_data<- read.csv("data/Nutrition__Physical_Activity__and_Obesity_-_Youth_Risk_Behavior_Surveillance_System.csv")


physical_sugar_obesity<- obesity_data %>% filter(YearEnd == YearStart) %>% 
  filter(Total == "Total")

physical_sugar_obesity<-physical_sugar_obesity[, c("YearStart","LocationDesc",
                                                   "QuestionID","Data_Value")]

tidy_data<- physical_sugar_obesity %>% 
  pivot_wider(names_from = QuestionID, values_from = Data_Value)
tidy_data <- tidy_data[,sort(names(tidy_data))]

colnames(tidy_data) <- c("state","Less_fruit","Less_veggie","Obesity",
                         "Overweight","MV_PE","Daily_PE","Drink_soda","Watch_TV","Year")

Year <- tidy_data %>% filter(Year != "2003")

states_data <- tidy_data %>% filter(Year == "2019")
states <- states_data$state


female<- obesity_data %>% filter(Gender == "Male")
male<- obesity_data %>% filter(Gender == "Female")
gender_obesity<- rbind(female,male)
gender_obesity<- gender_obesity %>% filter(QuestionID == "Q038")

model <- gender_obesity %>% lm(formula = Data_Value~Gender)



ui <- fluidPage(
  
  # Application title
  titlePanel("Adolencents Obsity Rate By State"),
  
  # sidebar with info 
  sidebarLayout(
    sidebarPanel(
      h3("Adolencents Obsity Rate Data"),
      p("This analysis focuses on teen obesity of states across time, 
        percentage of teen obesity and related activties and difference between male and female obesity."),
      HTML('Data from the <a href="https://chronicdata.cdc.gov/Nutrition-Physical-Activity-and-Obesity/Nutrition-Physical-Activity-and-Obesity-Youth-Risk/vba9-s8jp">CDC data catalog</a>')
    ),
    
    # show different plots
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map Plot", 
                           selectInput("year_to_select",
                                       "Select a Year:",
                                       choices = sort(unique(Year$Year))),
                           plotOutput("map_plot")),
                  tabPanel("Bar Plot", 
                           selectInput("state_to_select",
                                       "Select a State:",
                                       choices = sort(states)),
                           plotOutput("bar_plot", width = "700px")),
                  tabPanel("Error Bar Plot", 
                           
                           plotOutput("errbar_plot",width = "700px"))
)
    ))
)
    



server <- function(input, output) {

    output$map_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        data_to_plot <- tidy_data %>% filter(Year == input$year_to_select)
        
       data_to_plot %>% 
         statebins(state_data = .,
                   state_col = "state",
                   value_col = "Obesity")+
         scale_fill_continuous(name = "Obesity Rate",
                               low = "cornsilk", 
                               high = "cornsilk4") +
         theme(legend.position = "bottom",
               panel.background = element_blank(),
               axis.ticks = element_blank(),
               axis.text = element_blank())+
         ggtitle("Teen Obsity Rate By State") +
         labs(caption = "Note: State that is not showing up
                                                     indicate missing value for the selected year")
    })
    
    output$bar_plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      data_to_plot <- tidy_data %>% filter(Year == "2019") %>% 
        filter(state == input$state_to_select)
      
      data_to_plot<-data_to_plot %>% pivot_longer(cols = c("Less_fruit","Less_veggie","Obesity",
          "Overweight","MV_PE","Daily_PE","Drink_soda","Watch_TV"), names_to = "Measures",
          values_to = "Val")
      
      data_to_plot %>% ggplot(aes(x = Measures, y = Val)) + geom_col(fill = "Orange") +
        scale_x_discrete(labels= c("Daily PE",
                                   "Drink Soda", "Eat little fruit",
                                   "Eat little veggie","Moderate/Vigoruse PE",
                                   "Obesity","Overweight","Watch TV every day"))+
      ggtitle("Percentage of obesity and related factors by state, 2019") + ylab("Percentage Point")+
        xlab("Obsity and related Activties")
    })
    output$errbar_plot <- renderPlot({
      effect("Gender", model) %>%
        data.frame() %>%
        ggplot(aes(x = Gender,
                   y = fit,
                   label = round(fit, digits = 2))) +
        geom_errorbar(aes(ymin = lower,
                          ymax = upper),
                      width = .2) +
        geom_label() + ggtitle("Obesity Rate Comparison Between Male and Female")+
        xlab("Gender") + ylab("Rate") + labs(caption = "
                                             Male obesity rate are 6 percent point greater than female obesity rate")
    })
   
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
