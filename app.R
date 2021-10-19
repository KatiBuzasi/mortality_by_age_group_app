# Libraries----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(scales)
library(ggplot2)
library(dashboardthemes)
library(shinythemes)
library(viridis)
library(readxl)

data1 <- readRDS("data_tab1_new.rds")
data3 <- readRDS("EWDI.rds")
data4 <- readRDS("data_tab3_1.rds")

ui <- shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  
  titlePanel(h1(strong("Mortality in Amsterdam by age group, 1856-1926"))),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tab_selected==1", h4("Introduction to app features")),
      conditionalPanel(style = "font-size: 16px", condition = "input.tab_selected==2", checkboxGroupInput(inputId = "groups", label = "Select age groups", choices = sort(unique(data1$age_group)), selected = "total")),
      conditionalPanel(style = "font-size: 16px", condition = "input.tab_selected==3", radioButtons(inputId = "group", label = "Select age group", choices = sort(unique(data4$age_group)), selected = "total"),
                       sliderInput("range", "Period", min = 1856, max=1926, value=c(1856, 1926))),
    ),
    
    mainPanel(
      
      tabsetPanel(
        type="tabs",
        id="tab_selected",
        selected=1,
        tabPanel(title="Documentation", value = 1,
                 h2("Introduction"),
                 p("The aim of this web application is to explore mortality and its seasonality by age group in Amsterdam, the Netherlands, between 1856 and 1926. The app is developed as part of the research project",
                   a(em("Lifting the burden of disease. The modernisation of health in the Netherlands: Amsterdam 1854-1940."), href="https://www.ru.nl/rich/our-research/research-groups/radboud-group-historical-demography-family-history/current-research-projects/current-projects/lifting-burden-disease/"),
                   "Information on death is obtained from the Amsterdam Cause-of-death Database (ACD), population data are from the decennial censuses available",
                   a(em("here,"), href="http://www.volkstellingen.nl/nl/index.html"),
                   "and the number of live births are taken from the",
                   a(em("Statistical Yearbooks of Amsterdam."), href="https://data.amsterdam.nl/publicaties/zoek/?filters=date%3Bdate%3Aolder-2001"),
                   style = "font-size: 110%"
                 ),
                 h2("Tabs"),
                 h3("Counts and age-specific death rates"),
                 p("On this tab you can explore the development of the annual death count and age-specific death rates by age group. 
                   Stillbirth rate is the number of stillbirths divided by the number of total births (stillbirths+live births) times 1,000. 
                   Infant death rate is calculated as the number of deaths among those less than one year old divided by the number of live births times 1,000.
                   The death rate in the remaining groups is calculated as the number of deaths in the respective age group divided by the number of population in that age group times 1,000. 
                   Since multiple groups are allowed to be selected, differences between age groups and their changes over time can be explored. Beyond the raw numbers, the graphs also include smoothed values. ",
                   style = "font-size: 110%"),
                 h3("Seasonality"),
                 p("On this tab you can analyse the seasonality of mortality in the selected age group. You can also specify a period, which enables you to zoom in on specific time spans. 
                   The excess winter death index (EWDI), shown in the first graph, is the difference between the number of winter deaths (between December and March)  minus the average number of deaths in the two non-winter periods (August to November and April to July) as the percentage of the latter average. 
                   The second graph depicts the share of months in the total death counts by year. The monthly death counts are adjusted for month length. 
                   Bear in mind that if you select a period shorter than 15 years, standard errors around the smoothed EWDI line cannot be calculated due to insufficient number of observations.", 
                   style = "font-size: 110%" 
                 ),
                 
                 h2("Additional information"),
                 p("The death counts are missing for the following years: 1892, 1893, 1894, 1899 and 1921, hence the gaps in the line graphs.", style = "font-size: 110%"),
                 h2("Useful links"),
                 p("The codes for the web app are available at", a(em("https://github.com/KatiBuzasi"), href="https://github.com/KatiBuzasi"), style="font-size: 110%"),
                 a(em("Dood in Amsterdam"), href="https://doodinamsterdam.nl/", style="font-size: 110%"),
                 br(),
                 a(em("RG Citizen Science for the History of Health"), href="https://www.ru.nl/rich/our-research/research-groups/radboud-group-historical-demography-family-history/current-research-projects/current-projects/rg-citizen-science-history-health/", style="font-size: 110%"),
                 h2("Contact"),
                 p(em("Katalin Buzasi:"), "katalin.buzasi@ru.nl", style="font-size: 110%"),
                 icon = icon("question-circle")),
        tabPanel(title="Counts and age-specific death rates", fluidRow(plotOutput("plot_count"), plotOutput("plot_cdr")), value=2),
        tabPanel(title="Seasonality", fluidRow(plotOutput("plot_ewdi"), plotOutput("plot_monthshare")), value=3)
      )
    )
  )
)
)


shinyServer(server <- function(input, output){
  
  # data cleaning
  prepped_data1 <- reactive({
    clean_data1 <- data1 %>%
      filter( age_group %in% input$groups) %>%
      arrange(year)
  })
  
  
  prepped_data3 <- reactive({
    clean_data3 <- data3 %>%
      filter( age_group %in% input$group) %>%
      filter(year>=input$range[1] & year<=input$range[2]) %>%
      arrange(year)
  })
  
  prepped_data4 <- reactive({
    clean_data4 <- data4 %>%
      filter( age_group %in% input$group) %>%
      filter(year>=input$range[1] & year<=input$range[2]) %>%
      arrange(year)
  })
  
  output$plot_count <- renderPlot({
    ggplot( data = prepped_data1(), aes(y = count, x = year, color=age_group, group=age_group) ) +
      geom_line(size = 1.5) +
      geom_smooth(se=TRUE, span=0.4)+
      #ylab( metric_names[which(metric_choices == input$metric)] ) +
      labs(color="age group") +
      xlab("")+
      scale_y_continuous( label = comma ) +
      scale_x_continuous(breaks=seq(1860,1920,10))+
      theme(text = element_text(size=16), legend.position = "right") +
      ggtitle("Total death count")
  })
  
  output$plot_cdr <- renderPlot({
    ggplot( data = prepped_data1(), aes(y = CDR, x = year, color=age_group, group=age_group) ) +
      geom_line(size = 1.5) +
      geom_smooth(se=TRUE, span=0.4)+
      ylab( "by 1,000" ) +
      xlab("")+
      labs(color="age group") +
      scale_y_continuous( label = comma ) +
      scale_x_continuous(breaks=seq(1860,1920,10))+
      theme(text = element_text(size=16), legend.position = "right") +
      ggtitle("Age-specific death rates" )
  })
  
  
  output$plot_ewdi <- renderPlot({
    ggplot(data=prepped_data3(), aes(x=year, y=ewdi)) + 
      geom_line() +
      geom_smooth(se=TRUE, span=0.4)+
      geom_hline(yintercept=0, linetype="dashed")+
      scale_x_continuous(breaks=seq(1860,1920,10))+
      #theme_light()+
      theme(text = element_text(size=16), legend.position = "bottom") +
      labs(x="",y="in %", title = "Excess Winter Death Index")
  })
  
  output$plot_monthshare <- renderPlot({
    ggplot(data=prepped_data4(), aes(x=month_str, y=share, group=year)) +
      geom_line(aes(colour=year), size=1.1) +
      scale_color_continuous(type = "viridis") +
      #theme_light()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size=16)) +
      #ylim(4.5,20)+
      labs(x="",y="in %", title="Monthly death count shares by year")
  })
  
  
})

shinyApp(ui, server)


