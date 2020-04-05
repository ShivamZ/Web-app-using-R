
library(tidyverse)
library(shiny)
library(readxl)




girl_data <- read_xlsx('Top100_Popular_Baby_Names.xlsx' , sheet = 1)
girl_df <- girl_data[c(7:106) , -c(1,2)]


girl_df <- girl_df[ , apply(girl_df, 2, function(x) !any(is.na(x)))]


boy_data <- read_xlsx('Top100_Popular_Baby_Names.xlsx' , sheet = 2)
bname_df <- boy_data[c(7:106) , -c(1,2)]


bname_df <- bname_df[ , apply(bname_df, 2, function(x) !any(is.na(x)))]




top_g <- data.frame("Rank" = integer(0), 
                             "Name" = character(0),
                             "Frequency" = integer(0),
                             "Year" = integer(0),
                             stringsAsFactors = FALSE)
attributes(top_g)

#  ---->
r1 <- 1
r2 <- 100
c1 <- 1
c2 <- 2

for (year in 1954:2018){
  top_g[r1:r2, 4] <- year
  top_g[r1:r2, 1] <- seq(1,100)
  top_g[r1:r2,c(2,3)] <- girl_df[ ,c(c1,c2)]     ##edited after submission: changed 'girl_df' variable name
  r1 = r1+100
  r2 = r2+100
  c1 = c1+2
  c2 = c2+2
}



top_b <- data.frame("Rank" = integer(0), 
                            "Name" = character(0),
                            "Frequency" = integer(0),
                            "Year" = integer(0),
                            stringsAsFactors = FALSE)


r1_boy <- 1
r2_boy <- 100
c1_boy <- 1
c2_boy <- 2

for (year in 1954:2018){
  top_b[r1_boy:r2_boy, 4] <- year
  top_b[r1_boy:r2_boy, 1] <- seq(1,100)
  top_b[r1_boy:r2_boy,c(2,3)] <- bname_df[ ,c(c1_boy,c2_boy)]
  r1_boy = r1_boy+100
  r2_boy = r2_boy+100
  c1_boy = c1_boy+2
  c2_boy = c2_boy+2
}



unq_yr <- unique(top_b$Year)
unique_girl_names <- unique(top_g$Name)
unique_boy_names <- unique(top_b$Name)



ui <- fluidPage(
  headerPanel(title = "Top 100 Babies Names"),
  mainPanel(
    tabsetPanel(type = "tabs",
              tabPanel("10 most Popular Girl's names", 
                       sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId="Select_Year_Girls",
                                      label="Select Year",
                                      choices = unq_yr,
                                      selected=1)),
                        mainPanel(
                          tableOutput(
                            outputId = "Girls_list"
                         )
                       ))),
              tabPanel("10 most Popular Boy's names",  
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId="Select_Year_Boys",
                                       label="Select Year",
                                       choices = unq_yr,
                                       selected=1)),
                         mainPanel(
                           tableOutput(
                         outputId = "Boys_list"
                         )
                        ))),
              tabPanel("Popularity of Girls' Baby Names", 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId="Select_Girl_Name",
                                       label="Choose Girl Name",
                                       choices = unique_girl_names,
                                       selected=1)),
                         mainPanel(
                           plotOutput(
                         outputId = "Girl_popularity",
                         height = 700,
                         width = 900
                         )
                       ))),
              tabPanel("Popularity of Boys' Baby Names", 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId="boy_yr_seloy_Name",
                                       label="Choose Boy Name",
                                       choices = unique_boy_names,
                                       selected=1)), 
                         mainPanel(
                           plotOutput(
                         outputId = "Boy_popularity",
                         height = 700,
                         width = 900
                         )
                       )))
  )
)
)

server <- function(input, output) {
  
  #Question 1 - Input block for Top_Girl_List
  getGirlsDetails <- reactive({
    grl_yr_sel <- input$Select_Year_Girls
    result_set_girls <- head(filter(top_g , top_g$Year == grl_yr_sel ),10)
    return(result_set_girls)
  })
  
  output$Girls_list <- renderTable(
   getGirlsDetails()
  )
  
  #Q2)
  getBoysDetails <- reactive({
    boy_yr_sel <- input$Select_Year_Boys
    result_set_boys <- head(filter(top_b , top_b$Year == boy_yr_sel ),10)
    return(result_set_boys)
  })
  
  output$Boys_list <- renderTable(
    getBoysDetails()
  )
  
  
  getGirlsName_f <- function(name) {
    new_set_girl <- filter(top_g , top_g$Name == name )
    new_set_girl <- new_set_girl[,c(1,4)]
    return(new_set_girl)
  }
  
  output$Girl_popularity <- renderPlot({
    ggplot(getGirlsName_f(input$Select_Girl_Name), 
           aes(Year, Rank)) +
           geom_bar(stat="identity") +
           xlab("Year") +
           xlim( 1954,2018 ) +
           ylab("Rank") +
           ggtitle("Girl Name Popularity over years")
  })
  
  ##Q4)
   getBoysName_f <- function(name) {
    new_boy <- filter(top_b , top_b$Name == name )
    new_boy <- new_boy[,c(1,4)]
    return(new_boy)
  }
  
  output$Boy_popularity <- renderPlot({
    ggplot(getBoysName_f(input$boy_yr_seloy_Name), 
           aes(Year, Rank)) +
           geom_bar(stat="identity") +
           xlab("Year") +
           xlim( 1954,2018 ) +
           ylab("Rank") +
           ggtitle("Boy Name Popularity over years")
  })
  
}

shinyApp(ui = ui, server = server)

