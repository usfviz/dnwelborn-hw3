#setwd("~/Documents/msan622_data_visualization/hw/Homework 3")
cat('\014')
rm(list = ls())
library(ggplot2)
library(shiny)
library(reshape2)
library(GGally )
library(plotly)

facebook <- read.csv('dataset_Facebook.csv',sep = ';')

colnames(facebook) = c('Total_Likes', 'Type', 'Category', 'Month', 'Day',
                       'Hour', 'Paid', 'Reach', 'Impressions', 'Users',
                       'Consumers', 'Consumptions', 'Impressions_by_Likers',
                       'Reach_by_Likers', 'No_Liked_and_Engaged', 'comment',
                       'like', 'share', 'Interactions')

month_week_interactions <- aggregate(cbind(like,share,comment) ~ Day + Month, data = facebook, FUN = mean)
month_week_interactions$Day <- as.factor(month_week_interactions$Day)
month_week_interactions$Month <- as.factor(month_week_interactions$Month)




ui <- fluidPage(
  headerPanel('Derek Welborn: Data Viz Master'),
  mainPanel(
    tabsetPanel(
      tabPanel("Bubble Plot", plotlyOutput("Plot_1"),selectizeInput("month", "Select month to view:",
                                                                    choices = c("All", "January", "February",
                                                                                "March", "April", "May",
                                                                                "June", "July", "August",
                                                                                "September", "October", "November",
                                                                                "December"))),
      tabPanel("Small Multiples",plotlyOutput("Plot_2", width = "100%") ,
               sidebarPanel(
                 selectInput('var', 'Y Variable', c('Likes','Comments','Shares')))),
      tabPanel("Parallel Coordinates Plot", plotlyOutput("Plot_3"),selectizeInput("day", "Select day to view:",
                                                                                 choices = c("All", "Monday", "Tuesday",
                                                                                             "Wednesday", "Thursday", "Friday",
                                                                                             "Saturday", "Sunday"))))#,
      
      # sliderInput("day", "Day:", min=1, max=7,
      #           step=1, value = 1, animate = animationOptions(interval=1000), sep="", width = 550)
  )
)
server <- function(input, output) {
  
  #final<- reactive(subset(facebook, Day == input$day))
  final_1 <- reactive({
    switch(input$month,
           
           "All" = facebook,
           "January" = facebook[facebook$Month == 1, ],
           "February" = facebook[facebook$Month == 2, ],
           "March" = facebook[facebook$Month == 3, ],
           "April" = facebook[facebook$Month == 4, ],
           "May" = facebook[facebook$Month == 5, ],
           "June" = facebook[facebook$Month == 6, ],
           "July" = facebook[facebook$Month == 7, ],
           "August" = facebook[facebook$Month == 8, ],
           "September" = facebook[facebook$Month == 9, ],
           "October" = facebook[facebook$Month == 10, ],
           "November" = facebook[facebook$Month == 11, ],
           "December" = facebook[facebook$Month == 12, ]
    )})
  final_2 <- reactive({
    switch(input$day,
           
           "All" = facebook,
           "Monday" = facebook[facebook$Day == 1, ],
           "Tuesday" = facebook[facebook$Day == 2, ],
           "Wednesday" = facebook[facebook$Day == 3, ],
           "Thursday" = facebook[facebook$Day == 4, ],
           "Friday" = facebook[facebook$Day == 5, ],
           "Saturday" = facebook[facebook$Day == 6, ],
           "Sunday" = facebook[facebook$Day == 7, ]
    )
  })
    

  output$Plot_1 <- renderPlotly({
    plt <- ggplot(final_1(),aes(x=Reach, y=Impressions))
    plt <- plt + geom_point(aes(colour = Type, size = Interactions)) 
    plt <- plt + xlab('Lifetime Post Total Impressions') 
    plt <- plt + ylab('Lifetime Post Total Reach') + theme_bw() + theme(legend.title = element_blank())
    plt <- plt + scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a'))
    plt <- plt+xlim(0, 25000) + ylim(0, 24000)
  }) 
  
  # output$Plot_2 <- renderPlot({
  #   ggpairs(data=final(),columns=c('Reach','Impressions',
  #                                  'Lifetime.Engaged.Users','Interactions'))
  #   }) 
  
  output$Plot_2 <- renderPlotly({
    y_ax <- reactive({
      if ( "Likes" %in% input$var) return(month_week_interactions$like)
      if ( "Shares" %in% input$var) return(month_week_interactions$share)
      if ( "Comments" %in% input$var) return(month_week_interactions$comment)})
    plt <- ggplot(month_week_interactions, aes(y=y_ax(), x=Day))
    plt <- plt + geom_bar(stat = 'identity', aes(fill = Day))
    plt <- plt + facet_wrap(~Month) 
    plt <- plt + ggtitle("Small Multiples in R")
    plt <- plt + ylab('') + theme_bw() + theme(legend.title = element_blank(), axis.title.x=element_text(vjust=0.9, size = 12)) + xlab('\n \n \n Day of the Week')
    plt <- plt + theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color='#555555')) 
    plt <- plt + scale_fill_manual(values = c('#fbb4ae','#b3cde3','#ccebc5','#decbe4','#fed9a6','#ffffcc','#e5d8bd'))
    # plt <- plt + theme(axis.text.x = element_text(angle=90)) 
  })
  
  output$Plot_3 <- renderPlotly({
    plt <- ggparcoord(data = final_2(), columns = c(8,9,10,19), groupColumn = 'Type', scale = 'center')
    plt <- plt + theme(panel.background = element_rect(fill="white",color = "black", size = 0.5),
                       panel.grid.major = element_blank(), axis.title.y = element_blank())
    ggplotly(plt, tooltip = c('colour'))
  }) 
  
}
shinyApp(ui = ui, server = server)
