#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
inddeath = read.csv("Industrydeath.csv")

ui <-  dashboardPage(
  dashboardHeader(title="Industry death in different countries"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the dataset",tabName = "menu3"),
      menuItem("Univariate Plots",tabName = "menu1"),
      menuItem("Bivarite Plots",tabName = "menu2")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("menu1",h1("Univariate Plots"),fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput("coloumn","choose the variable",choices = c("Countries",
                                                                    "Locality",
                                                                    "Industry sector",
                                                                    "Accident level",
                                                                    "Potential Accident Level",
                                                                    "Gender",
                                                                    "Employment",
                                                                    "Critical Risk")),
            actionButton("go","apply")
          ),
          mainPanel(plotOutput("chart"),textOutput("write"))
        )
      )),
      tabItem("menu2",h1("Bivarite Plots"),fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput("coloumn1","choose the bivarte plots",choice = c("Countries vs Localities",
                                                                         "Countries vs Industry sector",
                                                                         "Countries vs Accident level",
                                                                         "Countries vs Potential Accident Level",
                                                                         "Countries vs Gender",
                                                                         "Countries vs Employment",
                                                                         "Countries vs Critical Risk",
                                                                         "Locality vs Industry Sector",
                                                                         "Locality vs Accident level",
                                                                         "Locality vs Potential Accident Level",
                                                                         "Locality vs Gender",
                                                                         "Locality vs Employment",
                                                                         "Locality vs Critical Risk",
                                                                         "Industry sector vs Accident Level",
                                                                         "Industry sector vs Potential Accident Level",
                                                                         "Industry sector vs Gender",
                                                                         "Industry sector vs Employment",
                                                                         "Industry sector vs Critical Risk",
                                                                         "Accident vs Potential Accident Level",
                                                                         "Accident vs Gender",
                                                                         "Accident vs Employment",
                                                                         "Accident vs Critical Risk",
                                                                         "Potential Accidental Level vs gender",
                                                                         "Potential Accident Level vs Critical Risk",
                                                                         "Potential Accident Level vs Employment",
                                                                         "Gender vs Employment",
                                                                         "Gender vs Critical Risk",
                                                                         "Employment vs Critical Risk")),
            actionButton("go1","Apply")
          ),
          mainPanel(plotlyOutput("chart1"),verbatimTextOutput("write1"))
        )
      )),
      tabItem("menu3",h1("About the Project"),fluidPage(
        tags$b(tags$h2("About the Dataset:")),
        tags$ul(tags$h4("Labor death is a very common issue in industrial field. The following dataset shows the death in different deaths in different regions in different countries.
The different variables present in the data set is shown below:")),
        tags$ul(tags$h4("X: It is the index of the given dataset")),
        tags$ul(tags$h4("Data: The dates of accidents")),
        tags$ul(tags$h4("Countries: Countries where the accident occured")),
        tags$ul(tags$h4("Local: The locality where accident occured")),
        tags$ul(tags$h4("Industry.Sector:The industry type where the accident occured")),
        tags$ul(tags$h4("Accident.Level: The level of accident occured")),
        tags$ul(tags$h4("Potential.Accident.Level : Estimated level of accident")),
        tags$ul(tags$h4("Genre: Gender of the victim")),
        tags$ul(tags$h4("Employee.or.Third.Party: Victim's organisation status")),
        tags$ul(tags$h4("Critical risk: Condition of the victim"
        )), 
        tags$b(tags$h2("What I want to find from this project:")),
        tags$ul(tags$h4("We here are trying to find in which country and locality the deaths occured most; which industry has the most cases of death, how do countries, industries and deaths depend on one another etc.")),
        tags$b(tags$h2("Approach:")),
        tags$b(tags$h4("Let us plot some graphs:"))
      ))
      
    )
  )
)
server <- function(input,output){
  observeEvent(input$go,
               if(input$coloumn=="Countries"){
                 output$chart<-renderPlot({
                   library(ggplot2)
                   ggplot(inddeath,aes(x= Countries, fill = Countries))+geom_bar(color = "white", width = 1)                 })
                 
               }
               else if(input$coloumn=="Locality"){
                 output$chart<-renderPlot({
                   ggplot(data = inddeath,aes(x= Local, fill = Local))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn=="Industry sector"){
                 output$chart<-renderPlot({
                   ggplot(data = inddeath,aes(x= Industry.Sector, fill = Industry.Sector))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn=="Accident level"){
                 output$chart<-renderPlot({
                   ggplot(inddeath,aes(x= Accident.Level,fill = Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn=="Potential Accident Level"){
                 output$chart<-renderPlot({
                   ggplot(data = inddeath,aes(x= Potential.Accident.Level,fill = Potential.Accident.Level))+
                     geom_bar()
                 })
                 
                 
               }
               else if(input$coloumn=="Gender"){
                 output$chart<-renderPlot({
                   ggplot(data = inddeath,aes(x= Genre,fill = Genre))+
                     geom_bar()
                 })
               }
               else if(input$coloumn=="Employment"){
                 output$chart<-renderPlot({
                   ggplot(data = inddeath,aes(x= Employee.or.Third.Party,fill = Employee.or.Third.Party))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn=="Critical Risk"){
                 output$chart<-renderPlot({
                   ggplot(data = inddeath,aes(x= Critical.Risk,fill = Critical.Risk))+
                     geom_bar(width = 0.5)+
                     coord_flip()
                 })
                 
               }
  )
  
  observeEvent(input$go1,
               if(input$coloumn1=="Countries vs Localities"){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Local))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Countries vs Industry sector" ){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Industry.Sector))+
                     geom_bar()
                   
                   
                 })
               }
               else if(input$coloumn1=="Countries vs Accident level"){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Countries vs Potential Accident Level" ){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Potential.Accident.Level))+
                     geom_bar()})
               }
               else if(input$coloumn1=="Countries vs Gender" ){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Genre))+
                     geom_bar()})
               }
               else if(input$coloumn1=="Countries vs Employment" ){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Employee.or.Third.Party ))+
                     geom_bar()})
               }
               else if(input$coloumn1=="Countries vs Critical Risk" ){
                 output$chart1<-renderPlotly({
                   ggplot(inddeath, aes(x = Countries, fill = Critical.Risk ))+
                     geom_bar()})
               }
               else if(input$coloumn1=="Locality vs Industry Sector"){
                 output$chart1<-renderPlotly({
                   ggplot(data = inddeath,aes(x= Local,fill = Industry.Sector))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Locality vs Accident level"){
                 output$chart1<-renderPlotly({
                   ggplot(data = inddeath,aes(x= Local,fill = Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Locality vs Potential Accident Level"){
                 output$chart1<-renderPlotly({
                   ggplot(data = inddeath,aes(x= Local,fill = Potential.Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Locality vs Gender"){
                 output$chart1<-renderPlotly({
                   ggplot(data = inddeath,aes(x= Local,fill = Genre))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Locality vs Employment"){
                 output$chart1<-renderPlotly({
                   ggplot(data = inddeath,aes(x= Local,fill = Employee.or.Third.Party))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Locality vs Critical Risk"){
                 output$chart1<-renderPlotly({
                   ggplot(data = inddeath,aes(x= Local,fill = Critical.Risk))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Industry sector vs Accident Level"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Industry.Sector,fill = Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Industry sector vs Potential Accident Level"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Industry.Sector,fill = Potential.Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Industry sector vs Gender"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Industry.Sector,fill = Genre))+
                     geom_bar()
                 })
                 output$write1<-renderText({
                   print("The tendency of fleeing in different races.")
                 })
               }
               else if(input$coloumn1=="Industry sector vs Employment"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Industry.Sector,fill = Employee.or.Third.Party))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Industry sector vs Critical Risk"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Industry.Sector,fill = Critical.Risk))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Accident vs Potential Accident Level"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Accident.Level, fill = Potential.Accident.Level))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Accident vs Gender"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Accident.Level, fill = Genre))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn1=="Accident vs Employment"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Accident.Level, fill = Employee.or.Third.Party))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn1=="Accident vs Critical Risk"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Accident.Level, fill = Critical.Risk))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn1=="Potential Accidental Level vs gender"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Potential.Accident.Level, fill = Genre))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn1=="Potential Accident Level vs Employment"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Potential.Accident.Level, fill = Employee.or.Third.Party))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn1=="Potential Accident Level vs Critical Risk"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Potential.Accident.Level, fill = Critical.Risk))+
                     geom_bar()
                 })
                 
               }
               else if(input$coloumn1=="Gender vs Employment"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Genre,fill = Employee.or.Third.Party))+
                     geom_bar()
                 })
                 output$write1<-renderText({
                   print("Mental stability of the armed people.")
                 })
               }
               else if(input$coloumn1=="Gender vs Critical Risk"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Genre,fill = Critical.Risk))+
                     geom_bar()
                 })
               }
               else if(input$coloumn1=="Employment vs Critical Risk"){
                 output$chart1<- renderPlotly({
                   ggplot(data = inddeath,aes(x= Employee.or.Third.Party,fill = Critical.Risk))+
                     geom_bar()
                 })
               }
  )
}

shinyApp(ui = ui, server = server)