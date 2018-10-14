library(shiny)
library(shinythemes)
library(markdown)
library(DT)
library(ggplot2)
#setwd("C:/Users/snowf/Desktop/MGMT using R/epicurious-recipes-with-rating-and-nutrition")

# Define UI for app ----
# Add navigation bar
ui <- navbarPage (
  theme = shinytheme('darkly'),
  "Tennis Analytics",
  tabsetPanel(type = "tabs",
              tabPanel (
                "Player Data",
                sidebarLayout(
                  sidebarPanel(
                    # Input: number of meals
                    radioButtons("Gender", label = "Select Tennis Association", c('ATP', 'WTA')),
                    selectizeInput('playername', choices = NULL, label ="Enter Player Name"),
                    dateRangeInput("DateRange",label = "Enter the Date Range",min = "2014-01-01",max = "2018-08-31",startview = "month",start = "2014-01-01",end="2018-08-31"),
                    #checkboxGroupInput("Statistics",label = "Playing Statistics",choices = c("Aces", "Double Faults", "Break Points Won"),selected = NULL,inline = FALSE,width = '100%'),
                    #actionbutton to execute
                    tags$head(tags$style(
                      HTML('#button{background-color:orange}')
                    )),
                    actionButton(inputId = 'button', label = 'GO')
                  ),
                  # Main panel for displaying outputs ----
                  mainPanel(#Add gif for background
                    #Player result
                    tabsetPanel(type = "tabs",
                                tabPanel("Player Information", 
                                         DT::dataTableOutput('table')
                                ),
                                tabPanel("Visualizations", 
                                         h3("Overall Statistics for all players in the selected time range"),
                                         plotOutput("AcePlot"),
                                         plotOutput("BreakpointPlot"),
                                         plotOutput("DoubleFaultPlot"),
                                         plotOutput("GamesWonPlot"),
                                         verbatimTextOutput("summary")),
                                tabPanel("Dataset", DT::dataTableOutput("averagetable"))
                    )
                  )
                )
              ),
              tabPanel("Predictive Analytics",
                       fluidRow(
                         h3("Focus Areas For Skill Improvement of Emerging Players"),
                         h6("Select Player 1 and Player 2 to know their actual winning scores"),
                         h6("Tweak Performance Parameters to calculate impact on winning score"),
                         column(3,
                                h4("Player 1 Information"),
                                selectizeInput('playername1', choices = NULL, label ="Enter Player 1 Name",selected ="Federer, Roger"),
                                column(3,
                                       br(),
                                       h5("Aces"),
                                       h5(textOutput("aces1")),
                                       br(),
                                       h5("Breakpoints"),
                                       h5(textOutput("bp1")),
                                       br(),
                                       h5("Double Faults"),
                                       h5(textOutput("df1")),
                                       br(),
                                       h5("First Serve Points"),
                                       h5(textOutput("fsp1")),
                                       br(),
                                       h5("First Serve Successful"),
                                       h5(textOutput("fss1")),
                                       br(),
                                       h5("Receiver Points Won"),
                                       h5(textOutput("rp1")),
                                       br(),
                                       h5("Second Serve Successful"),
                                       h5(textOutput("scc1")),
                                       br(),
                                       h5("Service Games Won"),
                                       h5(textOutput("sgw1")),
                                       br(),
                                       h5("Tiebreaks Won"),
                                       h5(textOutput("tw1")),
                                       br(),
                                       h3("Actual Score"),
                                       h3(textOutput("aws1"))
                                       
                                ),
                                column(6,offset = 3,
                                       sliderInput('acesval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       sliderInput('bpval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       sliderInput('dfval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       sliderInput('fspval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       sliderInput('fssval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       br(),
                                       sliderInput('rpval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       sliderInput('sccval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       br(),
                                       sliderInput('sgwval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       br(),
                                       sliderInput('twval1', "",  min=1, max=100, value=50, step=1, round=2,width = '100%'),
                                       br(),
                                       br(),
                                       br(),
                                       h3("Predicted Winning Score"),
                                       h3(textOutput("pws1"))
                                ),
                                br()
                         ),
                         column(3,offset = 3,
                                h4("Player 2 Information"),
                                selectizeInput('playername2', choices = NULL, label ="Enter Player 1 Name"),
                                column(3,
                                       br(),
                                       h5("Aces"),
                                       h5(textOutput("aces2")),
                                       br(),
                                       h5("Breakpoints"),
                                       h5(textOutput("bp2")),
                                       br(),
                                       h5("Double Faults"),
                                       h5(textOutput("df2")),
                                       br(),
                                       h5("First Serve Points"),
                                       h5(textOutput("fsp2")),
                                       br(),
                                       h5("First Serve Successful"),
                                       h5(textOutput("fss2")),
                                       br(),
                                       h5("Receiver Points Won"),
                                       h5(textOutput("rp2")),
                                       br(),
                                       h5("Second Serve Successful"),
                                       h5(textOutput("scc2")),
                                       br(),
                                       h5("Service Games Won"),
                                       h5(textOutput("sgw2")),
                                       br(),
                                       h5("Tiebreaks Won"),
                                       h5(textOutput("tw2")),
                                       br(),
                                       h3("Actual Score"),
                                       h3(textOutput("aws2"))
                                ),
                                column(6,offset = 2,
                                       sliderInput('acesval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       sliderInput('bpval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       sliderInput('dfval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       sliderInput('fspval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       sliderInput('fssval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       br(),
                                       sliderInput('rpval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       br(),
                                       sliderInput('sccval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       sliderInput('sgwval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       br(),
                                       sliderInput('twval2', "",  min=1, max=100, value=50, step=1, round=0,width = '100%'),
                                       br(),
                                       br(),
                                       br(),
                                       h3("Predicted Winning Score"),
                                       h3(textOutput("pws2"))
                                )
                         )
                       )
              )
  )
)

# Define server logic for slider examples ----
server <- function(input, output, session) {
  #tab 1 text
  source("pullplayerdata.R")
  choi = unique(data$Player_name.x)
  updateSelectizeInput(session,
                       'playername',
                       choices = choi,
                       server = TRUE)
  
  updateSelectizeInput(session,
                       'playername1',
                       choices = choi,
                       server = TRUE)
  
  updateSelectizeInput(session,
                       'playername2',
                       choices = choi,
                       server = TRUE)
  
  observe({
    # This will change the value of input$partnerName to searchResult()[,1]
    updateSelectizeInput(session,
                         'playername',
                         choices = searchResult(),
                         server = TRUE)
  })
  
  aces1 <- reactive({
    input$acesval1
  })
  bp1 <- reactive({
    input$bpval1
  })
  fsp1 <- reactive({
    input$fspval1
  })
  fss1 <- reactive({
    input$fssval1
  })
  scc1 <- reactive({
    input$sccval1
  })
  sgw1 <- reactive({
    input$sgwval1
  })
  rp1 <- reactive({
    input$rpval1
  })
  tw1 <- reactive({
    input$twval1
  })
  df1 <- reactive({
    input$dfval1
  })
  
  aces2 <- reactive({
    input$acesval2
  })
  bp2 <- reactive({
    input$bpval2
  })
  fsp2 <- reactive({
    input$fspval2
  })
  fss2 <- reactive({
    input$fssval2
  })
  scc2 <- reactive({
    input$sccval2
  })
  sgw2 <- reactive({
    input$sgwval2
  })
  rp2 <- reactive({
    input$rpval2
  })
  tw2 <- reactive({
    input$twval2
  })
  df2 <- reactive({
    input$dfval2
  })
  
  observe({
    df1 <- getplayerinfo(permatch_data,input$playername1)
    df2 <- getplayerinfo(permatch_data,input$playername2)
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "acesval1", value = df1[,'Aces'],
                      min = 0, max = df1[,'Aces']+20, step = 0.5 )
    updateSliderInput(session, "bpval1", value = df1[,'Breakpoints_won'],
                      min = 0, max = df1[,'Aces']+20, step = 0.5)
    updateSliderInput(session, "fspval1", value = df1[,'first_serve_points_won'],
                      min = 0, max = df1[,'first_serve_points_won']+20, step = 0.5)
    updateSliderInput(session, "fssval1", value = df1[,'first_serve_successful'],
                      min = 0, max = df1[,'first_serve_successful']+20, step = 0.5)
    updateSliderInput(session, "sccval1", value = df1[,'second_serve_successful'],
                      min = 0, max = df1[,'second_serve_successful']+20, step = 0.5)
    updateSliderInput(session, "sgwval1", value = df1[,'service_games_won'],
                      min = 0, max = df1[,'service_games_won']+20, step = 0.5)
    updateSliderInput(session, "twval1", value = df1[,'tiebreaks_won'],
                      min = 0, max = df1[,'tiebreaks_won']+20, step = 0.5)
    updateSliderInput(session, "rpval1", value = df1[,'Reciever_points_won'],
                      min = 0, max = df1[,'Reciever_points_won']+20, step = 0.5)
    updateSliderInput(session, "dfval1", value = df1[,'double_faults'],
                      min = 0, max = df1[,'double_faults']+20, step = 0.5)
    
    
    updateSliderInput(session, "acesval2", value = df2[,'Aces'],
                      min = 0, max = df2[,'Aces']+20, step = 0.5)
    updateSliderInput(session, "bpval2", value = df2[,'Breakpoints_won'],
                      min = 0, max = df2[,'Aces']+20, step = 0.5)
    updateSliderInput(session, "fspval2", value = df2[,'first_serve_points_won'],
                      min = 0, max = df1[,'first_serve_points_won']+20, step = 0.5)
    updateSliderInput(session, "fssval2", value = df2[,'first_serve_successful'],
                      min = 0, max = df2[,'first_serve_successful']+20, step = 0.5)
    updateSliderInput(session, "sccval2", value = df2[,'second_serve_successful'],
                      min = 0, max = df2[,'second_serve_successful']+20, step = 0.5)
    updateSliderInput(session, "sgwval2", value = df2[,'service_games_won'],
                      min = 0, max = df2[,'service_games_won']+20, step = 0.5)
    updateSliderInput(session, "twval2", value = df2[,'tiebreaks_won'],
                      min = 0, max = df2[,'tiebreaks_won']+20, step = 0.5)
    
    updateSliderInput(session, "rpval2", value = df2[,'Reciever_points_won'],
                      min = 0, max = df2[,'Reciever_points_won']+20, step = 0.5)
    updateSliderInput(session, "dfval2", value = df2[,'double_faults'],
                      min = 0, max = df2[,'double_faults']+20, step = 0.5)
    
    
  })
  
  searchResult<- reactive({
    if (input$Gender == 'ATP'){
      gender <- "ATP"
      choi = unique(data[data$Tournament_Gender == 'men', ]$Player_name.x)
    }
    else {
      gender <- "WTA"
      choi = unique(data[data$Tournament_Gender == 'women', ]$Player_name.x)
    }
    choi
  })
  
  permatch_perplayerdata1 <- reactive({
    getplayerinfo(permatch_data,input$playername1)
  }
  )
  permatch_perplayerdata2 <- reactive({
    getplayerinfo(permatch_data,input$playername2)
  }
  )
  
  averageddata <- reactive({
    updateSelectizeInput(session,
                         'playername',
                         choices = choi,
                         server = TRUE)
    getAveragedData(player_data,input$Gender,input$DateRange[1],input$DateRange[2])
  })
  
  averageddata <- eventReactive(input$button, {
    getAveragedData(player_data,input$Gender,input$DateRange[1],input$DateRange[2])
    
  })
  
  playerdata <- eventReactive(input$button, {
    playerdt(input$playername,input$DateRange[1],input$DateRange[2])
    
  })
  
  
  output$averagetable <- DT::renderDataTable({
    datatable(averageddata())%>%formatStyle(colnames(averageddata()),
                                color = "black"
                                 )
  }, escape = FALSE, options = list(lengthChange = FALSE))
  
  
  output$table <- DT::renderDataTable({
    datatable(playerdata())%>%formatStyle(colnames(playerdata()),
                                          color = "black")
  }, escape = FALSE, options = list(lengthChange = FALSE))
  
  output$AcePlot <- renderPlot(
    { 
      avg <- averageddata()
      x    <-   avg$Aces.x
      bins <- seq(min(x), max(x), length.out = 30)
      #hist(x, breaks = bins, col = 'darkgray', border = 'black', main ="Histogram of AcePlots")
      ggplot(avg, aes(x=Aces.x))+
        geom_density(color="darkblue", fill="lightblue")+
        geom_vline(xintercept = avg[avg$Player_name.x==input$playername,'Aces.x'])+
        scale_x_continuous(breaks=avg[avg$Player_name.x==input$playername,'Aces.x'], labels=input$playername)+
        xlab("Aces") + ylab("Density")
      
    }
  )
  output$BreakpointPlot <- renderPlot(
    { 
      avg <- averageddata()
      ggplot(avg, aes(x=Breakpoints_won.x))+
        geom_density(color="darkblue", fill="lightblue") +
        geom_vline(xintercept = avg[avg$Player_name.x==input$playername,'Breakpoints_won.x'])+
        scale_x_continuous(breaks=avg[avg$Player_name.x==input$playername,'Breakpoints_won.x'], labels=input$playername)+
        xlab("BreakPoints Won") + ylab("Density")
      
    }
  )
  output$DoubleFaultPlot <- renderPlot(
    
    { 
      avg <- averageddata()
      x    <- avg$double_faults.x
      bins <- seq(min(x), max(x), length.out = 30)
      # hist(x, breaks = bins, col = 'darkgray', border = 'black',main ="Histogram of Double Faults")
      ggplot(avg, aes(x=double_faults.x))+
        geom_density(color="darkblue", fill="lightblue")+
        geom_vline(xintercept = avg[avg$Player_name.x==input$playername,'double_faults.x'])+
        scale_x_continuous(breaks=avg[avg$Player_name.x==input$playername,'double_faults.x'], labels=input$playername)+
        xlab("Double Faults") + ylab("Density")
      
    }
  )  
  output$GamesWonPlot <- renderPlot(
    { 
      avg <- averageddata()
      x    <- avg$Games_won.x
      bins <- seq(min(x), max(x), length.out = 30)
      #hist(x, breaks = bins, col = 'darkgray', border = 'black')
      ggplot(avg, aes(x=Games_won.x))+
        geom_density(color="darkblue", fill="lightblue")+
        geom_vline(xintercept = avg[avg$Player_name.x==input$playername,'Games_won.x'])+
        scale_x_continuous(breaks=avg[avg$Player_name.x==input$playername,'Games_won.x'], labels=input$playername)+
        xlab("Games Won") + ylab("Density")
      
    }
  )
  output$aces1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'Aces']
  })
  output$bp1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'Breakpoints_won']
  })
  output$df1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'double_faults']
  })
  output$df2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'double_faults']
  })
  output$fsp1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'first_serve_points_won']
  })
  output$fss1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'first_serve_successful']
  })
  output$scc1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'second_serve_successful']
  })
  output$sgw1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'service_games_won']
  })
  output$tw1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'tiebreaks_won']  })
  output$aces2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'Aces']
  })
  output$bp2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'Breakpoints_won']
  })
  output$fsp2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'first_serve_points_won']
  })
  output$fss2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'first_serve_successful']
  })
  output$scc2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'second_serve_successful']
  })
  output$sgw2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'service_games_won']
  })
  output$tw2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'tiebreaks_won']  })
  
  output$rp1 <- renderText({ 
    df <- permatch_perplayerdata1()
    df[,'Reciever_points_won']  })
  output$rp2 <- renderText({ 
    df <- permatch_perplayerdata2()
    df[,'Reciever_points_won']  })
  output$aws2 <- renderText({ 
    df <- permatch_perplayerdata2()
    pred_gamespermatch(df[,'Aces'],df[,'Breakpoints_won'],df[,'double_faults'],df[,'first_serve_points_won'],df[,'first_serve_successful'],df[,'Reciever_points_won']
                       ,df[,'second_serve_successful'] , df[,'service_games_won'] , df[,'tiebreaks_won']
    )
  })
  output$aws1 <- renderText({ 
    df <- permatch_perplayerdata1()
    pred_gamespermatch(df[,'Aces'],df[,'Breakpoints_won'],df[,'double_faults'],df[,'first_serve_points_won'],df[,'first_serve_successful'],df[,'Reciever_points_won']
                       ,df[,'second_serve_successful'] , df[,'service_games_won'] , df[,'tiebreaks_won']
    )
  })
  output$pws1 <- renderText((
    pred_gamespermatch(aces1(),bp1(),df1(),fsp1(),fss1(),rp1(),scc1(),sgw1(),tw1())
  ))
  output$pws2 <- renderText((
    pred_gamespermatch(aces2(),bp2(),df2(),fsp2(),fss2(),rp2(),scc2(),sgw2(),tw2())
  ))
}



# Create Shiny app ----
shinyApp(ui = ui, server = server)
