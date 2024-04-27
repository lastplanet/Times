#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)

# run script update_mydata.R if new data
mydata=read.csv("mydata.csv",stringsAsFactors = FALSE)
#mydata=na.omit(mydata)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
  
  # Application title
  titlePanel("Sprint Kayak and Canoe Times"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('boat', 'Boat', sort(unique(as.character(mydata$boat))), selectize=TRUE,selected="K1"),
      selectInput('event', 'Distance', 
                  c("Select Distance", sort(unique(mydata$event))), 
                  selectize=TRUE, 
                  multiple=TRUE),
      checkboxGroupInput("type", "Types of records to include:",
                         choices = c('no info' = "null",
                                     'time-trial' = "Time-trial",
                                     heat = "Heat",
                                     'semi-final' = "Semis",
                                     final = "Final",
                                     'icf certified only' = "certified"),
                         selected = c("Time-trial","null","Heat","Semis","Final"), inline=TRUE),
      selectInput('name', 'Name', 
                  sort(unique(as.character(mydata$name))), 
                  multiple=TRUE, selectize=TRUE),
      selectInput('venue', 'Venues', 
                  sort(unique(as.character(mydata$venue))), 
                  multiple=TRUE, selectize=TRUE, 
                  selected=sort(unique(as.character(mydata$venue))))
    ),
    
    # Show a plot of the generated distribution
    #plotOutput("distPlot", height = "6in"),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot: Results vs Age", plotOutput("agePlot")),
        tabPanel("Plot: Results vs Year", plotOutput("yearPlot")),
        tabPanel("Results table", tableOutput("table"))
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    boat <- input$boat
    event <- input$event
    name <- input$name
    venue <- input$venue
    s_mydata <- mydata[ mydata$boat==boat,]
    if(!is.null(event)) s_mydata <- s_mydata[ s_mydata$event%in%event, ]
    s_name <- sort(unique(as.character(s_mydata$name)))
    
    # Text =====================================================
    # Change both the label and the text
    if(!all(name%in%s_name)) name=NULL
    updateSelectInput(session, "name", 
                      label = paste("Select", boat, "name (click on box)"),
                      choices = s_name, selected = name)
  }, priority=20)
  
  observe({
    boat <- input$boat
    event <- input$event
    name <- input$name
    venue <- isolate(input$venue)
    good <- sapply(mydata$type, function(x){any(str_detect(x,input$type))})
    if("null"%in%input$type) good <- good | mydata$type==""
    s_mydata <- mydata[mydata$boat==boat & good,]
    if(!is.null(event)) s_mydata <- s_mydata[ s_mydata$event%in%event, ]
    if(!is.null(name)) s_mydata <- s_mydata[s_mydata$name%in%name,]
    v_name <- sort(unique(as.character(s_mydata$venue)))
    
    updateSelectInput(session, "venue", 
                      label = paste("Venues (select and delete to remove)"),
                      choices = v_name, selected = v_name)
  }, priority=10)
  
  observe({
    boat <- input$boat
    event <- isolate(input$event)
    name <- input$name
    venue <- input$venue
    good <- sapply(mydata$type, function(x){any(str_detect(x,input$type))})
    if("null"%in%input$type) good <- good | mydata$type==""
    s_mydata <- mydata[ mydata$boat==boat & mydata$venue%in%venue & good,]
     if(!is.null(name)) s_mydata <- s_mydata[s_mydata$name%in%name,]
    avail_val <- sort(unique(s_mydata$event))
    sel_val = event
    if(!is.null(event)){ 
      if(!any(event %in% avail_val)){
        sel_val = NULL
      }else{ 
        sel_val = event[event%in%avail_val]
        }
      }
    updateSelectInput(session, "event", 
                      label = "Select Distance(s)",
                      choices = avail_val, selected = sel_val )
  }, priority=10)
  
  
  output$agePlot <- renderPlot({
    # subset data based on boat, event and age
    yesPlot = TRUE; #flag
    c_name <- input$name
    if(is.null(input$event)){ 
      gNoPlot = textGrob("Select a distance to plot")
      grid.arrange(gNoPlot)
    }else{
      x <- subset(mydata, boat==input$boat & event%in%input$event)
      if(nrow(x)==0) yesPlot = FALSE
      if(yesPlot){
        if(all(c_name%in%x$name) & !is.null(c_name)) x <- subset(x, name %in% c_name)
        # now remove any names without an age
        noage <- unique(x$name[is.na(x$age)])
        x <- x[!is.na(x$age),]
        if(nrow(x)==0) yesPlot = FALSE
      }
      # now remove any results that are not the right type (spec'd in check boxes by user)
      if(yesPlot){
        good <- sapply(x$type, function(x){any(str_detect(x,input$type))})
        if("null"%in%input$type) good <- good | x$type==""
        x <- subset(x, venue%in%input$venue & good)
        if(nrow(x)==0) yesPlot = FALSE
      }
      # now make plot if there is anything to plot
      if(yesPlot){
        g1 = ggplot(x, aes(x=age, y=time, color=name)) +
          geom_point() + 
          theme(legend.position="none",
                panel.grid.minor = element_line(colour="white", size=0.5)) +
          scale_y_continuous(minor_breaks = seq(0 , max(mydata$time,na.rm=TRUE), ifelse(identical(input$event,200),1,5))) + 
          scale_x_continuous(minor_breaks = seq(0,100,1),limits=c(12,29)) +
          xlab("age (years)") + ylab("time (seconds)")
        g2 = ggplot(x, aes(x=age, y=time, color=name)) + geom_point() +
          theme(legend.position="bottom")
        gA = ggplotGrob(g1)
        g_legend<-function(a.gplot){ 
          tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
          leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
          legend <- tmp$grobs[[leg]] 
          return(legend)} 
        nullg = nullGrob()
        if(any(c_name%in%x$name) & !is.null(c_name)){
          legend <- g_legend(g2)
          lheight=sum(legend$height)
          heights=unit.c(unit(4, "in"), lheight, unit(2, "in")-lheight)
        }else{
          legend = nullGrob()
          heights=unit.c(unit(4, "in"), unit(0, "in"), unit(2, "in"))
        }
        # if some names removed because no age, then post a warning.  Only do if some name selected
        if(length(noage)!=0 & !is.null(c_name)){
          gWarning = textGrob(paste0(
            "The following names have no age info and have been removed:\n",
            paste(noage, collapse=", "),"."))
          heights = unit.c(gWarning$x, heights)
          #grid.arrange(gWarning, gA, legend, nullg, heights=heights, ncol=1, nrow=length(heights))
          grid.arrange(gA, bottom=legend, top=gWarning, heights=unit(4,"in"))
        }else{
          #grid.arrange(gA, legend, nullg, heights=heights, ncol=1, nrow=length(heights))
          grid.arrange(gA, bottom=legend, heights=unit(4,"in"))
        }
      }else{ #yesPlot is FALSE
        # say why nothing is plotted
        gNoPlot = textGrob("Nothing to plot. No athletes with age data or no results of the type selected.\n\n")
        if(length(noage)!=0){
          gWarning = textGrob(paste0(
            "\nThe following names have no age info and have been removed:\n",
            paste(noage, collapse=", "),"."))
        }else{
          gWarning = nullGrob()
        }
        grid.arrange(gNoPlot,gWarning,ncol=1)
      } 
    }
    # code to add labels
    # require("ggrepel")
    # geom_label_repel(aes(label=x$venue, fill=x$name)), color='white', size=3.5)
  })
  
  output$yearPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    yesPlot = TRUE; #flag
    c_name <- input$name
    if(is.null(input$event)){ 
      gNoPlot = textGrob("Select a distance to plot")
      grid.arrange(gNoPlot)
    }else{
      x <- subset(mydata, boat==input$boat & event%in%input$event)
      if(nrow(x)==0) yesPlot = FALSE
      if(yesPlot){
        if(all(c_name%in%x$name) & !is.null(c_name)) x <- subset(x, name %in% c_name)
        good <- sapply(x$type, function(x){any(str_detect(x,input$type))})
        if("null"%in%input$type) good <- good | x$type==""
        x <- subset(x, venue%in%input$venue & good)
        if(dim(x)[1]!=0){
          g1 = ggplot(x, aes(x=year, y=time, color=name)) +
            geom_point() + 
            theme(legend.position="none",
                  panel.grid.minor = element_line(colour="white", size=0.5)) +
            scale_y_continuous(minor_breaks = seq(0 , max(mydata$time,na.rm=TRUE), ifelse(identical(input$event,200),1,5))) + 
            scale_x_continuous(minor_breaks = seq(0,3000,1),breaks = seq(0,3000,5)) +
            xlab("year") + ylab("time (seconds)")
          g2 = ggplot(x, aes(x=year, y=time, color=name)) + geom_point() +
            theme(legend.position="bottom")
          gA = ggplotGrob(g1)
          g_legend<-function(a.gplot){ 
            tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
            leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
            legend <- tmp$grobs[[leg]] 
            return(legend)} 
          nullg = nullGrob()
          if(all(c_name%in%x$name) & !is.null(c_name)){
            legend <- g_legend(g2)
            lheight=sum(legend$height)
            heights=unit.c(unit(4, "in"), lheight, unit(2, "in")-lheight)
          }else{
            legend = nullGrob()
            heights=unit.c(unit(4, "in"), unit(0, "in"), unit(2, "in"))
          }
          #grid.arrange(gA, legend, nullg, heights=heights)
          grid.arrange(gA, bottom=legend, heights=unit(4, "in"))
        }
      }else{
        gNoPlot = textGrob("Nothing to plot. No results of the type selected.\n\n")
        plot(gNoPlot)
      }
    }
  })
  
  output$table <- renderTable({
    if(length(input$name)!=0 & !is.null(input$event)){
      x <- subset(mydata, boat==input$boat & event%in%input$event & name%in%input$name)
      if(dim(x)[1]!=0){
        good <- sapply(x$type, function(x){any(str_detect(x,input$type))})
        if("null"%in%input$type) good <- good | x$type==""
        x <- x[good,]
        ord <- order(x$name,x$event,x$age)
        x[ord,c("name","age","boat","time","event","level","type","finish","venue","month","year")]
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

