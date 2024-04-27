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
library(dplyr)
library(utf8)

# run script update_mydata.R if new data
mydata=read.csv("mydata.csv",stringsAsFactors = FALSE)
mydata$name <- mydata$name %>% utf8::utf8_encode()
paddlers=read.csv("Paddlers.csv",stringsAsFactors = FALSE)
paddlers$name <- paddlers$name %>% utf8::utf8_encode()
#mydata=na.omit(mydata)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),
  
  # Application title
  titlePanel("Sprint Kayak and Canoe Times"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('boat', 'Boat', 
                  sort(unique(as.character(mydata$boat))), selectize=TRUE,selected="K1"),
      selectInput('event', 'Select Distance(s). Values shown are those available for the paddler names.', 
                  sort(unique(mydata$event)), 
                  selectize=TRUE, 
                  multiple=TRUE),
      selectInput('name', 'Name', 
                  sort(unique(as.character(mydata$name))), 
                  multiple=TRUE, selectize=TRUE),
      tags$div(
        tags$h2("Filters")
       ),
      checkboxGroupInput("type", "Types of records to include:",
                         choices = c('no info' = "null",
                                     'time-trial' = "Time-trial",
                                     heat = "Heat",
                                     'semi-final' = "Semis",
                                     final = "Final",
                                     'icf certified only' = "certified"),
                         selected = c("Time-trial","null","Heat","Semis","Final"), inline=TRUE),
      selectInput('byr', 'Birth Year (leave blank to include all)', 
                  sort(unique(paddlers$age)), 
                  multiple=TRUE, selectize=TRUE),
      selectInput('country', 'Nationality (leave blank to include all)', 
                  sort(unique(paddlers$country)), 
                  multiple=TRUE, selectize=TRUE, selected="USA"),
      selectInput('gender', 'Gender (leave blank to include both and unknown)', 
                  c("Male","Female"), 
                  multiple=TRUE, selectize=TRUE),
      selectInput('year', 'Year (only include data in these years)', 
                sort(unique(mydata$year)), 
                multiple=TRUE, selectize=TRUE)
  ),
  
    # Show a plot of the generated distribution
    #plotOutput("distPlot", height = "6in"),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot: Results vs Year", plotOutput("yearPlot")),
        tabPanel("Plot: Results vs Age", plotOutput("agePlot")),
        tabPanel("Results table", tableOutput("table")),
        tabPanel("Download", tags$br(), "\nDownload the results table: ", downloadButton('downloadData', 'Download'), 
                 tags$br(), tags$br(),"You will need to select a distance in order to produce a table of results first." )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    boat <- input$boat
    event <- input$event
    byr <- input$byr
    country <- input$country
    gender <- input$gender
    year <- input$year
    name <- isolate(input$name)
    s_mydata <- mydata[ mydata$boat==boat,]
    if(!is.null(event)) s_mydata <- s_mydata[ s_mydata$event %in% event, ]
    if(!is.null(byr)){
      s_mydata <- s_mydata[ !is.na(s_mydata$by), ]
      s_mydata <- s_mydata[ s_mydata$by %in% byr, ]
    }
    if(!is.null(country)){
      s_mydata <- s_mydata[ !is.na(s_mydata$country), ]
      s_mydata <- s_mydata[ s_mydata$country %in% country, ]
    }
    if(!is.null(year)){
      s_mydata <- s_mydata[ !is.na(s_mydata$year), ]
      s_mydata <- s_mydata[ s_mydata$year %in% year, ]
    }
    if(!is.null(gender)){
      s_mydata <- s_mydata[ !is.na(s_mydata$gender), ]
      gen = str_sub(gender,1,1)
      s_mydata <- s_mydata[ s_mydata$gender %in% gen, ]
    }
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
    event <- isolate(input$event)
    name <- input$name
    good <- sapply(mydata$type, function(x){any(str_detect(x,input$type))})
    if("null"%in%input$type) good <- good | mydata$type==""
    s_mydata <- mydata[ mydata$boat==boat & good,]
    if(!is.null(name)) s_mydata <- s_mydata[s_mydata$name%in%name,]
    avail_val <- sort(unique(s_mydata$event))
    sel_val = event
    if(!is.null(event)){
      if(!any(event %in% avail_val)){
        sel_val = NULL
      }else{
        sel_val = event[event %in% avail_val]
        }
      }
    updateSelectInput(session, "event",
                      label = "Select Distance(s). Values shown are those available for the paddler names.",
                      choices = avail_val, selected = sel_val )
  }, priority=10)
  
  
  output$agePlot <- renderPlot({
    # subset data based on boat, event and age
    yesPlot = TRUE; #flag
    c_name <- input$name
    byr <- input$byr
    country <- input$country
    gender <- input$gender
    year <- input$year
    if(!is.null(gender)) gender <- str_sub(gender, 1, 1)
    
    if(is.null(input$event)){ 
      gNoPlot = textGrob("Select a distance to plot",gp=gpar(fontsize=20))
      grid.arrange(gNoPlot)
    }else{
      x <- subset(mydata, boat==input$boat & event%in%input$event)
      if(nrow(x)==0) yesPlot = FALSE
      if(yesPlot){
        #if(all(c_name%in%x$name) & !is.null(c_name)) x <- subset(x, name %in% c_name)
        if(!is.null(c_name)) x <- subset(x, name %in% c_name)
        if(!is.null(byr)) x <- subset(x, by %in% input$byr)
        if(!is.null(country)) x <- subset(x, country %in% input$country)
        if(!is.null(year)) x <- subset(x, year %in% input$year)
        if(!is.null(gender)) x <- subset(x, gender %in% str_sub(input$gender,1,1))
        # now remove any names without an age
        noage <- unique(x$name[is.na(x$age)])
        x <- x[!is.na(x$age),]
        if(nrow(x)==0) yesPlot = FALSE
      }
      # now remove any results that are not the right type (spec'd in check boxes by user)
      if(yesPlot){
        good <- sapply(x$type, function(x){any(str_detect(x,input$type))})
        if("null"%in%input$type) good <- good | x$type==""
        if("certified"%in%input$type) good <- good & x$certified=="yes"
        x <- subset(x, good)
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
        if(!is.null(c_name)) g1 = g1 + scale_color_manual(values=order(c_name))
        g2 = ggplot(x, aes(x=age, y=time, color=name)) + geom_point() +
          theme(legend.position="bottom")
        if(!is.null(c_name)) g2 = g2 + scale_color_manual(values=order(c_name))
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
        gNoPlot = textGrob("Nothing to plot. No athletes with age data or no results of the type selected.\nTry clicking the year plot.\n\n", gp=gpar(fontsize=16))
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
        if(!is.null(c_name)) x <- subset(x, name %in% c_name)
        if(!is.null(input$byr)) x <- subset(x, by %in% input$byr)
        if(!is.null(input$country)) x <- subset(x, country %in% input$country)
        if(!is.null(input$year)) x <- subset(x, year %in% input$year)
        if(!is.null(input$gender)) x <- subset(x, gender %in% str_sub(input$gender,1,1))
         if(nrow(x)==0) yesPlot = FALSE
      }
      if(yesPlot){
        #if(all(c_name%in%x$name) & !is.null(c_name)) x <- subset(x, name %in% c_name)
        #if(!is.null(c_name)) x <- subset(x, name %in% c_name)
        good <- sapply(x$type, function(x){any(str_detect(x,input$type))})
        if("null"%in%input$type) good <- good | x$type==""
        if("certified"%in%input$type) good <- good & x$certified=="yes"
        x <- subset(x, good)
        x$x = x$year + x$month/12
        if(dim(x)[1]!=0){
          g1 = ggplot(x, aes(x=x, y=time, color=name)) +
            geom_point() + 
            theme(legend.position="none",
                  panel.grid.minor = element_line(colour="white", size=0.5)) +
            scale_y_continuous(minor_breaks = seq(0 , max(mydata$time,na.rm=TRUE), ifelse(identical(input$event,200),1,5))) + 
            scale_x_continuous(minor_breaks = seq(0,3000,1),breaks = seq(0,3000,5)) +
            xlab("year") + ylab("time (seconds)")
          if(!is.null(c_name)) g1 = g1 + scale_color_manual(values=order(c_name))
          g2 = ggplot(x, aes(x=year, y=time, color=name)) + geom_point() +
            theme(legend.position="bottom")
          if(!is.null(c_name)) g2 = g2 + scale_color_manual(values=order(c_name))
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
  
  datasetInput <- reactive({
      if(!is.null(input$event) & !is.null(input$boat)){
        x <- subset(mydata, boat==input$boat & event%in%input$event)
        if(!is.null(input$name)) x <- subset(x, name %in% input$name)
        if(!is.null(input$byr)) x <- subset(x, by %in% input$byr)
        if(!is.null(input$country)) x <- subset(x, country %in% input$country)
        if(!is.null(input$year)) x <- subset(x, year %in% input$year)
        if(!is.null(input$gender)) x <- subset(x, gender %in% str_sub(input$gender,1,1))
        if(dim(x)[1]!=0){
          good <- sapply(x$type, function(x){any(str_detect(x,input$type))})
          if("null"%in%input$type) good <- good | x$type==""
          if("certified"%in%input$type) good <- good & x$certified=="yes"
          x <- x[good,]
          ord <- order(x$name,x$event,x$year,x$month)
          x[ord,c("name","age","boat","time","event","level","type","finish","venue","month","year")]
        }
      }     
    })

  output$table <- renderTable({
    datasetInput()
  })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    
    # what name to use when saving the file.
    filename = function() { paste(input$boat,"Sprint Results.csv") },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      if(is.null(input$event)){ tab <- data.frame("Select a distance to see results") }else{ tab <- datasetInput() }
      # Write to a file specified by the 'file' argument
      write.table(tab, file, sep = ",", row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

