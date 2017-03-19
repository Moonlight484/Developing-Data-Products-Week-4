#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
##install.packages("shiny")
##install.packages("miniUI")
library(shiny)
library(ggplot2)
library(tidyr)

data("ChickWeight")

# Define UI for application that draws a histogram
ui <-fluidPage(
   
   # Application title
   titlePanel("Chick Weight Exploration"),
   
   # Sidebar with a slider input for number of bins 
   tabsetPanel(type = "tabs", 
               tabPanel("Chick Selection and Linear Approximations", br(),   
   sidebarLayout(
      sidebarPanel(
         
         h3("Chick Selection for Linear Regression "),
         radioButtons("chick", "Select a chick by clicking on a radio button. The weight gains below and the graph will be updated with the data for the selected chick",inline = TRUE,
                      c(seq(1:50))),
         h4("Average Weight Gain for All Chicks in Cohort in grams "),
         textOutput("selectedChickWeight"),
         h4(" Weight Gain for Selected Chick in grams "),
         textOutput("indivgain")
    
         
      ),
     
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("linearPlot"),
         h5(" In this application, each Chick is viewable in the context of the other chicks on the same diet. This is a rarely seen view, as usually the chicks are viewed as summary statistics withing their diet cohort."),
         h6("The Chicks are numbered such that all the Chick that are on the same diet are sequentially near each other. If the graph changes significantly when a new chick is clicked, a chick in a new diet cohort has been selected "),
         HTML("The <strong>Diet</strong> lines are the weight gain over time for all the chicks that were on the same diet as the selected Chick."),
         HTML("The <strong>Green</strong> line is a linear approximation of the weight gain of all the chicks that are on the same diet. The Green line will stay the same until a chick in a different cohort from the currentn one is selected"),
        HTML("The <strong>Dark Blue</strong> line is a linear approximation of the weight gain of the selected chick.")
         
         
         
      )
   )
               ),
   tabPanel("Application Explanation", br(), 
            h3("Diet Cohort Chick Weigh Gain User Information "),
   
   HTML(" <textarea cols= 120 rows = 10>The ChickWeight data consists of data related to 50 chicks who were fed one of 4 different diets over a period of several data readings. Not all chicks were weighed in every time period. The data readings correspond to the number of days since birth. More information about the ChickWeight data can be found at
(https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ChickWeight.html).

There are 2 main viewing areas. The Chick Selection and Linear Approximations on the first tab and this tab -  the Application Explanation tab. You may have to make the window taller to see both the Average Weight Gain for All Chicks in  Cohort and the Weight Gain for the Selected Chick on the first tab </textarea> "),
   h3("Chick Selection and Linear Approximations  tab") ,
   h4("Chick Selection Panel"),
   HTML(" <textarea cols= 120 rows = 4>Select a chick by clicking on a radio button. All displays are updated with the data for the selected chick. The results for Average Weight Gain for All Chicks in  Cohort and the Weight Gain for the Selected Chick are displayed below the radio buttons and are updated each time a new chick is selected. 
        </textarea> "),br(),
   h4("Graph Viewing Area"),
   HTML(" <textarea cols= 120 rows = 10>The Graph Viewing Area shows the Diet Cohort Chick Weight Gain. That is, the weight gain over time is plotted for all of the Chicks that were on the same diet as the selected Chick. The Chicks are numbered such that all the Chicks that are on the same diet are sequentially near each other. If the graph changes significantly when a new Chick is clicked, that means a Chick in a new diet cohort has been selected. This can be verified by looking at the legend on the graph to see which diet is being represented in the graph. 
       The Chicks are numbered such that all the Chick that are on the same diet are sequentially near each other. If the graph changes significantly when a new Chick is clicked, a Chick in a new diet cohort has been selected.
         The Diet lines are the weight gain over time for all the Chicks that were on the same diet as the selected Chick.
        The Green line is a linear approximation of the weight gain of all the Chicks that are on the same diet.
        The Dark Blue line is a linear approximation of the weight gain of the selected Chick.</textarea> "),br()
   

   )
   )
 )


# Define server logic required to draw a histogram
server <- function(input, output) {
   library(ggplot2)  
    
      data("ChickWeight")
      #create a color vector corresponding to levels in the Diet variable 
    ##  colors<-cols[ChickWeight$Diet]
      ChickWeight$Diet <-as.factor(ChickWeight$Diet)
      levels(ChickWeight$Diet) <- c("Diet 1","Diet 2","Diet 3","Diet 4", "Selected Chick")
      
      
      scweightgain <- reactive({
            selectedChick<- ChickWeight[ChickWeight$Chick== input$chick,"weight"]
            myrow <- length((selectedChick))
            selectedChick[myrow]-selectedChick[1]
          
            })     
      
      avgWeight <- reactive({
## get the selected chick and get his diet
            selectedChick<- ChickWeight[ChickWeight$Chick== input$chick,] 
            saveDiet <- selectedChick$Diet
## change the selected chick's diet to "Selected Chick" so it will show on the graph    
            selectedChick$Diet<-levels(ChickWeight$Diet)[5]
## get all other chicks on the same diet
            sc<-ChickWeight[ChickWeight$Diet==saveDiet[1],]
##find the average weight gain of all chicks on that diet
            splitChick <- split(sc, sc$Chick)
            onlyGoodChicks <-splitChick[lapply(splitChick,nrow)>1]
            mean(sapply( onlyGoodChicks,function(x){
             
            x[nrow(x),"weight"] - x[1,"weight"]
           })) 
           })
     
     linePlot <-     reactive({  
          
## get the selected chick and get his diet
           selectedChick<- ChickWeight[ChickWeight$Chick== input$chick,] 
           saveDiet <- selectedChick$Diet
## change the selected chick's diet to "Selected Chick" so it will show on the graph
           selectedChick$Diet<-levels(ChickWeight$Diet)[5]
           ## get all other chicks on the same diet
           sc<-ChickWeight[ChickWeight$Diet==saveDiet[1],]
## calculate the linear regression for the selected chick and the cohort           
            reg1 <- lm(weight~Time,data=sc)
            reg2 <- lm(weight~Time,data=selectedChick) 
            ggplot() + ggtitle("Diet Cohort Chick Weight Gain") +
            labs(x="Days Since Birth" , y="Weight Change in grams") + 
            geom_line(data = sc, aes(x=Time, y=weight, group = Chick, colour=Diet ), size=1.02 ) +
            geom_abline(intercept =reg1[[1]][1], slope = reg1[[1]][2], colour="green",size=1.2) +
            geom_line(data = selectedChick, aes(x=Time, y=weight, group = Chick, colour=Diet), size=1.2)  +
           geom_abline(intercept =reg2[[1]][1], slope = reg2[[1]][2], colour="blue",size=1.2) +
       theme(plot.title = element_text( color="blue", face="bold", size=32, hjust=0)) +
         theme(axis.title = element_text( color="#662266", face="bold", size=22))
          
         
        
     })
     ##output$radiolist <- reactive(function() {
     ##      switch(input$dietgroup,
      ##            "Diet 1" =c(seq(1:10)))
    ## })
       
   
     output$selectedChickWeight<- renderText({avgWeight()})
     output$indivgain <-renderText({scweightgain()})
     output$linearPlot <-   renderPlot({ linePlot()  }) 
     
     output$summary <- renderText("hyay")
     
     
}

# Run the application 
shinyApp(ui = ui, server = server)

