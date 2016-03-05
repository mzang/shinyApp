library(shiny)

#install.packages("devtools") 
#install_github('rCharts', 'ramnathv')
##load('yelp.RData')
load('biz.RData')
load("arizona.tract10.RData")
load("bizframe-geocoded.RData")
##source('global.R')

library(devtools) 
attach(new.biz)
require(ggplot2)
require(maps)
require(rCharts)
require(rjson)
require(sp)
require(reshape2)
library(RColorBrewer)
#require(rCharts)
options(RCHART_LIB = 'polycharts')

shinyUI(fluidPage(
    headerPanel("Welcome to Yelp Challenge!"),
    tabsetPanel(
      tabPanel('Yelp Reviews',
      sidebarLayout(
        sidebarPanel(
               selectInput('var4',"Please select:",choices=c('funny','useful','cool'),selected='funny'),
               selectInput('var5',"Please select:",choices=c('funny','useful','cool'),selected='useful'),
               selectInput('color',"Please select:",choices=c('food_type','region'),selected='food_type')),
        mainPanel(
               showOutput("chart1", "polycharts")
               ))),
      tabPanel("Barchart",
        sidebarLayout(position='left',
              sidebarPanel(
              selectInput('var1',"Please select:",list('Region'=21,'Food Type'=19),'region'),
              selectInput('var2',"Please select:",list('Region'=21,'Food Type'=19),'Food Type'),
              radioButtons('var3',label='Select file type for Download: Try download my plots!',choices = list('png','pdf'))
                    ),
        mainPanel(
          plotOutput('myplot'),
          downloadButton(outputId ='down',label='Download the plot')
          ))),
      tabPanel("Transformation",
        sidebarLayout(position='left',
              sidebarPanel(
                sliderInput('slide','Press the arrorw for animation',min=-3,max=3,value=-3,animate = T,
                              step=0.5),
                  radioButtons('Category',label='Select Category',choice=list('Food Type'=19,'Region'=21))
                ),
        mainPanel(
          plotOutput('temp',height=600,width=400)      
          ))), 
      tabPanel("Most Review Areas",
        sidebarLayout(position='left',
            sidebarPanel(
              radioButtons('var6',label='Review Counts by',choice=list('Tracts','Regions'),'Tracts')
              ##submitButton('Update!'),
              ##p('Click Update for Submit')
            ),
        mainPanel(
          plotOutput('map')
           ))),
      tabPanel('Explore Phonix',
       headerPanel('High Yelp Reviews Resturants with Ratings'),
              mainPanel(
                basicPage(
                  tags$style('.leaflet {height: 400px;}'),
                  showOutput('myChart2', 'leaflet')
                )
               
               ))
    )))            
                      
    
 
      
      
      
      

