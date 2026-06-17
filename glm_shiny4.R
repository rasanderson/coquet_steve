## a shiny app for loading data for a SEM
library(shiny)
library(ggplot2)
library(glmmTMB)
library(nlme)

rain_weather.dat<-read.csv("df_events(in).csv",header=TRUE)
temp<-nrow(rain_weather.dat)
days<-seq(1,temp,1)
attach(rain_weather.dat)
rain_weather.dat<-cbind(days,event_WWTW,event_harbour,rainfall_warkworth,Wark_rainfall_lag1,wwtwmin1)
rain_weather.dat<-na.omit(rain_weather.dat)
detach()
rain_weather.dat<-data.frame(rain_weather.dat)

# must cotain, rain and lag, event and flow and time
waste_amble<-function(rain, lag)
          {
          p=-3.8281+(1.57339*rain)+(0.45819*lag)
          prob=exp(p)/(1+exp(p))
          return(prob)
          }
waste_amble_ser<-function(rain, lag,ser)
          {
          p=-4.1796+(1.5060*ser)+(1.6735*rain)+(0.2847*lag)
          prob=exp(p)/(1+exp(p))
          return(prob)
          }          
waste_harbour<-function(rain, lag)
          {
          p=-4.10971+(0.48350*rain)+(0.23122*lag)
          prob=exp(p)/(1+exp(p))
          return(prob)
          }  #####################
zero_inf<-function(rain,lag)
         {
         p=3.82353-(1.58168*rain)-(0.46154*lag)
         prob=exp(p)/(1+exp(p))
         return(1-prob)
         }


##### the server bit
ui <- fluidPage(

   # Application title
   titlePanel("Modelling storm water events and weather in the Coquet"),
    sidebarLayout(
      sidebarPanel(        
      # choice of output
     radioButtons("bins1"," Analysis to undertake ",choices=c("Time series", "Models and prediction"), selected=c("Time series")),     
     # select the variable to plot
     radioButtons("bins2","Pick one variable to plot/model ",choices=c("Rainfall","Storm water events"), selected=c("Rainfall")), 
      # commented out until models done
      radioButtons("bin3","Pick the sewage outflow to plot/model ", choices=c("Amble waste water","Amble harbour "), selected=c("Amble waste water")),
      
      # add new data
        # Sidebar with a slider input for number of bins 
 
         sliderInput("bins4",
                     "Rainfall today mm:",
                     min = 0.01,
                     max = 30,
                     value = 0.01),
      
         sliderInput("bins5",
                    "Rainfall yesterday mm:",
                     min = 0.01,
                     max = 10,
                     value = 0.01), 
                     
        sliderInput("bins6",
                    "Event yesterday ? :",
                     min = 0.01,
                     max = 1,
                     value = 0.01)),
                                                
       
         # Show a plot of the data and fitted model by outfall 
      mainPanel(
                plotOutput("distPlot")
               )
        ) # end of bar layout
) # end of fluidpage



  #############  this is for running the model

server <- function(input, output) {
  
     plot_info <- reactive({
     choice1<-input$bins1
     choice2<-input$bins2
     choice3<-input$bins3
     rains<-input$bins4
     new_rain<-rains
     lags<-input$bins5
     new_lag<-lags
     yesterday<-input$bin6
     new_data<-matrix(0,nrow=1,ncol=3)
     new_data[1,1]=new_rain
     new_data[1,2]=new_lag
     new_data[1,3]=1
     new_yesterday=1
     if(yesterday<0.5)
        {
        new_yesterday=0
        }
 
     new_data<-data.frame(new_data)
     colnames(new_data)<-c("rain","lag","SWE")
     
     
     # testing
    # new_data<-matrix(0,nrow=1,ncol=3)
    #     new_data<-data.frame(new_data)
    # colnames(new_data)<-c("rain","lag","SWE")
   
   #  new_data$rain=5
   #  new_data$lag=5
   #  new_yesterday=1
     
   #  choice1<-c("Time series")
    # choice1=c("Models and predictions")
    # choice2<-c("Rainfall")
    #choice2<-c("Storm water events")
   # choice3=c("Amble waste water")
    
           if(choice1=="Time series")
             {
             # check counts is ok should be named var
             if(choice2=="Rainfall")
                {
                ggplot(rain_weather.dat, mapping=aes(x=days))+
                geom_line(aes(y=`rainfall_warkworth`),color="blue")+
                labs(y="rain(mm)", title=`choice2`) 
             
                return(myplot)
                }
              if(choice2=="Storm water events")
                {
                # note here I'm suggesting hours in day
                ggplot(rain_weather.dat, mapping=aes(x=days))+
               geom_line(aes(y=`event_WWTW`),color="blue")+
               labs(y="event occuring 0/1", title=`choice2`)  
                           
               return(myplot)
                }                             
             }
           if(choice1=="Models and prediction")
             {            
             #  need an if statement here to separate two outflows
               if(choice3=="Amble waste water")
                 {
                 test.glm<-glm(event_WWTW~rainfall_warkworth+Wark_rainfall_lag1+wwtwmin1, binomial, rain_weather.dat)                
                 }
               if(choice3=="Amble harbour")
                 {
                 test2.glm<-glm(event_harbour~rainfall_warkworth+Wark_rainfall_lag1, binomial, rain_weather.dat)                
                 }  
               
               #  get the statistics for the model
               min_warkworth_rain<-min(rain_weather.dat$rainfall_warkworth)
               max_warkworth_rain<-max(rain_weather.dat$rainfall_warkworth)
               min_warkworth_rain_lag1<-min(rain_weather.dat$Wark_rainfall_lag1)
               max_warkworth_rain_lag1<-max(rain_weather.dat$Wark_rainfall_lag1)

              # now make the predictor matrix to get a surface              
              rowsw=100
              colsw=100
              
              rain_step=((max_warkworth_rain-min_warkworth_rain)/rowsw)
              lag_step=((max_warkworth_rain_lag1-min_warkworth_rain_lag1)/rowsw)              
              start_rain=min_warkworth_rain
              start_lag=min_warkworth_rain_lag1
              
              #### surface         
      
              ####### testin 
              # choice3=c("Amble waste water")
             events<-matrix(0,nrow=rowsw*colsw, ncol=3)
             events_noser<-matrix(0,nrow=rowsw*colsw, ncol=3)
             counter=1
             for(i in 1:rowsw)
               {
               if(i==1)
                 {
                 rain=start_rain
                 }
               if(i>1)
                 {
                 rain=rain+rain_step
                 }
               for(j in 1:colsw)
                  {
                  if(j==1)
                    {
                    lag=start_lag                   
                    }                                                                     
                    events[counter,1]=rain
                    events[counter,2]=lag
                    events_noser[counter,1]=rain
                    events_noser[counter,2]=lag
                   
                    if(choice3=="Amble waste water")
                      {
                       #events[counter,3]=waste_amble(rain,lag)
                       #events[counter,3]=zero_inf(rain,lag)
                       events[counter,3]=waste_amble_ser(rain,lag,1)
                       events_noser[counter,3]=waste_amble_ser(rain,lag,0)
                      } 
                     if(choice3=="Amble harbour")
                      {
                       events[counter,3]=waste_harbour(rain,lag)
                      }
                      lag=lag+lag_step 
                    counter=counter+1
                   
                  } # end of coks                  
               } # end of rows
               ###### TEST ONLY #####
            new_lag=1
            new_rain=10
            new_yesterday=1
           new_data<-data.frame(rainfall_warkworth=new_rain, Wark_rainfall_lag1=new_lag, wwtwmin1=new_yesterday)
           plot_data<-new_data[,c(1,2)]
           colnames(plot_data)<-c("rain","lag")
           plot_data<-data.frame(plot_data)
           plot_data$SWE=1
           pred<-predict(test.glm,new_data)
           pred=exp(pred)/(exp(pred)+1)
            events<-data.frame(events)
            colnames(events)<-c("rain","lag","SWE")
            new_data$SWE=1
            # cane we add a point for new data
            if(new_yesterday==1)
              {
              ggplot(events, aes(rain,lag, z = SWE))+
              geom_contour_filled()+           
              geom_point(data=plot_data, aes(x=rain, y=lag,color=obs),  color = "red", size = 4, shape = 19) 
              return(myplot)              
              }
           if(new_yesterday==0)
              {
              ggplot(events_noser, aes(rain,lag, z = SWE))+
              geom_contour_filled() +          
              geom_point(data=plot_data, aes(x=rain, y=lag,color=obs),  color = "red", size = 4, shape = 19) 
              return(myplot)
              }                                          
       
            } # end of models and prediction
            
        
    output$distPlot <- renderPlot({
      plot_info()
    }) # this expression above generates a plot
 
    
      }) #end of reactive/renderPlot plot   
  
  } # end of server 
  
  
shinyApp(ui = ui, server = server)



  #aes(x = rain,y=lag, z=SWE),
  
#  table_info <- reactive({
#    if(input$bins1=="Linear_Models")
#     {
#      choice1<-input$bins1
#      choice2<-input$bins2
      # choice1<-c("Time_series")
      # choice2<-c("Seed_sown")
#      cat("choice 1 ",choice1, " choice 2 ", choice2,"\n")
#      df.dat<-cbind(sweden$county,sweden$time,sweden[,choice2])
#      df.dat<-data.frame(df.dat)
      
#      colnames(df.dat)<-c("County","time","Counts")
      
#      test.gls<-lme(log(Counts+1)~time, random=~1|County,df.dat)
#      fitted<-exp(fitted(test.gls))-1
      #  get the statistics for the model
 #     test.sum<-summary(test.gls)
 #     test.t<-test.sum$tTable

  #    return(test.t)      
   #   }
    
 # })
  
  #  output$distPlot <- renderPlot({
    #  plot_info()
   # }) # this expression above generates a plot

    #output$table <- renderTable({
    #  table_info()
    #})
    
#}




######## run it
shinyApp(ui = ui, server = server)
