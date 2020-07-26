library(shiny)
library(shinyWidgets)
library(httr)
library(jsonlite)
library(shinyjs)
library(ggplot2)


server=function(input,output,session){

  #### reactive inputs #####
  
  myeyetest= reactive({input$eyetest})
  
  mygender = reactive({input$gender})
  
  myeyetestage = reactive({input$eyetestage})
  
  myonsetage = reactive({input$onsetage})
  
  myparents = reactive({input$parents})
  
  myoutdoors = reactive({input$outdoors})
  
  myhptime = reactive({input$hptime})
  
  mymyopic = reactive({input$myopic})
  
  
  ### end of reactive inputs ####
  
  ########## toggle buttons ############
  

  observe({

    if (is.null(input$eyetest)==TRUE) {
      shinyjs::disable("predOnset")
    }else if(is.null(input$gender)==TRUE){
      shinyjs::disable("predOnset")
    }else if(is.null(input$eyetestage)==TRUE){
      shinyjs::disable("predOnset")
    }else if(is.null(input$onsetage)==TRUE){
      shinyjs::disable("predOnset")
     }else if(is.null(input$parents)==TRUE){
       shinyjs::disable("predOnset")
     }else if(is.null(input$outdoors)==TRUE){
       shinyjs::disable("predOnset")
     }else if(is.null(input$myopic)==TRUE){
       shinyjs::disable("predOnset")
     }else if(is.null(input$hptime)==TRUE){
       shinyjs::disable("predOnset")
     } else {
    shinyjs::enable("predOnset")
  }

  })
  
 ##### reset everything to default
  
  
  observeEvent(input$reset, {
    
    reset("eyetest")
    reset("gender")
    reset("eyetestage")
    reset("onsetage")
    reset("outdoors")
    reset("hptime")
    reset("parents")
    reset("myopic")
    
    html("onsetRisk","")
    html("onsetHighRisk","")
    html("onsetAge","")
    html("stableAge","")
    html("dangerHigh","")
    
    html("progHigh","")
    html("progRate","")
    html("progStab","")
    
    
    hide("progGraph")

  })
  
  

  ######### reset eye test age ###########
  
  observe({
    
    if (is.null(input$eyetest)==TRUE) {
      shinyjs::hide("eyetestage")
      shinyjs::hide("pleaseEyeAge")
    } else if(input$eyetest=="Yes"){
      shinyjs::show("eyetestage")
      shinyjs::show("pleaseEyeAge")
    } else if(input$eyetest=="No"){
      shinyjs::hide("eyetestage")
      shinyjs::hide("pleaseEyeAge")
    }
  })
  
 
  ##################################
  
  #### start of HTML stuff
  
  output$pleaseEyeTest=renderText({
    HTML("Have you ever had an eye test at an optometrist / eye doctor before?")
  })
  
  output$pleaseGender=renderText({
    HTML("What is your gender?")
  })
  
  output$pleaseAge=renderText({
    HTML("What is your current age (years)?")
  })
  
  output$pleaseEyeAge=renderText({
    HTML("When was your last eye test age (years)?")
  })
  
  
  output$pleaseParents = renderText({
    HTML("How many of your parents are myopic?")
  })
  
  output$pleaseOutdoors = renderText({
    HTML("On average, how many hours per week do you spend on outdoor activities?")
  })
  
  output$pleaseHptime = renderText({
    HTML("On average, how many hours per day do you spend on smart devices?")
  })
  
  
  output$onsetLabel = renderText({
    HTML("Probabilty that you will develop myopia")
    })
  
  output$pleaseMyopic = renderText({
    HTML("Are you wearing glasses for short-sightedness (myopia)?")
  })
  
  ### end of HTML stuff
  
  
  #### start of function 1a
  
  probOnsetapi = function(){
    
    output$onsetRisk=renderText({
      
      # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())
      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      myMYO<-as.character(myMYO)
      
      url <- "http://18.141.214.37:2323/onsetprob"      # the url changes when you restart AWS
      body <- list(EyeTest=myC,
                   Gender=myG,
                   EyeTestAge=myET,
                   CurrentAge=myOA,
                   Outdoor=myOD,
                   Parent=myPA,
                   Hptime=myHP
                  )

      r <- POST(url, body = body, encode = "json")

      r2=content(r, "parsed")
      
      r3=unlist(r2)
      
      r3=r3 * 100

   ####### stataments

      p1=paste("Your risk of developing myopia is:", r3,"%")

      ht1=HTML(p1,"</br>")   

      ht2=if(myMYO=="No"){
      
            ht1
        
      } else { "" }
      
    })    

    
  }
  
  
  #### end of function 1a
  
  
  #### start of function 2
  
  ageOnsetapi = function(){

    output$onsetAge=renderText({

      # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())
      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      myMYO <- as.character(myMYO)

      url1 <- "http://18.141.214.37:2323/onsetage1"      # the url changes when you restart AWS
      body1 <- list(EyeTest=myC,
                   Gender=myG,
                   EyeTestAge=myET,
                   CurrentAge=myOA,
                   Outdoor=myOD,
                   Parent=myPA,
                   Hptime=myHP
      )

      r1 <- POST(url1, body = body1, encode = "json")

      r2=content(r1, "parsed")

      r3=unlist(r2)

      r21=r3[1]
      
      url2 <- "http://18.141.214.37:2323/onsetage2"      # the url changes when you restart AWS
      body2 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r22 <- POST(url2, body = body2, encode = "json")
      
      r4=content(r22, "parsed")
      
      r5=unlist(r4)
      
      r31=r5[1]


      p1 = HTML(paste("If you develop myopia, you are at risk of developing myopia between", r21,"to",r31,"years","</br>"))

      p2 = if(myMYO=="No"){
        p1
      
      } else {
        
        ""
      }
      
      p2
     
    })


  }

  
  #### end of function 2
  
  
  
  #### start of function 3
  
  progRateapi = function(){
    
    output$progRate=renderText({
      
      # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())

      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      myMYO<-as.character(myMYO)
      
      url1 <- "http://18.141.214.37:2323/prograte"      # the url changes when you restart AWS
      body1 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r1 <- POST(url1, body = body1, encode = "json")
      
      r2=content(r1, "parsed")
      
      r3=unlist(r2)
      
      r21=r3[1]
      
     p1=HTML(paste("If you develop myopia, your average myopia progression rate per year is",r21,"degrees/year","</br>"))

     p2=if(myMYO=="No"){
       p1
     }else{ "" }
     
     p2
     
     })
    
    
  }
  
  
  #### end of function 3
  
  
  #### start of function 3a
  
  progStableapi = function(){
    
    output$progStab=renderText({
      
      # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())
      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      myMYO=as.character(myMYO)
      
      url2 <- "http://18.141.214.37:2323/progfinal"      # the url changes when you restart AWS
      body2 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r22 <- POST(url2, body = body2, encode = "json")
      
      r4=content(r22, "parsed")
      
      r5=unlist(r4)
      
      r31=r5[1]
      
      
      url3 <- "http://18.141.214.37:2323/stable"      # the url changes when you restart AWS
      body3 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r33 <- POST(url3, body = body3, encode = "json")
      
      r8=content(r33, "parsed")
    
      r9=unlist(r8)
      
      r91=r9[1]
      
      p1=HTML(paste("If you develop myopia, your age of stabilization of myopia will be at",r91,
                 "years, with a stabilization degree of",r31,"</br>"))
      
      
      p2=if(myMYO=="No"){
        
        p1
      
    } else {  "" }
      
      p2
      
      
    })
    
    
  }
  
  
  #### end of function 3a

  
  
  #### start of function 3b
  
  highFinalapi = function(){
    
    output$progHigh=renderText({
      
        # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())
      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      myMYO<-as.character(myMYO)
      
      url3 <- "http://18.141.214.37:2323/highage"      # the url changes when you restart AWS
      body3 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r33 <- POST(url3, body = body3, encode = "json")
      
      r8=content(r33, "parsed")
      
      r9=unlist(r8)
      
      r91=r9[1]
      
      p1=HTML(paste("to develop high myopia at",r91,"</br>"))
      ht1=HTML(p1,"</br>") 
      
      
      ht2= if(myMYO=="Yes"){
        
        ht1
        
      } else { "" }
      
      ht2

      
    })
    
    
  }
  
  
  #### end of function 3b
  
  
  
  
  #### start of function 6
  
  graphProgapi = function(){
    
    output$progGraph=renderPlot({
      
      # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())
      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      
      
      url <- "http://18.141.214.37:2323/proggraph"      # the url changes when you restart AWS
      body <- list(EyeTest=myC,
                   Gender=myG,
                   EyeTestAge=myET,
                   CurrentAge=myOA,
                   Outdoor=myOD,
                   Parent=myPA,
                   Hptime=myHP
      )
      
      r <- POST(url, body = body, encode = "json")
      
      r3=content(r, "parsed")
      
      if(is.null(r3[[1]])==FALSE){  
        
        df <- data.frame(matrix(unlist(r3), ncol=2, byrow=T),stringsAsFactors=FALSE)
        colnames(df)=c("age", "deg")
        df$age=as.numeric(df$age)
        df$deg=as.numeric(df$deg)
      
        nn2=nrow(df)
        nn28=NA
        
        if(nn2>10){
          nn28=nn2-3
        } else if(nn2>5){
          nn28=nn2-4
        } else if(nn2>7){
          nn28 =nn2-2
        }
        
        mygraph=df
        
        coords = mygraph$deg
        color2 = ifelse(mygraph$deg >=600,"blue","red")
        
        library(ggplot2)
        p2=ggplot(mygraph) + 
          geom_point(aes(x = age, y = deg, colour = color2), size = 3) +
          geom_label(aes(x= age+0.5, y = deg+0.5,label=coords)) + 
          stat_smooth(aes(x = age, y = deg), method = "lm",
                      formula = y ~ poly(x, nn28), se = FALSE) +
          coord_cartesian(ylim = c(0, 1000)) +
          labs(x = "Age", y="Myopia Degree", title="Degree of myopia across Age")
      
      
      p2
      
    } else {
      
      NA
    }
    
      
    })    
    
    
  }
  

  ### end of function 6 #########

  #### start of function 7 ########
  
  dangerHighapi = function(){
    
    output$dangerHigh=renderText({
      
      # req(input$country)
      # req(input$item)
      # req(input$customer)
      # req(input$sales)
      # req(input$quantity)
      
      myC=isolate(myeyetest())
      myG=isolate(mygender())
      myET=isolate(myeyetestage())
      myOA=isolate(myonsetage())
      myPA=isolate(myparents())
      myOD=isolate(myoutdoors())
      myHP=isolate(myhptime())
      myMYO=isolate(mymyopic())
      
      myC<-as.character(myC)
      myG<-as.character(myG)
      myET<-as.numeric(myET)
      myOA<-as.numeric(myOA)
      myPA<-as.character(myPA)
      myOD<-as.numeric(myOD)
      myHP<-as.numeric(myHP)
      myMYO=as.character(myMYO)
      
      url2 <- "http://18.141.214.37:2323/progfinal"      # the url changes when you restart AWS
      body2 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r22 <- POST(url2, body = body2, encode = "json")
      
      r4=content(r22, "parsed")
      
      r5=unlist(r4)
      
      r31=r5[1]
      
      ####################
      
      url3 <- "http://18.141.214.37:2323/highage"      # the url changes when you restart AWS
      body3 <- list(EyeTest=myC,
                    Gender=myG,
                    EyeTestAge=myET,
                    CurrentAge=myOA,
                    Outdoor=myOD,
                    Parent=myPA,
                    Hptime=myHP
      )
      
      r39 <- POST(url3, body = body2, encode = "json")
      
      r9=content(r39, "parsed")
      
      r95=unlist(r9)
      
      r99=r9[1]
      
      p1=HTML(paste("You are in danger of developing high myopia at",r99,"</br>"))
      ht1=HTML(p1,"</br>") 
      
      
      ht2= if(r31>=600 & myMYO=="No"){
        
        ht1
        
      } else { "" }
      
      ht2
      
    })
    
    
  }
  
  
  ##### end of function 7 ##############
  
  
 ### enagage the AI on click ####

  observeEvent(input$predOnset,{
    
    probOnsetapi()
    ageOnsetapi()
    #graphOnsetapi()
    #show("onsetGraph")
    #progTableapi()
    #show("progDF")
    graphProgapi()
    #show("progGraph")
    progStableapi()
    progRateapi()
    #highFinalapi()       # consider putting back
    dangerHighapi()
    
      })
    
      
   
  ########
  
 
}   # end of server > do not delete



###############



ui <- fluidPage(
  useShinyjs(),
  
  setBackgroundColor(color = "#33E0FF"),
  
  tags$style(type = 'text/css', '#pleaseAge   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseGender   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseEyeTest   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseEyeAge   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseOutdoors   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseParents   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseHptime   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#onsetRisk   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#onsetAge   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#progDF   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#stableAge   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#progStab   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#progRate   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#progHigh   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#pleaseMyopic   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#onsetHighRisk   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:white; border-style: none;}'),
  tags$style(type = 'text/css', '#dangerHigh   {font-size: 30px; font-family: calibri light; background-color: rgba(0,0,0,0); color:red; border-style: none;}'),
  
  titlePanel("PMCV"),
  sidebarLayout(
    
    sidebarPanel = "",
    
    mainPanel(
      
      
      fluidRow(
        column(3, offset=0,
               #       wellPanel(
               
               htmlOutput("pleaseAge"),
               
               knobInput(width = 100, height = 100,      #300
                         inputId = "onsetage",
                         label = "",
                         value = 2,
                         min =   2,
                         max=  13,
                         displayPrevious = TRUE,
                         lineCap = "round",
                         fgColor = "#428BCA",
                         inputColor = "#ffffff"
                         )
               ),
        

        column(3, offset=0,
               #       wellPanel(
               
               htmlOutput("pleaseOutdoors"),
               
               knobInput(width = 100, height = 100,      #300
                         inputId = "outdoors",
                         label = "",
                         value = 0,
                         min =   0,
                         max=  40,
                         displayPrevious = TRUE,
                         lineCap = "round",
                         fgColor = "#428BCA",
                         inputColor = "#ffffff"
               )
        ),
        
        column(3, offset=0,
               #       wellPanel(
               
               htmlOutput("pleaseHptime"),
               
               knobInput(width = 100, height = 100,      #300
                         inputId = "hptime",
                         label = "",
                         value = 0,
                         min =   0,
                         max=  12,
                         displayPrevious = TRUE,
                         lineCap = "round",
                         fgColor = "#428BCA",
                         inputColor = "#ffffff"
               )
        ),
        
        
        column(3, offset=0,
               #       wellPanel(
               
               htmlOutput("pleaseEyeAge"),
               
               knobInput(width = 100, height = 100,      #300
                         inputId = "eyetestage",
                         label = "",
                         value = 0,
                         min =   0,
                         max=  13,
                         displayPrevious = TRUE,
                         lineCap = "round",
                         fgColor = "#428BCA",
                         inputColor = "#ffffff"
               )
        )
       
       ),
        
        
     br(),
     br(),
     br(),
     
     fluidRow(
       column(4, offset=0,
     
           htmlOutput("pleaseGender"),
               
               radioButtons(
                 inputId = "gender", 
                 label = "",
                 choices = c("Male","Female"),
                 selected=""
                           )

      ),        
        
      column(4, offset=0,
      
           htmlOutput("pleaseParents"),
                      
                     radioButtons(
                        inputId = "parents", 
                        label = "",
                        choices = c("None","One","Both"),
                        selected=""
                      )
            
         )
     ),
      
     br(),
     br(),
     
      
     fluidRow(
       
      column(4, offset=0,
             
             htmlOutput("pleaseEyeTest"),
             
             radioButtons(
               inputId = "eyetest", 
               label = "",
               choices = c("Yes","No"),
               selected=""
             )
             
      ),
      
      
      column(4, offset=0,
             
             htmlOutput("pleaseMyopic"),
             
             radioButtons(
               inputId = "myopic", 
               label = "",
               choices = c("Yes","No"),
               selected=""
             )
             
      )
      
     ),
      
                   
    br(),
    br(),
    br(),
     
    
    fluidRow(
      
      column(1, offset=0,
   
          actionButton("predOnset", "Go!",
                    icon = icon("forward"))
      ),
          
      
      column(1, offset=0,
             
             actionButton("reset", "reset",
                          icon = icon("stop"))
      ),  
 
      column(8, offset=1,
    
             htmlOutput("onsetRisk"),
             htmlOutput("onsetAge"),
             htmlOutput("progRate"),
             htmlOutput("progStab"),
             htmlOutput("progHigh"),
             htmlOutput("dangerHigh")
              
    )
    
),
    
    
fluidRow( 
    
    
  column(10, offset=2,
           
           plotOutput("progGraph")
           
    )
    
),
    
br(),
br(),
br()


  )


  )
  
)   # end of ui > do not delete



shinyApp(ui=ui, server=server)