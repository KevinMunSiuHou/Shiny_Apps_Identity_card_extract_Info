#IC Information extractor
library(shiny)

ui<-pageWithSidebar(
    # Application title
    headerPanel("Malaysian Identity Card"),
    sidebarPanel(
      textInput('IC_number', 'Identity Card number', "Example: 781010154567"
                ),
      submitButton('Submit')
    ),
    mainPanel(
      h3('Details for Identity Card Holder'),
      h4('Age:'),
      verbatimTextOutput("User_Age"),
      h4('Birth Location: '),
      verbatimTextOutput("User_BL"),
      h4('Gender:'),
      verbatimTextOutput("User_G")
    )
  )

server<- function(input, output) {
  
  user_data_age <- reactive({
    x <- if(as.numeric(substr(as.numeric(input$IC_number),1,2))<=20){
      print(2020-(as.numeric(substr(as.numeric(input$IC_number),1,2))+2000))
    }else if(as.numeric(substr(as.numeric(input$IC_number),1,2))>=21){
      print(2020-(as.numeric(substr(as.numeric(input$IC_number),1,2))+1900))
    }else{
      print("Invalid ID")
    }
    x
  })
  user_data_BL <- reactive({
    data_convert <- substr(as.numeric(input$IC_number),7,8)
    y <- if(data_convert=='01'||data_convert=='21'||data_convert=='22'||data_convert=='23'||data_convert=='24'){
      print("Johor")
    }else if(data_convert=='02'||data_convert=='25'||data_convert=='26'||data_convert=='27'){
      print("Kedah")
    }else if(data_convert=='03'||data_convert=='28'||data_convert=='29'){
      print("Kelantan")
    }else if(data_convert=='04'||data_convert=='30'){
      print("Malacca")
    }else if(data_convert=='05'||data_convert=='31'||data_convert=='59'){
      print("Negeri Sembilan")
    }else if(data_convert=='06'||data_convert=='32'||data_convert=='33'){
      print("Pahang")
    }else if(data_convert=='07'||data_convert=='34'||data_convert=='35'){
      print("Penang")
    }else if(data_convert=='08'||data_convert=='36'||data_convert=='37'||data_convert=='38'||data_convert=='39'){
      print("Perak")
    }else if(data_convert=='09'||data_convert=='40'){
      print("Perlis")
    }else if(data_convert=='10'||data_convert=='41'||data_convert=='42'||data_convert=='43'||data_convert=='44'){
      print("Selangor")
    }else if(data_convert=='11'||data_convert=='45'||data_convert=='46'){
      print("Terengganu")
    }else if(data_convert=='12'||data_convert=='47'||data_convert=='48'||data_convert=='49'){
      print("Sabah")
    }else if(data_convert=='13'||data_convert=='50'||data_convert=='51'||data_convert=='52'||data_convert=='53'){
      print("Sarawak")
    }else if(data_convert=='14'||data_convert=='54'||data_convert=='55'||data_convert=='56'||data_convert=='57'){
      print("Federal Territory of Kuala Lumpur")
    }else if(data_convert=='15'||data_convert=='58'){
      print("Federal Territory of Labuan")
    }else if(data_convert=='16'){
      print("Federal Territory of Putrajaya")
    }else{
      print("Invalid ID")
    }
    y
  })
  user_data_G <- reactive({
    convert <- as.numeric(substr(as.numeric(input$IC_number),12,12))
    z <- if(convert%%2==0){
      print("Female")
    }else if(convert%%2!=0){
      print("Male")
    }else{
      print("Invalid ID")
    }
    z
  })
    output$User_Age <- renderText({user_data_age()})
    output$User_BL <- renderText({user_data_BL()})
    output$User_G <- renderText({user_data_G()})
  }


shinyApp(ui = ui, server = server)