text1<-c("This web application is intended to help study terrorist attacks and terrorism thanks to the use of the GTD. There is no user guide as this app is designed to be user-friendly. All errors are due to its author: Ivan Adolfo Lorenzo De Gracia")
text2<-c("National Consortium for the Study of Terrorism and Responses to Terrorism
(START). (2017). Global Terrorism Database [Data file]. Retrieved from http://www.start.umd.edu/gtd")

#Data load
load("dataPanelV5.rda")
load("countries.rda")
load("Nationality_of_Target_or_Victim_nulo.rda")
load("weapons_nulo.rda")
load("weapons_subtype.rda")
load("terrorist_groups.rda")
load("terrorist_groups_capped.rda")
load("Target_or_Victim_Subtype.rda")
load("years.rda")

#packages load
library(shiny)
library(sqldf)
library(ggplot2)
library(leaflet)
library(DT)
library(htmltools)
library(RColorBrewer)
library(lemon)

ui <- fluidPage(tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
),
tabsetPanel(              
    tabPanel(title = "Main - Introduction",
      h2("Introduction - Purpose of this app"),br(),p(text1),br(),p(text2)
      
    ),

# Tab 1 - Terrorist Attacks
    navbarMenu(title = "Pane 1 - Terrorist attacks",
      #1.1 Two countries comparison
      tabPanel(title = "1.1- Two countries comparison",h2("1.1- Total number of attacks by year - Two countries comparison"),
               br(),
               helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
               selectInput(inputId = 'sel_country111', label = 'First country to compare',choices=countries,selected = countries[1]),
               selectInput(inputId = 'sel_country112', label = 'Second country to compare',choices=countries,selected = "Albania"),
               plotOutput("barplot11")
      ),
      #1.2 Type of attack
      tabPanel(title = "1.2- Type of attack",h2("1.2- Type of attack in a country by year"),br(),
               helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
               helpText("First choose the country and then the perpetrator:"),
               selectInput(inputId = 'sel_country12', label = 'Choose the country',choices=countries,selected = countries[1]),uiOutput("use_perpetrator12"),
               plotOutput(outputId = "barplot12")
               
      ),
      #1.3 Suicide attacks
      tabPanel(title = "1.3- Suicide attacks",h2("1.3- Number of suicide attacks in a country by year"),br(),
               helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
               helpText("First choose the country and then the perpetrator:"),
               selectInput(inputId = 'sel_country13', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator13"),
               plotOutput("barplot13")
        ),
      #1.4 Attacks by month
      tabPanel(title = "1.4- Attacks by month",h2("1.4- Number of attacks by month by year in a country"),
               br(),
               helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
               helpText("First choose the country and then the perpetrator:"),
               selectInput(inputId = 'sel_country14', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator14"),
               plotOutput("barplot14")
               
      )
    ),  

# Tab 2 - Weapons used
navbarMenu(title = "Pane 2 - Weapons",
           #2.1 Weapon type used in a country
           tabPanel(title = "2.1- Weapon type was used in a country",h2("2.1- Number of times a type of weapon was used in terrorist attacks in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country21', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator21"),
                    plotOutput("barplot21")
           ),
           #2.2 Weapon subtype used in a country
           tabPanel(title = "2.2- Weapon subtype used in a country",h2("2.2- Number of times a subtype of weapon was used in terrorist attacks in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country22', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator22"),
                    plotOutput("barplot22")
                    
           ),
           #2.3 Countries where a Weapon type was used
           tabPanel(title = "2.3- Countries where a weapon type was used",h2("2.3- Countries where a weapon type was used by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_weapon23', label = 'Choose weapon type to explore',choices=weapons_nulo,selected = weapons_nulo[1]),
                    plotOutput("barplot23")
                    
           ),
           #2.4 Countries where a Weapon subtype was used
           tabPanel(title = "2.4- Countries where a weapon subtype was used",h2("2.4- Countries where a weapon subtype was used by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    selectInput(inputId = 'sel_weapon_subtype24', label = 'Choose weapon subtype to explore',choices=weapons_subtype,selected = weapons_subtype[23]),
                    plotOutput("barplot24")
                    
           )
),

# Tab 3 - Targets
navbarMenu(title = "Pane 3 - Targets",
           #3.1 Attacked target type
           tabPanel(title = "3.1- Attacked target type",h2("3.1- Number of attacks by type of attacked target in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country31', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator31"),
                    plotOutput("barplot31")
           ),
           #3.2 Attacket target subype
           tabPanel(title = "3.2- Attacked target subype",h2("3.2- Detail of the subtype of attacked target in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country32', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator32"),
                    plotOutput("barplot32")
                    
           ),
           #3.3 Attacked target nationality by country
           tabPanel(title = "3.3- Attacked target nationality by country",h2("3.3- Number of attacks by nationality of the attacked target in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country33', label = 'Choose the country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator33"),
                    plotOutput("barplot33")
                    
           ),
           #3.4 Attacked countries with a specific target nationality
           tabPanel(title = "3.4- Attacked targets with a specific nationality",h2("3.4- Number of attacks suffered by targets of a specific nationality in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_nationality34', label = 'Choose the nationality of the target',choices=Nationality_of_Target_or_Victim_nulo,selected = Nationality_of_Target_or_Victim_nulo[1]),uiOutput("use_perpetrator34"),
                    plotOutput("barplot34")
                    
           )
),

# Tab 4 - Perpetrators
navbarMenu(title = "Pane 4 - Perpetrators",
           #4.1 Attacks carried out by perpetrator
           tabPanel(title = "4.1- Attacks carried out by a perpetrator in a country",h2("4.1- Number of attacks carried out by a perpetrator in a country by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country41', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator41"),
                    plotOutput("barplot41")
           ),
           #4.2 Number of perpetrators acting in country
           tabPanel(title = "4.2- Number of perpetrators acting in country",h2("4.2- Number of perpetrators acting in a country by perpetrator and year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country42', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator42"),
                    plotOutput("barplot42")
                    
           ),
           #4.3 Number of perpetrators captured in country
           tabPanel(title = "4.3- Number of perpetrators captured in country",h2("4.3- Number of perpetrators captured in a country by perpetrator and year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country43', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator43"),
                    plotOutput("barplot43")
                    
           ),
           #4.4 Overall number of perpertrator fatalities
           tabPanel(title = "4.4- Number of perpetrator fatalities in country",h2("4.4- Number of perpetrator fatalities in a country by perpetrator and year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country44', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator44"),
                    plotOutput("barplot44")
                     
           ),
           #4.5 Overall number of injured perpetrators
           tabPanel(title = "4.5- Number of injured perpetrators in a country",h2("4.5- Number of injured perpetrators in a country by perpetrator and year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country45', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator45"),
                    plotOutput("barplot45")
           ),
           #4.6 Countries where a perpetrator attacked
           tabPanel(title = "4.6- Countries where a perpetrator attacked",h2("4.6- Countries and number of attacks by country where a perpetrator attacked by year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    selectInput(inputId = 'sel_perpetrator46', label = 'Choose perpetrator to explore',choices=terrorist_groups_capped,selected = terrorist_groups_capped[1]),
                    plotOutput("barplot46")
           )
),

# Tab 5 - Victims
navbarMenu(title = "Pane 5 - Victims",
           #5.1 Number of of victim fatalities by a perpetrator
           tabPanel(title = "5.1- Number of victim fatalities in a country by a perpertrator",h2("5.1- Number of victim fatalities in a country by perpetrator and year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country51', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator51"),
                    plotOutput("barplot51")
                    
           ),
           #5.2 Number of injured victims by a perpetrator
           tabPanel(title = "5.2- Number of injured victims in a country by a perpertrator",h2("5.2- Number of injured victims in a country by perpetrator and year"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country52', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator52"),
                    plotOutput("barplot52")
                                                                                               
           )
),

# Tab 6 - Property damage
navbarMenu(title = "Pane 6 - Property damage",
           #6.1- Number of atacks by type of Property damage
           tabPanel(title = "6.1- Number of atacks by type of property damage",h2("6.1- Number of atacks by type of Property damage, perpetrator and year in a country"),
                    br(),
                    helpText("Some queries may require between 30 s and 60 s. Please, be patient. Thank you."),
                    helpText("First choose the country and then the perpetrator:"),
                    selectInput(inputId = 'sel_country61', label = 'Country to explore',choices=countries,selected = countries[1]),uiOutput("use_perpetrator61"),
                    plotOutput("barplot61")
           )
),

# Tab 7 - Maps
navbarMenu(title = "Pane 7 - Map. Attacks distribution",
           #7.1 Attacks carried out by perpetrator in a country
           tabPanel(title = "7.1- Map. Attacks carried out in a country by year",h2("7.1- Map. Number of attacks carried out in a country by year"),
                    br(),
                    helpText("First choose the country and then the year:"),
                    br(),
                    helpText("If after the selection, no data is shown, that is because there are no data for that year. Please, be patient. Thank you."),
                    fluidRow(column(6,selectInput(inputId = 'sel_country71', label = 'Country to explore',choices=countries,selected = countries[1])),column(6,selectInput(inputId = 'sel_year71', label = 'Year to explore',choices=years,selected = years[1]))),fluidRow(leafletOutput("map71",height=600))

                    
  )


)))

server <- function(input, output) {
  
# Tab 1 - Terrorist Attacks 
  #1.1 Two countries comparison
  

  
    country11<- reactive({as.data.frame(matrix(c(input$sel_country112,input$sel_country111),2,1))})
  question11<- reactive({
    
    country111<- country11()
    sqldf("SELECT dp.Country_name, dp.Year, COUNT (*) as num_ataques11 FROM dataPanelV5 dp, country111 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Country_name")})
  
  n_facets11<-function(){
                  question111<- question11()
                  return (500*length(unique(question111$Country_name)))}
  
  output$barplot11 <- renderPlot({ 
    
    question111<- question11()
    
    ggplot(question111,aes(x=factor(Year),y=num_ataques11,fill=Country_name)) + geom_bar(stat="identity") + facet_rep_grid(Country_name ~ .,scales = "free", repeat.tick.labels=TRUE) + geom_text(aes(label=num_ataques11), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Countries", x="Year")
    
  },height = n_facets11)
  
  #1.2 Type of attack
  
  output$use_perpetrator12 <- renderUI({
    
    use_country12 <-as.data.frame(matrix(c(input$sel_country12),1,1))
    
    perpetratorList12<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country12 p WHERE dp.Country_name = p.V1")
    
    perpetratorList12<-sort(as.vector(as.matrix(perpetratorList12)))
    
    perpetratorList12[length(perpetratorList12) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator12', label = 'Choose perpetrator to explore',choices=perpetratorList12,selected = perpetratorList12[length(perpetratorList12)])
    
    
  })
  
  perpetrator12<- reactive({as.data.frame(matrix(c(input$sel_perpetrator12),1,1))})
  
  country12<- reactive({as.data.frame(matrix(c(input$sel_country12),1,1))})
  
  observeEvent(c(input$sel_country12,input$sel_perpetrator12),{ 
    
    question12<- reactive({
      
      country121<- country12()
      
      perpetrator121<- perpetrator12()
      
      if (perpetrator121 =="All"){
      
        sqldf("SELECT dp.Year, dp.Attack_Type_text, COUNT (*) as num_ataques12 FROM dataPanelV5 dp, country121 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Attack_Type_text")
        
      } else {
      

      
      sqldf("SELECT dp.Year, dp.Attack_Type_text, COUNT (*) as num_ataques12 FROM dataPanelV5 dp, country121 p, perpetrator121 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Attack_Type_text")
      }
      
    })
    
    n_facets12<-function(){
      question121<- question12()
      aux_var<-500*length(unique(question121$Attack_Type_text))
      if(aux_var!=0){
        return (aux_var)
      } else {
        return (500)}
    }
    
    output$barplot12 <- renderPlot({ 
      
      question121<- question12() 
      
      if(length(unique(question121$Attack_Type_text))!=0){
        
        ggplot(question121,aes(x=factor(Year),y=num_ataques12,fill=Attack_Type_text)) + geom_bar(stat="identity") + facet_rep_grid(Attack_Type_text ~ .,scales = "free", repeat.tick.labels=TRUE) +
          geom_text(aes(label=num_ataques12), vjust=-0.2, colour="black")  + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Type of attack", x="Year")
        
      } else {
        showNotification("No matches")
      }
      
    },height = n_facets12)
    
  })
  
  #1.3 Suicide attacks
  
output$use_perpetrator13 <- renderUI({
          
          use_country13 <-as.data.frame(matrix(c(input$sel_country13),1,1))
          
          perpetratorList13<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country13 p WHERE dp.Country_name = p.V1 AND dp.Suicide_Attack = 1")
          
          perpetratorList13<-sort(as.vector(as.matrix(perpetratorList13)))
          
          perpetratorList13[length(perpetratorList13) + 1] <- "All"
          
          selectInput(inputId = 'sel_perpetrator13', label = 'Choose perpetrator to explore',choices=perpetratorList13,selected = perpetratorList13[length(perpetratorList13)])
          
        })


  perpetrator13<- reactive({as.data.frame(matrix(c(input$sel_perpetrator13),1,1))})
  
  country13<- reactive({as.data.frame(matrix(c(input$sel_country13),1,1))})
  
  observeEvent(c(input$sel_country13,input$sel_perpetrator13),{ 
    
      question13<- reactive({
        
        country131<- country13()
        
        perpetrator131<- perpetrator13()
        
        if (perpetrator131 =="All"){
          
          sqldf("SELECT dp.Year, dp.Country_name, dp.Suicide_Attack, COUNT (*) as num_ataques13 FROM dataPanelV5 dp, country131 p WHERE dp.Country_name = p.V1 AND dp.Suicide_Attack = 1 GROUP BY dp.Year")
          
          
          
        } else {
        
        sqldf("SELECT dp.Year, dp.Suicide_Attack, dp.Perpetrator_Group_Name, COUNT (*) as num_ataques13 FROM dataPanelV5 dp, country131 p, perpetrator131 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Suicide_Attack = 1 GROUP BY dp.Year")
        }
          })
      
      output$barplot13 <- renderPlot({ 
        
        question131<- question13() 
        
        perpetrator131<- perpetrator13()
        
        if(length(unique(question131$Suicide_Attack))!=0){
          
          if (perpetrator131 =="All"){
            
            ggplot(question131,aes(x=factor(Year),y=num_ataques13,fill=Country_name)) + geom_col() + geom_text(aes(label=num_ataques13), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Suicide attacks by all perpetrators", x="Year") + scale_fill_manual(values=c("#006400"))
            
            
          } else {
          
          ggplot(question131,aes(x=factor(Year),y=num_ataques13,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_ataques13), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Suicide attacks by specific perpetrator", x="Year") + scale_fill_manual(values=c("#006400"))
          }
          
        } else {

          showNotification("No matches")
        }
        
    },height = 500)
  
})  
  

  #1.4 Attacks by month
  
  
  output$use_perpetrator14 <- renderUI({
    
    use_country14 <-as.data.frame(matrix(c(input$sel_country14),1,1))
    
    perpetratorList14<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country14 p WHERE dp.Country_name = p.V1")
    
    perpetratorList14<-sort(as.vector(as.matrix(perpetratorList14)))
    
    perpetratorList14[length(perpetratorList14) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator14', label = 'Choose perpetrator to explore',choices=perpetratorList14,selected = perpetratorList14[length(perpetratorList14)])
    

  })
  
  
  output$barplot14 <- renderPlot({ 
    
    country14<-as.data.frame(matrix(c(input$sel_country14),1,1))
    
    perpetrator14<- as.data.frame(matrix(c(input$sel_perpetrator14),1,1))
    
    if (perpetrator14 =="All"){

      question14 <-sqldf("SELECT dp.Month, dp.Year, COUNT (*) as num_ataques14 FROM dataPanelV5 dp, country14 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Month")
      
    } else {
      
      question14 <-sqldf("SELECT dp.Month, dp.Year, COUNT (*) as num_ataques14 FROM dataPanelV5 dp, country14 p, perpetrator14 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Month")
      

    }
      
    if(length(unique(question14$Month))!=0){
    
      ggplot(question14, aes(y=factor(Month), x=factor(Year), fill=num_ataques14)) + scale_y_discrete(limits=c("12-December","11-November","10-October","09-September","08-August","07-July","06-June","05-May_","04-April","03-March","02-February","01-January","00-Unknown" )) + geom_tile()  + scale_fill_gradient(low="yellow", high="red", limits=c(0, max(question14$num_ataques14))) +  scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016)), position = "top") + labs(fill="Number of attacks", x="Year", y="Month") + theme(legend.position="top") + theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "grey"))
      
    } else {

      showNotification("No matches")
    }
    
  },height = 500)
  
  
# Tab 2 - Weapons used 
  
  #2.1 Weapon type used in a country
  
  output$use_perpetrator21 <- renderUI({
    
    use_country21 <-as.data.frame(matrix(c(input$sel_country21),1,1))
    
    perpetratorList21<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country21 p WHERE dp.Country_name = p.V1")
    
    perpetratorList21<-sort(as.vector(as.matrix(perpetratorList21)))
    
    perpetratorList21[length(perpetratorList21) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator21', label = 'Choose perpetrator to explore',choices=perpetratorList21,selected = perpetratorList21[length(perpetratorList21)])
    
  })
  
  
  perpetrator21<- reactive({as.data.frame(matrix(c(input$sel_perpetrator21),1,1))})
  
  country21<- reactive({as.data.frame(matrix(c(input$sel_country21),1,1))})
  
  observeEvent(c(input$sel_country21,input$sel_perpetrator21),{ 
    
      question21<- reactive({
        
        country211<- country21()
        
        perpetrator211<- perpetrator21()
        
        if (perpetrator211 =="All"){
          
          sqldf("SELECT dp.Year, dp.Weapon_Type_text, COUNT (*) as num_ataques21 FROM dataPanelV5 dp, country211 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Weapon_Type_text")
          
          
        } else {
          
          sqldf("SELECT dp.Year, dp.Weapon_Type_text, COUNT (*) as num_ataques21 FROM dataPanelV5 dp, country211 p, perpetrator211 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Weapon_Type_text")  
          
        }
        
})
      
      n_facets21<-function(){
        question211<- question21()
        aux_var<-500*length(unique(question211$Weapon_Type_text))
        if(aux_var!=0){
          return (aux_var)
        } else {
          return (500)}
      }
      
      output$barplot21 <- renderPlot({ 
        
        question211<- question21() 
        
        if(length(unique(question211$Weapon_Type_text))!=0){
          
          ggplot(question211,aes(x=factor(Year),y=num_ataques21,fill=Weapon_Type_text)) + geom_bar(stat="identity") + facet_rep_grid(Weapon_Type_text ~ .,scales = "free", repeat.tick.labels=TRUE) +
            geom_text(aes(label=num_ataques21), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank())  + labs(fill="Weapon type", x="Year")
          
        } else {
          showNotification("No matches")
        }
        
      },height = n_facets21)
  
  
})  
 
  #2.2 Weapon subtype used in a country
  
  output$use_perpetrator22 <- renderUI({
    
    use_country22 <-as.data.frame(matrix(c(input$sel_country22),1,1))
    
    perpetratorList22<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country22 p WHERE dp.Country_name = p.V1 AND dp.Weapon_Subtype_text <> 'Not applicable'")
    
    perpetratorList22<-sort(as.vector(as.matrix(perpetratorList22)))
    
    perpetratorList22[length(perpetratorList22) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator22', label = 'Choose perpetrator to explore',choices=perpetratorList22,selected = perpetratorList22[length(perpetratorList22)])
    
    
  })
  
  
  perpetrator22<- reactive({as.data.frame(matrix(c(input$sel_perpetrator22),1,1))})
  
  country22<- reactive({as.data.frame(matrix(c(input$sel_country22),1,1))})
  
  observeEvent(c(input$sel_country22,input$sel_perpetrator22),{
    
    question22<- reactive({
      
      country221<- country22()
      
      perpetrator221<- perpetrator22()
      
      if (perpetrator221 =="All"){
        
        sqldf("SELECT dp.Year, dp.Weapon_Subtype_text, COUNT (*) as num_ataques22 FROM dataPanelV5 dp, country221 p WHERE dp.Country_name = p.V1 AND dp.Weapon_Subtype_text <> 'Not applicable' GROUP BY dp.Year, dp.Weapon_Subtype_text")
        
        
      } else {
        
        sqldf("SELECT dp.Year, dp.Weapon_Subtype_text, COUNT (*) as num_ataques22 FROM dataPanelV5 dp, country221 p, perpetrator221 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Weapon_Subtype_text <> 'Not applicable' GROUP BY dp.Year, dp.Weapon_Subtype_text")
        
        
      }
      
      })
    
    n_facets22<-function(){
      question221<- question22()
      aux_var<-500*length(unique(question221$Weapon_Subtype_text))
      if(aux_var!=0){
        return (aux_var)
      } else {
        return (500)}
      }
  
    output$barplot22 <- renderPlot({ 
      
      question221<- question22() 
      
      if(length(unique(question221$Weapon_Subtype_text))!=0){
        
        ggplot(question221,aes(x=factor(Year),y=num_ataques22,fill=Weapon_Subtype_text)) + geom_bar(stat="identity") + facet_rep_grid(Weapon_Subtype_text ~ .,scales = "free", repeat.tick.labels=TRUE) +
          geom_text(aes(label=num_ataques22), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Weapon subtypes", x="Year")
        
      } else {
        showNotification("No matches")
      }
      
    },height = n_facets22)
    
})
  
  #2.3 Countries where a Weapon type was used
  
  weapon23<- reactive({as.data.frame(matrix(c(input$sel_weapon23),1,1))})
  
  question23<- reactive({
    
    weapon231<- weapon23()
    
    sqldf("SELECT dp.Year, dp.Country_name, COUNT (*) as num_ataques23 FROM dataPanelV5 dp, weapon231 w WHERE dp.Weapon_Type_text = w.V1 GROUP BY dp.Year, dp.Country_name")})
  
  n_facets23<-function(){
    question231<- question23()
    aux_var<-500*length(unique(question231$Country_name))
    if(aux_var!=0){
      return (500*length(unique(question231$Country_name)))
    } else {
      return (500)}
  }
  
  output$barplot23 <- renderPlot({ 
    
    question231<- question23()
    
    if(length(unique(question231$Country_name))!=0){
      
      ggplot(question231,aes(x=factor(Year),y=num_ataques23,fill=Country_name)) + geom_bar(stat="identity") + facet_rep_grid(Country_name ~ .,scales = "free", repeat.tick.labels=TRUE) + geom_text(aes(label=num_ataques23), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of attacks where a weapon type was used by country and year", x="Year")
      
    } else {

      showNotification("No matches")
    }
    
  },height = n_facets23)  
  
  
  #2.4 Countries where a Weapon subtype was used
  
  weapon_subtype24<- reactive({as.data.frame(matrix(c(input$sel_weapon_subtype24),1,1))})
  
  question24<- reactive({
    
    weapon_subtype241<- weapon_subtype24()
    
    sqldf("SELECT dp.Year, dp.Country_name, COUNT (*) as num_ataques24 FROM dataPanelV5 dp, weapon_subtype241 ws WHERE dp.Weapon_Subtype_text = ws.V1 GROUP BY dp.Year, dp.Country_name")})
  
  n_facets24<-function(){
    question241<- question24()
    aux_var<-500*length(unique(question241$Country_name))
    if(aux_var!=0){
      return (500*length(unique(question241$Country_name)))
    } else {
      return (500)}
  }
  
  output$barplot24 <- renderPlot({ 
    
    question241<- question24()
    
    if(length(unique(question241$Country_name))!=0){
      
      ggplot(question241,aes(x=factor(Year),y=num_ataques24,fill=Country_name)) + geom_bar(stat="identity") + facet_rep_grid(Country_name ~ .,scales = "free", repeat.tick.labels=TRUE) + geom_text(aes(label=num_ataques24), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of attacks where a weapon subtype was used by country and year", x="Year")
      
    } else {

      showNotification("No matches")
    }
    
  },height = n_facets24)  
  
  
  
# Tab 3 - Targets 
  #3.1 Attacked target type

output$use_perpetrator31 <- renderUI({
  
  use_country31 <-as.data.frame(matrix(c(input$sel_country31),1,1))
  
  perpetratorList31<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country31 p WHERE dp.Country_name = p.V1 AND dp.Target_or_Victim_Type_1_text <> 'Not applicable'")
  
  perpetratorList31<-sort(as.vector(as.matrix(perpetratorList31)))
  
  perpetratorList31[length(perpetratorList31) + 1] <- "All"
  
  selectInput(inputId = 'sel_perpetrator31', label = 'Choose perpetrator to explore',choices=perpetratorList31,selected = perpetratorList31[length(perpetratorList31)])
  
  
})

  
  perpetrator31<- reactive({as.data.frame(matrix(c(input$sel_perpetrator31),1,1))})
  
  country31<- reactive({as.data.frame(matrix(c(input$sel_country31),1,1))})
  
  observeEvent(c(input$sel_country31,input$sel_perpetrator31),{ 
    
        question31<- reactive({
        
            country311<- country31()
            
            perpetrator311<- perpetrator31()
            
            if (perpetrator311 =="All"){
              
              sqldf("SELECT dp.Year, dp.Target_or_Victim_Type_1_text, COUNT (*) as num_ataques31 FROM dataPanelV5 dp, country311 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Target_or_Victim_Type_1_text")
              
              
            } else {
              
              sqldf("SELECT dp.Year, dp.Target_or_Victim_Type_1_text, COUNT (*) as num_ataques31 FROM dataPanelV5 dp, country311 p, perpetrator311 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Target_or_Victim_Type_1_text")
              
            }
            
            })
            
            n_facets31<-function(){
              question311<- question31()
              aux_var<-500*length(unique(question311$Target_or_Victim_Type_1_text))
              if(aux_var!=0){
                return (aux_var)
              } else {
                return (500)}
            }
      
            output$barplot31 <- renderPlot({ 
              
              question311<- question31() 
              
              if(length(unique(question311$Target_or_Victim_Type_1_text))!=0){
                
                ggplot(question311,aes(x=factor(Year),y=num_ataques31,fill=Target_or_Victim_Type_1_text)) + geom_bar(stat="identity") + facet_rep_grid(Target_or_Victim_Type_1_text ~ .,scales = "free", repeat.tick.labels=TRUE) + geom_text(aes(label=num_ataques31), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Attacked target type", x="Year")
                
              } else {
                showNotification("No matches")
              }
              
            },height = n_facets31)
        
  
  })
  

  #3.2 Attacket target subype
  
  output$use_perpetrator32 <- renderUI({
    
    use_country32 <-as.data.frame(matrix(c(input$sel_country32),1,1))
    
    perpetratorList32<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country32 p WHERE dp.Country_name = p.V1")
    
    perpetratorList32<-sort(as.vector(as.matrix(perpetratorList32)))
    
    perpetratorList32[length(perpetratorList32) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator32', label = 'Choose perpetrator to explore',choices=perpetratorList32,selected = perpetratorList32[length(perpetratorList32)])
    
    
  })

  output$barplot32 <- renderPlot({ 
    
    country32<- as.data.frame(matrix(c(input$sel_country32),1,1))
    perpetrator32<- as.data.frame(matrix(c(input$sel_perpetrator32),1,1))
    
    if (perpetrator32 =="All"){
      
      question32 <-sqldf("SELECT dp.Target_or_Victim_Subtype_1_text, dp.Year, COUNT (*) as num_ataques32 FROM dataPanelV5 dp, country32 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Target_or_Victim_Subtype_1_text")
      
      
    } else {
      
      question32 <-sqldf("SELECT dp.Target_or_Victim_Subtype_1_text, dp.Year, COUNT (*) as num_ataques32 FROM dataPanelV5 dp, country32 p, perpetrator32 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Target_or_Victim_Subtype_1_text")
      
      
    }
    
      
    if(length(unique(question32$Target_or_Victim_Subtype_1_text))!=0){
    
      ggplot(question32, aes(y=factor(Target_or_Victim_Subtype_1_text), x=factor(Year), fill=num_ataques32)) + geom_tile() + scale_fill_gradient2(low = "yellow", mid = "red", high = "blue", midpoint = max(question32$num_ataques32)/2) +  scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016)),position = "top") + scale_y_discrete(limits=Target_or_Victim_Subtype,breaks=Target_or_Victim_Subtype,labels=Target_or_Victim_Subtype) + theme(legend.position="top") + labs(fill="Target or Victim Subtype", x="Year", y="Subtype of target or victim")+ theme(panel.background = element_blank(),panel.grid.major = element_line(colour = "grey"))
      

    } else {
      showNotification("No matches")
    }

  },height = 2000)
  
  
  #3.3 Attacked target nationality by country
  
  output$use_perpetrator33 <- renderUI({
  
    use_country33 <-as.data.frame(matrix(c(input$sel_country33),1,1))
    
    perpetratorList33<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country33 p WHERE dp.Country_name = p.V1 AND dp.Nationality_of_Target_or_Victim_text <> 'Not applicable'")
    
    perpetratorList33<-sort(as.vector(as.matrix(perpetratorList33)))
    
    perpetratorList33[length(perpetratorList33) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator33', label = 'Choose perpetrator to explore',choices=perpetratorList33,selected = perpetratorList33[length(perpetratorList33)])
  
  
})
  
  perpetrator33<- reactive({as.data.frame(matrix(c(input$sel_perpetrator33),1,1))})
  
  country33<- reactive({as.data.frame(matrix(c(input$sel_country33),1,1))})
  
  observeEvent(c(input$sel_country33,input$sel_perpetrator33),{ 
    
      question33<- reactive({
        
        country331<- country33()
      
        perpetrator331<- perpetrator33()
        
        if (perpetrator331 =="All"){
          
          sqldf("SELECT dp.Year, dp.Nationality_of_Target_or_Victim_text, COUNT (*) as num_ataques33 FROM dataPanelV5 dp, country331 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year, dp.Nationality_of_Target_or_Victim_text")
          
        } else {
          
          sqldf("SELECT dp.Year, dp.Nationality_of_Target_or_Victim_text, COUNT (*) as num_ataques33 FROM dataPanelV5 dp, country331 p, perpetrator331 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Nationality_of_Target_or_Victim_text")
          
        }
        
        })
      
      n_facets33<-function(){
        question331<- question33()
        aux_var<-500*length(unique(question331$Nationality_of_Target_or_Victim_text))
        if(aux_var!=0){
          return (aux_var)
        } else {
          return (500)}
      }
      
      output$barplot33 <- renderPlot({ 
        
        question331<- question33() 
        
        if(length(unique(question331$Nationality_of_Target_or_Victim_text))!=0){
          
          ggplot(question331,aes(x=factor(Year),y=num_ataques33,fill=Nationality_of_Target_or_Victim_text)) + geom_bar(stat="identity") + facet_rep_grid(Nationality_of_Target_or_Victim_text ~ .,scales = "free", repeat.tick.labels=TRUE) +
            geom_text(aes(label=num_ataques33), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Nationality of Target or Victim", x="Year")
          
        } else {
          showNotification("No matches")
        }
        
      },height = n_facets33)
  
  })
 
  
  #3.4 Attacked countries with a specific target nationality
  
  output$use_perpetrator34 <- renderUI({
  
    use_nationality34 <-as.data.frame(matrix(c(input$sel_nationality34),1,1))
    
    perpetratorList34<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_nationality34 n WHERE dp.Nationality_of_Target_or_Victim_text = n.V1 AND dp.Nationality_of_Target_or_Victim_text <> 'Not applicable'")
    
    perpetratorList34<-sort(as.vector(as.matrix(perpetratorList34)))
    
    perpetratorList34[length(perpetratorList34) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator34', label = 'Choose perpetrator to explore',choices=perpetratorList34,selected = perpetratorList34[length(perpetratorList34)])
  
  
})
  
  perpetrator34<- reactive({as.data.frame(matrix(c(input$sel_perpetrator34),1,1))})
  
  nationality34<- reactive({as.data.frame(matrix(c(input$sel_nationality34),1,1))})
  
  observeEvent(c(input$sel_nationality34,input$sel_perpetrator34),{ 
    
    question34<- reactive({
      
      nationality341<- nationality34()
      
      perpetrator341<- perpetrator34()
      
      if (perpetrator341 =="All"){
        
        sqldf("SELECT dp.Year, dp.Country_name, COUNT (*) as num_ataques34 FROM dataPanelV5 dp, nationality341 n WHERE dp.Nationality_of_Target_or_Victim_text = n.V1 GROUP BY dp.Year, dp.Country_name")
        
        
      } else {
        
        sqldf("SELECT dp.Year, dp.Country_name, COUNT (*) as num_ataques34 FROM dataPanelV5 dp, nationality341 n, perpetrator341 per WHERE dp.Nationality_of_Target_or_Victim_text = n.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Country_name")
        
      }

           })
    
    n_facets34<-function(){
      question341<- question34()
      aux_var<-500*length(unique(question341$Country_name))
      if(aux_var!=0){
        return (aux_var)
      } else {
        return (500)}
    }
    
    output$barplot34 <- renderPlot({ 
      
      question341<- question34() 
      
      if(length(unique(question341$Country_name))!=0){
        
        ggplot(question341,aes(x=factor(Year),y=num_ataques34,fill=Country_name)) + geom_bar(stat="identity") + facet_rep_grid(Country_name ~ .,scales = "free", repeat.tick.labels=TRUE) +
          geom_text(aes(label=num_ataques34), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Country where the target or victim was attacked", x="Year")
        
      } else {
        showNotification("No matches")
      }
      
    },height = n_facets34)
  
}) 
  

# Tab 4 - Perpetrators  
  
  #4.1 Attacks carried out by perpetrator
  
  output$use_perpetrator41 <- renderUI({
    
    use_country41 <-as.data.frame(matrix(c(input$sel_country41),1,1))
    
    perpetratorList41<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country41 p WHERE dp.Country_name = p.V1")
    
    perpetratorList41<-sort(as.vector(as.matrix(perpetratorList41)))
    
    perpetratorList41[length(perpetratorList41) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator41', label = 'Choose perpetrator to explore',choices=perpetratorList41,selected = perpetratorList41[length(perpetratorList41)])
    
    
  }) 
  
  
  output$barplot41 <- renderPlot({ 
    
    country41<-as.data.frame(matrix(c(input$sel_country41),1,1))
    perpetrator41<-as.data.frame(matrix(c(input$sel_perpetrator41),1,1))
    
    if (perpetrator41 =="All"){
      
      question41 <-sqldf("SELECT dp.Year, dp.Country_name, COUNT (*) as num_ataques41 FROM dataPanelV5 dp, country41 p WHERE dp.Country_name = p.V1 GROUP BY dp.Year")
      
      
    } else {
    
    question41 <-sqldf("SELECT dp.Year, dp.Perpetrator_Group_Name, COUNT (*) as num_ataques41 FROM dataPanelV5 dp, country41 p, perpetrator41 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year")
    
    }
          
          if(length(unique(question41$Year))!=0){
            
            if (perpetrator41 =="All"){
              
              ggplot(question41,aes(x=factor(Year),y=num_ataques41,fill=Country_name)) + geom_bar(stat="identity") + geom_text(aes(label=num_ataques41), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of attacks by all perpetrators", x="Year")
              
              
            } else {
    
            ggplot(question41,aes(x=factor(Year),y=num_ataques41,fill=Perpetrator_Group_Name)) + geom_bar(stat="identity") + geom_text(aes(label=num_ataques41), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of attacks by perpetrator:", x="Year")
              
            }
            
          } else{
            showNotification("No matches")
          }
      
  },height = 500)
  
  
  #4.2 Number of perpetrators acting in country
  
  
  output$use_perpetrator42 <- renderUI({
    
    use_country42 <-as.data.frame(matrix(c(input$sel_country42),1,1))
    
    perpetratorList42<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country42 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrators <> -9999 AND dp.Number_of_Perpetrators <> -99 AND dp.Number_of_Perpetrators <> 0")
    
    perpetratorList42<-sort(as.vector(as.matrix(perpetratorList42)))
    
    perpetratorList42[length(perpetratorList42) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator42', label = 'Choose perpetrator to explore',choices=perpetratorList42,selected = perpetratorList42[length(perpetratorList42)])
    
    
  }) 
  
  
  output$barplot42 <- renderPlot({ 
    
    country42<-as.data.frame(matrix(c(input$sel_country42),1,1))
    perpetrator42<-as.data.frame(matrix(c(input$sel_perpetrator42),1,1))
    
    if (perpetrator42 =="All"){
      
      question42 <-sqldf("SELECT dp.Year, dp.Country_name, SUM (dp.Number_of_Perpetrators) as num_perpetrator42 FROM dataPanelV5 dp, country42 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrators <> -9999 AND dp.Number_of_Perpetrators <> -99 AND dp.Number_of_Perpetrators <> 0 GROUP BY dp.Year")
      
    } else {
      
      question42 <-sqldf("SELECT dp.Year, dp.Perpetrator_Group_Name, SUM (dp.Number_of_Perpetrators) as num_perpetrator42 FROM dataPanelV5 dp, country42 p, perpetrator42 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Number_of_Perpetrators <> -9999 AND dp.Number_of_Perpetrators <> -99 AND dp.Number_of_Perpetrators <> 0 GROUP BY dp.Year")
      
    }
    
            
      if(length(unique(question42$Year))!=0){
        
        if (perpetrator42 =="All"){
          
          ggplot(question42,aes(x=factor(Year),y=num_perpetrator42,fill=Country_name)) + geom_col() + geom_text(aes(label=num_perpetrator42), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="All perpetrators", x="Year") + scale_fill_manual(values=c("#4169E1"))
          
          
        } else {
        
    
        ggplot(question42,aes(x=factor(Year),y=num_perpetrator42,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_perpetrator42), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Perpetrator Group Name:", x="Year") + scale_fill_manual(values=c("#4169E1"))
          
        }
              
          } else{
              showNotification("No matches")
          }
 
    
  },height = 500)  
  
  #4.3 Number of perpetrators captured in country
  
  output$use_perpetrator43 <- renderUI({
    
    use_country43 <-as.data.frame(matrix(c(input$sel_country43),1,1))
    
    perpetratorList43<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country43 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrators_Captured <> -9999 AND dp.Number_of_Perpetrators_Captured <> -99 AND dp.Number_of_Perpetrators_Captured <> 0")
    
    perpetratorList43<-sort(as.vector(as.matrix(perpetratorList43)))
    
    perpetratorList43[length(perpetratorList43) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator43', label = 'Choose perpetrator to explore',choices=perpetratorList43,selected = perpetratorList43[length(perpetratorList43)])
    
    
  }) 
  
  output$barplot43 <- renderPlot({ 
    
    country43<-as.data.frame(matrix(c(input$sel_country43),1,1))
    perpetrator43<-as.data.frame(matrix(c(input$sel_perpetrator43),1,1))
    
    if (perpetrator43 =="All"){
      
      question43 <-sqldf("SELECT dp.Year, dp.Country_name, SUM (dp.Number_of_Perpetrators_Captured) as num_perpetrator43 FROM dataPanelV5 dp, country43 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrators_Captured <> -9999 AND dp.Number_of_Perpetrators_Captured <> -99 AND dp.Number_of_Perpetrators_Captured <> 0 GROUP BY dp.Year")
      
    } else {
      
      question43 <-sqldf("SELECT dp.Year, dp.Perpetrator_Group_Name, SUM (dp.Number_of_Perpetrators_Captured) as num_perpetrator43 FROM dataPanelV5 dp, country43 p, perpetrator43 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Number_of_Perpetrators_Captured <> -9999 AND dp.Number_of_Perpetrators_Captured <> -99 AND dp.Number_of_Perpetrators_Captured <> 0 GROUP BY dp.Year")
      
      
    }

          if(length(unique(question43$Year))!=0){
            
            if (perpetrator43 =="All"){
              
              ggplot(question43,aes(x=factor(Year),y=num_perpetrator43,fill=Country_name)) + geom_col() + geom_text(aes(label=num_perpetrator43), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="All perpetrators", x="Year") + scale_fill_manual(values=c("#228B22"))
              
            } else {
    
            ggplot(question43,aes(x=factor(Year),y=num_perpetrator43,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_perpetrator43), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Perpetrator Group Name:", x="Year") + scale_fill_manual(values=c("#228B22"))
              
            }
            
          } else{
            showNotification("No matches")
          }
  },height = 500)   
  
  
  #4.4 Number of perpertrator fatalities
  
  output$use_perpetrator44 <- renderUI({
    
    use_country44 <-as.data.frame(matrix(c(input$sel_country44),1,1))
    
    perpetratorList44<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country44 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrator_Fatalities <> -9999 AND dp.Number_of_Perpetrator_Fatalities <> -99 AND dp.Number_of_Perpetrator_Fatalities <> 0")
    
    perpetratorList44<-sort(as.vector(as.matrix(perpetratorList44)))
    
    perpetratorList44[length(perpetratorList44) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator44', label = 'Choose perpetrator to explore',choices=perpetratorList44,selected = perpetratorList44[length(perpetratorList44)])
    
    
  })
  
  output$barplot44 <- renderPlot({ 
    
    country44<-as.data.frame(matrix(c(input$sel_country44),1,1))
    perpetrator44<-as.data.frame(matrix(c(input$sel_perpetrator44),1,1))
    
    if (perpetrator44 =="All"){
      
      question44 <-sqldf("SELECT dp.Year, dp.Country_name, SUM (dp.Number_of_Perpetrator_Fatalities) as num_perpetrator44 FROM dataPanelV5 dp, country44 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrator_Fatalities <> -9999 AND dp.Number_of_Perpetrator_Fatalities <> -99 AND dp.Number_of_Perpetrator_Fatalities <> 0 GROUP BY dp.Year")
      
    } else {
      
      question44 <-sqldf("SELECT dp.Year, dp.Perpetrator_Group_Name, SUM (dp.Number_of_Perpetrator_Fatalities) as num_perpetrator44 FROM dataPanelV5 dp, country44 p, perpetrator44 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Number_of_Perpetrator_Fatalities <> -9999 AND dp.Number_of_Perpetrator_Fatalities <> -99 AND dp.Number_of_Perpetrator_Fatalities <> 0 GROUP BY dp.Year")
      
    }

    if(length(unique(question44$Year))!=0){
      
      if (perpetrator44 =="All"){
        
        ggplot(question44,aes(x=factor(Year),y=num_perpetrator44,fill=Country_name)) + geom_col() + geom_text(aes(label=num_perpetrator44), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of perpertrator fatalities in a country", x="Year") + scale_fill_manual(values=c("#6A5ACD"))
        
      } else {
        
        ggplot(question44,aes(x=factor(Year),y=num_perpetrator44,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_perpetrator44), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of perpertrator fatalities in a country", x="Year") + scale_fill_manual(values=c("#6A5ACD"))
        
        
      }
    
        } else{
              showNotification("No matches")
            }

  },height = 500)  
  
  
  #4.5 Number of injured perpertrators
  
  output$use_perpetrator45 <- renderUI({
    
    use_country45 <-as.data.frame(matrix(c(input$sel_country45),1,1))
    
    perpetratorList45<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country45 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrators_Injured <> -9999 AND dp.Number_of_Perpetrators_Injured <> -99 AND dp.Number_of_Perpetrators_Injured <> 0")
    
    perpetratorList45<-sort(as.vector(as.matrix(perpetratorList45)))
    
    perpetratorList45[length(perpetratorList45) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator45', label = 'Choose perpetrator to explore',choices=perpetratorList45,selected = perpetratorList45[length(perpetratorList45)])
    
    
  })
  
  output$barplot45 <- renderPlot({ 
    
  country45<-as.data.frame(matrix(c(input$sel_country45),1,1))
  perpetrator45<-as.data.frame(matrix(c(input$sel_perpetrator45),1,1))
  
  if (perpetrator45 =="All"){
    
    question45 <-sqldf("SELECT dp.Year, dp.Country_name, SUM (dp.Number_of_Perpetrators_Injured) as num_perpetrator45 FROM dataPanelV5 dp, country45 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Perpetrators_Injured <> -9999 AND dp.Number_of_Perpetrators_Injured <> -99 AND dp.Number_of_Perpetrators_Injured <> 0 GROUP BY dp.Year")
    
    
  } else {
    
    question45 <-sqldf("SELECT dp.Year, dp.Perpetrator_Group_Name, SUM (dp.Number_of_Perpetrators_Injured) as num_perpetrator45 FROM dataPanelV5 dp, country45 p, perpetrator45 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Number_of_Perpetrators_Injured <> -9999 AND dp.Number_of_Perpetrators_Injured <> -99 AND dp.Number_of_Perpetrators_Injured <> 0 GROUP BY dp.Year")
    
    
  }
  

    if(length(unique(question45$Year))!=0){
      
      if (perpetrator45 =="All"){
        
        ggplot(question45,aes(x=factor(Year),y=num_perpetrator45,fill=Country_name)) + geom_col() + geom_text(aes(label=num_perpetrator45), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of injured perpetrators in the country, all perpetrators", x="Year") + scale_fill_manual(values=c("#e753bc"))
        
        
      } else {
        
        ggplot(question45,aes(x=factor(Year),y=num_perpetrator45,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_perpetrator45), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of injured perpetrators in the country:", x="Year") + scale_fill_manual(values=c("#e753bc"))
      }

        } else{
          showNotification("No matches")
        }
  
},height = 500)    

  
  #4.6 Countries where a perpetrator attacked
  
    perpetrator46<- reactive({as.data.frame(matrix(c(input$sel_perpetrator46),1,1))})
    
    observeEvent(c(input$sel_perpetrator46),{ 
    
    question46<- reactive({
      
      perpetrator461<- perpetrator46()
      
      sqldf("SELECT dp.Year, dp.Country_name, COUNT (*) as num_ataques46 FROM dataPanelV5 dp, perpetrator461 per WHERE dp.Perpetrator_Group_Name = per.V1 GROUP BY dp.Year, dp.Country_name")})
    
    n_facets46<-function(){
      question461<- question46()
      return (500*length(unique(question461$Country_name)))}
    
    output$barplot46 <- renderPlot({ 
      
      question461<- question46()
      
      ggplot(question461,aes(x=factor(Year),y=num_ataques46,fill=Country_name)) + geom_bar(stat="identity") + facet_rep_grid(Country_name ~ .,scales = "free", repeat.tick.labels=TRUE) + geom_text(aes(label=num_ataques46), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of attacks by country and year", x="Year")

  },height = n_facets46)  
  
    })
  
 # Tab 5 - Victims
 #5.1 Number of of victim fatalities by perpetrator
    
  output$use_perpetrator51 <- renderUI({
      
      use_country51 <-as.data.frame(matrix(c(input$sel_country51),1,1))
      
      perpetratorList51<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country51 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Victim_Fatalities <> -9999 AND dp.Number_of_Victim_Fatalities <> -99 AND dp.Number_of_Victim_Fatalities <> 0")
      
      perpetratorList51<-sort(as.vector(as.matrix(perpetratorList51)))
      
      perpetratorList51[length(perpetratorList51) + 1] <- "All"
      
      selectInput(inputId = 'sel_perpetrator51', label = 'Choose perpetrator to explore',choices=perpetratorList51,selected = perpetratorList51[length(perpetratorList51)])
      
      
    })  
    
  output$barplot51 <- renderPlot({ 
    
    country51<-as.data.frame(matrix(c(input$sel_country51),1,1))
    perpetrator51<-as.data.frame(matrix(c(input$sel_perpetrator51),1,1))
    
    if (perpetrator51 =="All"){
      
      question51<-sqldf("SELECT dp.Year, dp.Country_name, SUM (dp.Number_of_Victim_Fatalities) as num_victims51 FROM dataPanelV5 dp, country51 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Victim_Fatalities <> -9999 AND dp.Number_of_Victim_Fatalities <> -99 AND dp.Number_of_Victim_Fatalities <> 0 GROUP BY dp.Year")
      
      
    } else {
      
      question51<-sqldf("SELECT dp.Year, dp.Country_name,  dp.Perpetrator_Group_Name, SUM (dp.Number_of_Victim_Fatalities) as num_victims51 FROM dataPanelV5 dp, country51 p, perpetrator51 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Number_of_Victim_Fatalities <> -9999 AND dp.Number_of_Victim_Fatalities <> -99 AND dp.Number_of_Victim_Fatalities <> 0 GROUP BY dp.Year")
      
      
    }

    if(length(unique(question51$Year))!=0){
      
      if (perpetrator51 =="All"){
        
        ggplot(question51,aes(x=factor(Year),y=num_victims51,fill=Country_name)) + geom_col() + geom_text(aes(label=num_victims51), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of victim fatalities by all perpetrators", x="Year") + scale_fill_manual(values=c("#FF0000"))
        
        
      } else {
        
        ggplot(question51,aes(x=factor(Year),y=num_victims51,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_victims51), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of victim fatalities by perpetrator:", x="Year") + scale_fill_manual(values=c("#FF0000"))
        
        
      }
          
       
            
          } else{
            showNotification("No matches")
          }

  },height = 500) 
  
  #5.2 Number of injured victims by a perpetrator
  
  output$use_perpetrator52 <- renderUI({
    
    use_country52 <-as.data.frame(matrix(c(input$sel_country52),1,1))
    
    perpetratorList52<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country52 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Victims_Injured <> -9999 AND dp.Number_of_Victims_Injured <> -99 AND dp.Number_of_Victims_Injured <> 0")
    
    perpetratorList52<-sort(as.vector(as.matrix(perpetratorList52)))
    
    perpetratorList52[length(perpetratorList52) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator52', label = 'Choose perpetrator to explore',choices=perpetratorList52,selected = perpetratorList52[length(perpetratorList52)])
    
    
  }) 
  
  output$barplot52 <- renderPlot({ 
    
    country52<-as.data.frame(matrix(c(input$sel_country52),1,1))
    perpetrator52<-as.data.frame(matrix(c(input$sel_perpetrator52),1,1))
    
    #INICIO introduzco los cambios que quiero con el if
    if (perpetrator52 =="All"){
      
      question52<-sqldf("SELECT dp.Year, dp.Country_name, SUM (dp.Number_of_Victims_Injured) as num_victims52 FROM dataPanelV5 dp, country52 p WHERE dp.Country_name = p.V1 AND dp.Number_of_Victims_Injured <> -9999 AND dp.Number_of_Victims_Injured <> -99 AND dp.Number_of_Victims_Injured <> 0 GROUP BY dp.Year")
      
      
    } else {
      
      question52<-sqldf("SELECT dp.Year, dp.Country_name, dp.Perpetrator_Group_Name, SUM (dp.Number_of_Victims_Injured) as num_victims52 FROM dataPanelV5 dp, country52 p, perpetrator52 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Number_of_Victims_Injured <> -9999 AND dp.Number_of_Victims_Injured <> -99 AND dp.Number_of_Victims_Injured <> 0 GROUP BY dp.Year")
      
      
    }
    
    
            
    if(length(unique(question52$Year))!=0){
      
      if (perpetrator52 =="All"){
        
        ggplot(question52,aes(x=factor(Year),y=num_victims52,fill=Country_name)) + geom_col() + geom_text(aes(label=num_victims52), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of injured victims by all perpetrators", x="Year") + scale_fill_manual(values=c("#DC143C"))
        
        
      } else {
        
        ggplot(question52,aes(x=factor(Year),y=num_victims52,fill=Perpetrator_Group_Name)) + geom_col() + geom_text(aes(label=num_victims52), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Number of injured victims by perpetrator:", x="Year") + scale_fill_manual(values=c("#DC143C"))
        
      }
    
            } else{
              showNotification("No matches")
            }
   
  },height = 500)
  
  
# Tab 6 - Property damage             
  
  #6.1- Number of atacks by type of Property damage
  
  output$use_perpetrator61 <- renderUI({
    
    use_country61 <-as.data.frame(matrix(c(input$sel_country61),1,1))
    
    perpetratorList61<- sqldf("SELECT DISTINCT dp.Perpetrator_Group_Name FROM dataPanelV5 dp, use_country61 p WHERE dp.Country_name = p.V1 AND dp.Extent_of_Property_Damage_text <> 'Not applicable'")
    
    perpetratorList61<-sort(as.vector(as.matrix(perpetratorList61)))
    
    perpetratorList61[length(perpetratorList61) + 1] <- "All"
    
    selectInput(inputId = 'sel_perpetrator61', label = 'Choose perpetrator to explore',choices=perpetratorList61,selected = perpetratorList61[length(perpetratorList61)])
    
    
  })  
  
  perpetrator61<- reactive({as.data.frame(matrix(c(input$sel_perpetrator61),1,1))})
  
  country61<- reactive({as.data.frame(matrix(c(input$sel_country61),1,1))})
  
  observeEvent(c(input$sel_country61,input$sel_perpetrator61),{ 
    
      question61<- reactive({
        
        country611<- country61()
        
        perpetrator611<- perpetrator61()
        
        if (perpetrator611 =="All"){
          
          sqldf("SELECT dp.Year, dp.Extent_of_Property_Damage_text, COUNT (*) as num_ataques61 FROM dataPanelV5 dp, country611 p WHERE dp.Country_name = p.V1 AND dp.Extent_of_Property_Damage_text <> 'Not applicable' GROUP BY dp.Year, dp.Extent_of_Property_Damage_text")
          
        } else {
          
          sqldf("SELECT dp.Year, dp.Extent_of_Property_Damage_text, COUNT (*) as num_ataques61 FROM dataPanelV5 dp, country611 p, perpetrator611 per WHERE dp.Country_name = p.V1 AND dp.Perpetrator_Group_Name = per.V1 AND dp.Extent_of_Property_Damage_text <> 'Not applicable' GROUP BY dp.Year, dp.Extent_of_Property_Damage_text")
 
        }
        
        })
      
      n_facets61<-function(){
        question611<- question61()
        aux_var<-500*length(unique(question611$Extent_of_Property_Damage_text))
        if(aux_var!=0){
                  return (500*length(unique(question611$Extent_of_Property_Damage_text)))
        } else {
                  return (500)}
        }

      output$barplot61 <- renderPlot({ 
        
        question611<- question61() 
        
        if(length(unique(question611$Extent_of_Property_Damage_text))!=0){

          ggplot(question611,aes(x=factor(Year),y=num_ataques61,fill=Extent_of_Property_Damage_text)) + geom_bar(stat="identity") + facet_rep_grid(Extent_of_Property_Damage_text ~ .,scales = "free", repeat.tick.labels=TRUE) + geom_text(aes(label=num_ataques61), vjust=-0.2, colour="black") + scale_x_discrete(limits=as.character(c(1970:1992,1994:2016)),breaks=as.character(c(1970:1992,1994:2016)),labels=as.character(c(1970:1992,1994:2016))) + theme(legend.position="top",axis.title.y=element_blank(),axis.text.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) + labs(fill="Extent of Property Damage:", x="Year")
          
        } else {
          showNotification("No matches")
        }
        
      },height = n_facets61)
      
   })
  
  
  # Tab 7 - Maps  
  
  #7.1 Attacks carried out by perpetrator
  
  country71<- reactive({as.data.frame(matrix(c(input$sel_country71),1,1))})
  
  year71<- reactive({as.data.frame(matrix(c(input$sel_year71),1,1))})
  
  observeEvent(c(input$sel_country71,input$sel_year71),{ 
    
  my_map71 <- reactive({
    
    country711<- country71()
    
    year711<- year71()

    sqldf("SELECT dp.Year, dp.Latitude, dp.Longitude, dp.Number_of_Victim_Fatalities, dp.City, dp.Perpetrator_Group_Name FROM dataPanelV5 dp, year711 y, country711 p WHERE dp.Year = y.V1 AND dp.Country_name = p.V1 AND dp.Latitude <> -9999 AND dp.Longitude <> -9999")})
  
  uso <- paste("<p>","Location: ", my_map71()$City,"</p>",
                            "<p>","Year: ", my_map71()$Year,"</p>",
                            "<p>","Latitude: ", my_map71()$Latitude,"</p>",
                            "<p>", "Longitude: ",my_map71()$Longitude,"</p>",
                            "<p>", "Perpetrator group: ",my_map71()$Perpetrator_Group_Name,"</p>",
                            "<p>","Number of victims: ", {ifelse(my_map71()$Number_of_Victim_Fatalities==-9999,"No data",my_map71()$Number_of_Victim_Fatalities)},"</p>")
  
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  
  pal <- colorBin("YlOrRd", domain = my_map71()$Number_of_Victim_Fatalities, bins = bins)
  
  output$map71 <- renderLeaflet({ 
    

    leaflet(height = "100%") %>% addTiles() %>% addCircleMarkers(lat=my_map71()$Latitude,lng = my_map71()$Longitude, label = lapply(uso, HTML))
    

    
  })
  })
  
  
}

shinyApp(server = server, ui = ui)
