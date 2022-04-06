#Main-Shiny_App_Code

WHOCVRisk_Shiny=function()
{

suppressMessages(suppressWarnings(require(shinythemes)))
suppressMessages(suppressWarnings(require(shiny)))
suppressMessages(suppressWarnings(require(WHOCVRisk)))
suppressMessages(suppressWarnings(require(data.table)))
suppressMessages(suppressWarnings(require(gridExtra)))
suppressMessages(suppressWarnings(require(plotly)))

ui <- navbarPage(
  "WHO CVD Risk Assessment Tool",collapsible = TRUE,
		 inverse = TRUE, theme = shinytheme("spacelab"),
  tabPanel("Total cholesterol is available",
		fluidPage( 
		fluidRow(
		column(7,
			sidebarLayout(
       		sidebarPanel(
				selectInput(inputId = "Country1", label = "Country", 
				choices =sort(c("Bolivia","Ecuador","Peru","Australia","New Zealand",
						"Antigua and Barbuda","Bahamas","Belize","Bermuda",
						"Barbados","Cuba","Dominica","Dominican Republic",
						"Grenada","Guyana","Haiti","Jamaica","Saint Lucia",
						"Puerto Rico","Suriname","Trinidad and Tobago","Saint Vincent and the Grenadines",
						paste("Virgin Islands, U.S."),"Armenia","Azerbaijan","Georgia",
						"Kazakhstan","Kyrgyzstan","Mongolia","Tajikistan","Turkmenistan",
						"Uzbekistan","Albania","Bulgaria","Bosnia and Herzegovina",
						"Czech Republic","Croatia","Hungary","Macedonia (TFYR)","Montenegro",
						"Poland","Romania","Serbia","Slovakia","Slovenia",
						"Angola","Central African Republic","DR Congo","Congo","Gabon",
						"Equatorial Guinea","China","North Korea","Taiwan","Burundi","Comoros",
						"Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mozambique",
						"Malawi","Rwanda","Somalia","Tanzania","Uganda","Zambia",
						"Brunei Darussalam","Japan","South Korea","Singapore","Afghanistan",
						"United Arab Emirates","Bahrain","Iran","Iraq","Jordan",
						"Kuwait","Lebanon","Oman","Occupied Palestinian Territory","Qatar",
						"Saudi Arabia","Syrian Arab Republic","Turkey","Yemen","American Samoa",
						"Fiji","Guam","Micronesia (Federated States of)","Kiribati",
						"Marshall Islands","Northern Mariana Islands","Papua New Guinea",
						"Solomon Islands","Tonga","Vanuatu","Samoa","Bangladesh","Bhutan",
						"India","Nepal","Pakistan","Argentina","Chile","Uruguay","Botswana",
						"Lesotho","Namibia","Swaziland","South Africa","Zimbabwe","Cyprus",
						"Israel","Colombia","Costa Rica","Guatemala","Honduras","Mexico","Nicaragua",
						"Panama","El Salvador","Venezuela","Belarus","Estonia","Lithuania","Latvia",
						"Moldova","Russian Federation","Ukraine","Canada","Greenland","United States of America",
						"Indonesia","Cambodia","Lao PDR","Sri Lanka","Maldives","Myanmar","Malaysia",
						"Philippines","Thailand",paste("Timor-Leste"),"Viet Nam","Brazil","Paraguay",
						"Benin","Burkina Faso",paste("Cote d'Ivoire"),"Cameroon","Cabo Verde","Ghana",
						"Guinea","Gambia","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria",
						"Senegal","Sierra Leone","Sao Tome and Principe","Chad","Togo"))),

				
				selectInput(inputId = "Gender1", label = "Gender", 
				choices =c("Male","Female")),
				
				selectInput(inputId = "DM1", label = "Diabetes", 
				choices =c("Yes","No")),

				selectInput(inputId = "Smoke1", label = "Smoke", 
				choices =c("Smoker","Non-smoker")),

				numericInput(inputId = "Age1", label = "Age [40 to 74]",
				value =40)),

				sidebarLayout(
       			sidebarPanel(
								
					
				numericInput(inputId = "SBP1", label = "Systolic blood pressure in mmHg",
				value =120),

				numericInput(inputId = "Cholesterol", label = "Cholesterol in mmol/l",
				value =4),
				
				actionButton("do", "Calculate")),
mainPanel(tags$head(
	  tags$style(type="text/css",
	".test_type {color: red;
                           font-size:12 px; 
                           font-style: italic;}",
      ".shiny-output-error {visibility: hidden;}",
      ".shiny-output-error:before {visibility: hidden;}")),
	tags$div(class="test_type"))
))),

fluidPage( 
column(4,
mainPanel(tags$style(type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"),
	tags$div(style="margin-bottom:50px;"),
	plotlyOutput("plot1", height =400, width =400))))
))),

		  tabPanel("Total cholesterol is not available",
		fluidPage( 
		fluidRow(
		column(7,
			sidebarLayout(
       		sidebarPanel(
				selectInput(inputId = "Country2", label = "Country", 
				choices =sort(c("Bolivia","Ecuador","Peru","Australia","New Zealand",
						"Antigua and Barbuda","Bahamas","Belize","Bermuda",
						"Barbados","Cuba","Dominica","Dominican Republic",
						"Grenada","Guyana","Haiti","Jamaica","Saint Lucia",
						"Puerto Rico","Suriname","Trinidad and Tobago","Saint Vincent and the Grenadines",
						paste("Virgin Islands, U.S."),"Armenia","Azerbaijan","Georgia",
						"Kazakhstan","Kyrgyzstan","Mongolia","Tajikistan","Turkmenistan",
						"Uzbekistan","Albania","Bulgaria","Bosnia and Herzegovina",
						"Czech Republic","Croatia","Hungary","Macedonia (TFYR)","Montenegro",
						"Poland","Romania","Serbia","Slovakia","Slovenia",
						"Angola","Central African Republic","DR Congo","Congo","Gabon",
						"Equatorial Guinea","China","North Korea","Taiwan","Burundi","Comoros",
						"Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mozambique",
						"Malawi","Rwanda","Somalia","Tanzania","Uganda","Zambia",
						"Brunei Darussalam","Japan","South Korea","Singapore","Afghanistan",
						"United Arab Emirates","Bahrain","Iran","Iraq","Jordan",
						"Kuwait","Lebanon","Oman","Occupied Palestinian Territory","Qatar",
						"Saudi Arabia","Syrian Arab Republic","Turkey","Yemen","American Samoa",
						"Fiji","Guam","Micronesia (Federated States of)","Kiribati",
						"Marshall Islands","Northern Mariana Islands","Papua New Guinea",
						"Solomon Islands","Tonga","Vanuatu","Samoa","Bangladesh","Bhutan",
						"India","Nepal","Pakistan","Argentina","Chile","Uruguay","Botswana",
						"Lesotho","Namibia","Swaziland","South Africa","Zimbabwe","Cyprus",
						"Israel","Colombia","Costa Rica","Guatemala","Honduras","Mexico","Nicaragua",
						"Panama","El Salvador","Venezuela","Belarus","Estonia","Lithuania","Latvia",
						"Moldova","Russian Federation","Ukraine","Canada","Greenland","United States of America",
						"Indonesia","Cambodia","Lao PDR","Sri Lanka","Maldives","Myanmar","Malaysia",
						"Philippines","Thailand",paste("Timor-Leste"),"Viet Nam","Brazil","Paraguay",
						"Benin","Burkina Faso",paste("Cote d'Ivoire"),"Cameroon","Cabo Verde","Ghana",
						"Guinea","Gambia","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria",
						"Senegal","Sierra Leone","Sao Tome and Principe","Chad","Togo"))),

			
				selectInput(inputId = "Gender2", label = "Gender", 
				choices =c("Male","Female")),

		
				selectInput(inputId = "Smoke2", label = "Smoke", 
				choices =c("Smoker","Non-smoker")),

		
      			numericInput(inputId = "Age2", label = "Age [40 to 74]",
				value =40),
    			
				numericInput(inputId = "SBP2", label = "Systolic blood pressure in mmHg",
				value =120)),

			
				sidebarLayout(
       			sidebarPanel(
			
				numericInput(inputId = "Weight2", label = "Weight in kg",
				value =65),

				numericInput(inputId = "Height2", label = "Height in cm",
				value =170),
			
				actionButton("do", "Calculate")),
mainPanel(tags$head(
	  tags$style(type="text/css",
	".test_type {color: red;
                           font-size:12 px; 
                           font-style: italic;}",
      ".shiny-output-error {visibility: hidden;}",
      ".shiny-output-error:before {visibility: hidden;}")),
	tags$div(class="test_type"))
))),

	fluidPage( 
	column(4,
	mainPanel(tags$style(type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"),
	tags$div(style="margin-bottom:50px;"),
	plotlyOutput("plot2", height =400, width =400))))
))
))


server <- function(input, output)
{
options(shiny.maxRequestSize=30*1024^2)


#Total cholesterol is available
data1 <- reactive({
my_data=data.frame(Country=input$Country1,age=input$Age1,gender=input$Gender1,
			smoke=input$Smoke1,sbp=input$SBP1,dm=input$DM1,chol=input$Cholesterol)

my_data$region=NA
my_data$region[my_data$Country %in% c("Bolivia","Ecuador","Peru")]<-"Andean Latin America"

my_data$region[my_data$Country %in% c("Australia","New Zealand")]<-"Australasia"

my_data$region[my_data$Country %in% c("Antigua and Barbuda","Bahamas","Belize","Bermuda",
						"Barbados","Cuba","Dominica","Dominican Republic",
						"Grenada","Guyana","Haiti","Jamaica","Saint Lucia",
						"Puerto Rico","Suriname","Trinidad and Tobago","Saint Vincent and the Grenadines",
						"Virgin Islands, U.S.")]<-"Caribbean"

my_data$region[my_data$Country %in% c("Armenia","Azerbaijan","Georgia",
						"Kazakhstan","Kyrgyzstan","Mongolia","Tajikistan","Turkmenistan",
						"Uzbekistan")]<-"Central Asia"

my_data$region[my_data$Country %in% c("Albania","Bulgaria","Bosnia and Herzegovina",
						"Czech Republic","Croatia","Hungary","Macedonia (TFYR)","Montenegro",
						"Poland","Romania","Serbia","Slovakia","Slovenia")]<-"Central Europe"

my_data$region[my_data$Country %in% c("Angola","Central African Republic","DR Congo","Congo","Gabon",
						"Equatorial Guinea")]<-"Central Sub-Saharan Africa"

my_data$region[my_data$Country %in% c("China","North Korea","Taiwan")]<-"East Asia"

my_data$region[my_data$Country %in% c("Burundi","Comoros",
						"Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mozambique",
						"Malawi","Rwanda","Somalia","Tanzania","Uganda","Zambia")]<-"Eastern Sub-Saharan Africa"

my_data$region[my_data$Country %in% c("Brunei Darussalam","Japan","South Korea","Singapore")]<-"High-Income Asia Pacific"

my_data$region[my_data$Country %in% c("Afghanistan",
						"United Arab Emirates","Bahrain","Iran","Iraq","Jordan",
						"Kuwait","Lebanon","Oman","Occupied Palestinian Territory","Qatar",
						"Saudi Arabia","Syrian Arab Republic","Turkey","Yemen")]<-"North Afrika and Middle East"

my_data$region[my_data$Country %in% c("American Samoa",
						"Fiji","Guam","Micronesia (Federated States of)","Kiribati",
						"Marshall Islands","Northern Mariana Islands","Papua New Guinea",
						"Solomon Islands","Tonga","Vanuatu","Samoa")]<-"Oceania"

my_data$region[my_data$Country %in% c("Bangladesh","Bhutan","India","Nepal","Pakistan")]<-"South Asia"

my_data$region[my_data$Country %in% c("Argentina","Chile","Uruguay")]<-"Southern Latin America"
my_data$region[my_data$Country %in% c("Botswana","Lesotho","Namibia","Swaziland","South Africa","Zimbabwe")]<-"Southern Sub-Saharan Africa"

my_data$region[my_data$Country %in% c("Cyprus","Israel")]<-"Western Europe"

my_data$region[my_data$Country %in% c("Colombia","Costa Rica","Guatemala","Honduras","Mexico","Nicaragua",
						"Panama","El Salvador","Venezuela")]<-"Central Latin America"

my_data$region[my_data$Country %in% c("Belarus","Estonia","Lithuania","Latvia",
						"Moldova","Russian Federation","Ukraine")]<-"Eastern Europe"

my_data$region[my_data$Country %in% c("Canada","Greenland","United States of America")]<-"High-Income North America"

my_data$region[my_data$Country %in% c("Indonesia","Cambodia","Lao PDR","Sri Lanka","Maldives","Myanmar","Malaysia",
						"Philippines","Thailand","Timor-Leste","Viet Nam")]<-"Southeast Asia"

my_data$region[my_data$Country %in% c("Brazil","Paraguay")]<-"Tropical Latin America"

my_data$region[my_data$Country %in% c("Benin","Burkina Faso","Cote d'Ivoire","Cameroon","Cabo Verde","Ghana",
						"Guinea","Gambia","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria",
						"Senegal","Sierra Leone","Sao Tome and Principe","Chad","Togo")]<-"Western Sub-Saharan Africa"

my_data$gender=ifelse(my_data$gender=="Male","M","F")
my_data$age=as.character(my_data$age)
my_data$sbp=as.character(my_data$sbp)
my_data$chol=as.character(my_data$chol)
my_data=data.frame(my_data)
attach(my_data)

Ref=CVDRiskwithlab
df=my_data

#Age
df$age2=NA
df$age2[df$age>=40 & df$age<=44.9]<-"40-44"
df$age2[df$age>=45 & df$age<=49.9]<-"45-49"
df$age2[df$age>=50 & df$age<=54.9]<-"50-54"
df$age2[df$age>=55 & df$age<=59.9]<-"55-59"
df$age2[df$age>=60 & df$age<=64.9]<-"60-64"
df$age2[df$age>=65 & df$age<=69.9]<-"65-69"
df$age2[df$age>=70 & df$age<=74]<-"70-74"

#SBP
df$sbp2=NA
df$sbp2[df$sbp<120]<-"<120"
df$sbp2[df$sbp>=120 & df$sbp<=139.9]<-"120-139"
df$sbp2[df$sbp>=140 & df$sbp<=159.9]<-"140-159"
df$sbp2[df$sbp>=160 & df$sbp<=179.9]<-"160-179"
df$sbp2[df$sbp>=180]<-">=180"


df$chol2=NA
df$chol2[df$chol<4]<-"<4"
df$chol2[df$chol>=4 & df$chol<=4.9]<-"4-4.9"
df$chol2[df$chol>=5 & df$chol<=5.9]<-"5-5.9"
df$chol2[df$chol>=6 & df$chol<=6.9]<-"6-6.9"
df$chol2[df$chol>=7]<-">=7"

Var1=c("region","age2","gender","smoke","sbp2","dm","chol2")
df=df[,Var1]


Region<-Ref$Region[match(df$region,Ref$Region)]
Age<-Ref$Age[match(df$age2,Ref$Age)]
Gender<-Ref$Gender[match(df$gender,Ref$Gender)]
Smoke<-Ref$Smoke[match(df$smoke,Ref$Smoke)]
SBP<-Ref$SBP[match(df$sbp2,Ref$SBP)]
DM<-Ref$DM[match(df$dm,Ref$DM)]
CHOL<-Ref$CHOL[match(df$chol2,Ref$CHOL)]

#Create a new variable called luv (look up value)
  df$fin<-paste(df$region,df$age2,df$gender,df$smoke,df$sbp2,df$dm,df$chol2,sep="")

#Create a new variable called refv (reference value)
  Ref$fin<-paste(Ref$Region,Ref$Age,Ref$Gender,Ref$Smoke,Ref$SBP,Ref$DM,Ref$CHOL,sep="")

df$Risk<-paste0(Ref$Risk[match(df$fin,Ref$fin)],"%")

    return(df[,-8])

})

observeEvent(input$do,{
	output$plot1<- renderPlotly({
df=setDT(data1())
Names=c("Region",
	"Age","Gender","Smoke","SBP","DM","Chol","Risk")
df=data.frame(Measures=Names,Results=t(df))
  summary_statistics <- tableGrob(
    df,
    theme = ttheme_default(
      base_size =14,
      base_colour = "black",
      parse = T
      ),
    rows = NULL
    )

value=as.numeric(gsub("%","",df[8,2]))
fig <- plot_ly(
domain = list(x = c(0,100), y = c(0,100)),
value = value,
number = list(suffix = "%"),
title = list(text="WHO CVD Risk"),
type = "indicator",
mode = "gauge+number",
gauge= list(
axis = list(range= list(0,100),tick0=0,dtick=10,tickwidth=1, tickcolor="white"),
bar = list(color= "blue",tickwidth=2),
steps = list(
      list(range = c(0,4.9), color = "green"),
	list(range = c(5,9.9), color = "yellow"),
	list(range = c(10,19.9), color = "orange"),
	list(range = c(20,29.9), color = "red"),
	list(range = c(30,100), color = "purple"))))
fig <- fig%>%
layout(
margin =list(l=50,r=50),
font = list(color="Black", family ="Arial"))

fig
	
	})

})



#Total cholesterol is not available
data2 <- reactive({
my_data2=data.frame(Country=input$Country2,age=input$Age2,gender=input$Gender2,
			smoke=input$Smoke2,sbp=input$SBP2,Weight=input$Weight2,Height=input$Height2)

my_data2$region=NA
my_data2$region[my_data2$Country %in% c("Bolivia","Ecuador","Peru")]<-"Andean Latin America"

my_data2$region[my_data2$Country %in% c("Australia","New Zealand")]<-"Australasia"

my_data2$region[my_data2$Country %in% c("Antigua and Barbuda","Bahamas","Belize","Bermuda",
						"Barbados","Cuba","Dominica","Dominican Republic",
						"Grenada","Guyana","Haiti","Jamaica","Saint Lucia",
						"Puerto Rico","Suriname","Trinidad and Tobago","Saint Vincent and the Grenadines",
						"Virgin Islands, U.S.")]<-"Caribbean"

my_data2$region[my_data2$Country %in% c("Armenia","Azerbaijan","Georgia",
						"Kazakhstan","Kyrgyzstan","Mongolia","Tajikistan","Turkmenistan",
						"Uzbekistan")]<-"Central Asia"

my_data2$region[my_data2$Country %in% c("Albania","Bulgaria","Bosnia and Herzegovina",
						"Czech Republic","Croatia","Hungary","Macedonia (TFYR)","Montenegro",
						"Poland","Romania","Serbia","Slovakia","Slovenia")]<-"Central Europe"

my_data2$region[my_data2$Country %in% c("Angola","Central African Republic","DR Congo","Congo","Gabon",
						"Equatorial Guinea")]<-"Central Sub-Saharan Africa"

my_data2$region[my_data2$Country %in% c("China","North Korea","Taiwan")]<-"East Asia"

my_data2$region[my_data2$Country %in% c("Burundi","Comoros",
						"Djibouti","Eritrea","Ethiopia","Kenya","Madagascar","Mozambique",
						"Malawi","Rwanda","Somalia","Tanzania","Uganda","Zambia")]<-"Eastern Sub-Saharan Africa"

my_data2$region[my_data2$Country %in% c("Brunei Darussalam","Japan","South Korea","Singapore")]<-" High-Income Asia Pacific"

my_data2$region[my_data2$Country %in% c("Afghanistan",
						"United Arab Emirates","Bahrain","Iran","Iraq","Jordan",
						"Kuwait","Lebanon","Oman","Occupied Palestinian Territory","Qatar",
						"Saudi Arabia","Syrian Arab Republic","Turkey","Yemen")]<-"North Afrika and Middle East"

my_data2$region[my_data2$Country %in% c("American Samoa",
						"Fiji","Guam","Micronesia (Federated States of)","Kiribati",
						"Marshall Islands","Northern Mariana Islands","Papua New Guinea",
						"Solomon Islands","Tonga","Vanuatu","Samoa")]<-"Oceania"

my_data2$region[my_data2$Country %in% c("Bangladesh","Bhutan","India","Nepal","Pakistan")]<-"South Asia"
my_data2$region[my_data2$Country %in% c("Argentina","Chile","Uruguay")]<-"Southern Latin America"

my_data2$region[my_data2$Country %in% c("Botswana","Lesotho","Namibia","Swaziland","South Africa","Zimbabwe")]<-"Southern Sub-Saharan Africa"

my_data2$region[my_data2$Country %in% c("Cyprus","Israel")]<-"Western Europe"

my_data2$region[my_data2$Country %in% c("Colombia","Costa Rica","Guatemala","Honduras","Mexico","Nicaragua",
						"Panama","El Salvador","Venezuela")]<-"Central Latin America"

my_data2$region[my_data2$Country %in% c("Belarus","Estonia","Lithuania","Latvia",
						"Moldova","Russian Federation","Ukraine")]<-"Eastern Europe"

my_data2$region[my_data2$Country %in% c("Canada","Greenland","United States of America")]<-"High-Income North America"

my_data2$region[my_data2$Country %in% c("Indonesia","Cambodia","Lao PDR","Sri Lanka","Maldives","Myanmar","Malaysia",
						"Philippines","Thailand","Timor-Leste","Viet Nam")]<-"Southeast Asia"

my_data2$region[my_data2$Country %in% c("Brazil","Paraguay")]<-"Tropical Latin America"

my_data2$region[my_data2$Country %in% c("Benin","Burkina Faso","Cote d'Ivoire","Cameroon","Cabo Verde","Ghana",
						"Guinea","Gambia","Guinea Bissau","Liberia","Mali","Mauritania","Niger","Nigeria",
						"Senegal","Sierra Leone","Sao Tome and Principe","Chad","Togo")]<-"Western Sub-Saharan Africa"

my_data2$gender=ifelse(my_data2$gender=="Male","M","F")
my_data2$age=as.character(my_data2$age)
my_data2$sbp=as.character(my_data2$sbp)
my_data2$bmi=(my_data2$Weight)/(my_data2$Height/100)^2
my_data2$bmi=as.character(my_data2$bmi)
my_data2=data.frame(my_data2)
attach(my_data2)

Ref2=CVDRisk
df2=my_data2

#Age
df2$age2=NA
df2$age2[df2$age>=40 & df2$age<=44.9]<-"40-44"
df2$age2[df2$age>=45 & df2$age<=49.9]<-"45-49"
df2$age2[df2$age>=50 & df2$age<=54.9]<-"50-54"
df2$age2[df2$age>=55 & df2$age<=59.9]<-"55-59"
df2$age2[df2$age>=60 & df2$age<=64.9]<-"60-64"
df2$age2[df2$age>=65 & df2$age<=69.9]<-"65-69"
df2$age2[df2$age>=70 & df2$age<=74.9]<-"70-74"

#BMI
df2$bmi2=NA
df2$bmi2[df2$bmi<20]<-"<20"
df2$bmi2[df2$bmi>=20 & df2$bmi<24.9]<-"20-24"
df2$bmi2[df2$bmi>=25 & df2$bmi<29.9]<-"25-29"
df2$bmi2[df2$bmi>=30 & df2$bmi<34.9]<-"30-35"
df2$bmi2[df2$bmi>=35]<-">=35"

#SBP

df2$sbp2=NA
df2$sbp2[df2$sbp<120]<-"<120"
df2$sbp2[df2$sbp>=120 & df2$sbp<139.9]<-"120-139"
df2$sbp2[df2$sbp>=140 & df2$sbp<159.9]<-"140-159"
df2$sbp2[df2$sbp>=160 & df2$sbp<179.9]<-"160-179"
df2$sbp2[df2$sbp>=180]<-">=180"

Var1=c("region","age2","gender","smoke","bmi2","sbp2")
df2=df2[,Var1]

Region<-Ref2$Region[match(df2$region,Ref2$Region)]
Age<-Ref2$Age[match(df2$age2,Ref2$Age)]
Gender<-Ref2$Gender[match(df2$gender,Ref2$Gender)]
Smoke<-Ref2$Smoke[match(df2$smoke,Ref2$Smoke)]
BMI<-Ref2$BMI[match(df2$bmi2,Ref2$BMI)]
SBP<-Ref2$SBP[match(df2$sbp2,Ref2$SBP)]

#Create a new variable called luv (look up value)
  df2$fin<-paste(df2$region,df2$age2,df2$gender,df2$smoke,df2$bmi2,df2$sbp2,sep="")

#Create a new variable called refv (reference value)
  Ref2$fin<-paste(Ref2$Region,Ref2$Age,Ref2$Gender,Ref2$Smoke,Ref2$BMI,Ref2$SBP,sep="")

df2$Risk<-paste0(Ref2$Risk[match(df2$fin,Ref2$fin)],"%")

    return(df2[,-7])

})


observeEvent(input$do,{
	output$plot2<- renderPlotly({
df=setDT(data2())
Names=c("Region",
	"Age","Gender","Smoke","BMI","SBP","Risk")
df=data.frame(Measures=Names,Results=t(df))
  summary_statistics <- tableGrob(
    df,
    theme = ttheme_default(
      base_size =14,
      base_colour = "black",
      parse = T
      ),
    rows = NULL
    )

value=as.numeric(gsub("%","",df[7,2]))
fig <- plot_ly(
domain = list(x = c(0,100), y = c(0,100)),
value = value,
number = list(suffix = "%"),
title = list(text="WHO CVD Risk"),
type = "indicator",
mode = "gauge+number",
gauge= list(
axis = list(range= list(0,100),tick0=0,dtick=10, tickwidth=1, tickcolor="white"),
bar = list(color= "blue",tickwidth=2),
steps = list(
      list(range = c(0,4.9), color = "green"),
	list(range = c(5,9.9), color = "yellow"),
	list(range = c(10,19.9), color = "orange"),
	list(range = c(20,29.9), color = "red"),
	list(range = c(30,100), color = "purple"))))
fig <- fig%>%
layout(
margin =list(l=50,r=50),
font = list(color="Black", family ="Arial"))

fig
	})
})

}

shinyApp(ui = ui, server = server)
}

