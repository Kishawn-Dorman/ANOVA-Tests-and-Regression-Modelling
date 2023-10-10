#Shiny App for Age vs Predictors
library(shiny)
df <- read.csv("BMC Dataset full.csv", header = T)

#change Predictors into a factor with presentable titles.
df$job <- factor(df$job, labels = c("Admin", "BlueCollar", "Entrep", "Maid", "Mgt", "Retired", "Self-Emp", "Servies", "Student", "Technic", "Unemp", "Unknown"))
df$marital <- factor(df$marital, labels = c("Divorced", "Married", "Single"))
df$education <- factor(df$education, labels = c("Primary", "Secondary", "Tertiary", "Unknown"))
df$default <- factor(df$default, labels = c("No", "Yes"))
df$balance <- factor(df$balance)
df$housing <- factor(df$housing, labels = c("No", "Yes"))
df$contact <- factor(df$contact, labels = c("Cellular", "Telephone", "Unknown"))
df$month <- factor(df$month, labels = c("April", "August", "Dec", "feb", "January", "July", "June", "March", "May", "Nov", "Oct", "Sept"))
df$poutcome <- factor(df$poutcome, labels = c("Failure", "Other", "Success", "Unknown"))

#Displaying outputs
ui <- fluidPage(
  titlePanel("Average Age of Clients per Predictor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "variable:",
                  c("Type of Job" = "job",
                    "Marital Status" = "marital",
                    "Type of Education" = "education",
                    "Credit in Default" = "default",
                    "Account Balance" = "balance",
                    "Possess a Housing Loan" = "housing",
                    "Contact Method" = "contact",
                    "Last Contact Month" = "month",
                    "Outcome of Prev Campaign" = "poutcome")),
      checkboxInput("outliners", "Show outliners", TRUE)
    ), 
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("ageplot")
    )
  )
)

#Define server logic to plot various variables against age
server <- function(input, output) {
  formulaText <- reactive({
    paste("age ~", input$variable)
  }) 
  output$caption <- renderText({
    formulaText()
  }) 
  output$ageplot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = df,
            outline = input$outliners,
            col = "#007bc2", pch = 19)
  })
  
}


#Run shiny app object
shinyApp(ui, server)
