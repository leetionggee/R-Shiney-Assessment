#library(shiny)
#library(readr)
#library(ggplot2)
ui <- fluidPage(
  titlePanel("Claims Data"),
  fileInput("file", "Choose CSV file", accept = c(".csv")),
  numericInput("tailFactor","Tail Factor",value=0),
  dataTableOutput("data_table"),
  plotOutput("plot", width = "400px")
)

server <- function(input, output,session) {
  lossYear <- reactive({
    req(input$file)
    df = read_csv(input$file$datapath)
    colnames(df) <- c("Loss Year","Development Year","Amount of Claims Paid ($)")
    lossYear = unique(df$'Loss Year')
  })
  data <- reactive({
    req(input$file)
    df = read_csv(input$file$datapath)
    colnames(df) <- c("Loss Year","Development Year","Amount of Claims Paid ($)")
    #lossYear = unique(df$'Loss Year')
    paidClaimsTable = matrix(0,nrow = length(lossYear()), ncol = length(lossYear())+1 )
    cumulativeClaimsTable = matrix(0,nrow = length(lossYear()), ncol = length(lossYear())+1 )
    colInd = 1
#format input into development table pattern
    for (i in 1:length(lossYear())){
      for(j in 1:nrow(df)){
        if(df$'Loss Year'[j] == lossYear()[i]){
          paidClaimsTable[i,colInd] = df$'Amount of Claims Paid ($)'[j]
          colInd = colInd+1
        }
        else{
          colInd=1
        }
      }
    }
#calculate cumulative table as matrix
    for (i in 1:length(lossYear())){
      for (j in 1:length(lossYear())){
        if(paidClaimsTable[i,j]!=0){
          if(j==1){
            cumulativeClaimsTable[i,j] = paidClaimsTable[i,j]
          }
          else{
            cumulativeClaimsTable[i,j] = sum(paidClaimsTable[i,1:j])
          }
        }
      }
    }
devFactorTable = matrix(1,nrow = 1, ncol = length(lossYear())+1 )
tailFactor = 1.1
for (i in 1:length(lossYear())){#every column in pf table except tail year
  dev1 = 0
  dev2 = 0
  if(i!=1){
    dev1 = sum(cumulativeClaimsTable[1:(length(lossYear())+1-i),i-1])
    dev2 = sum(cumulativeClaimsTable[1:(length(lossYear())+1-i),i])
    devFactorTable[1,i] = dev2/dev1
  }
}
devFactorTable[1,4] = input$tailFactor
projectedClaimsTable = cumulativeClaimsTable
for (i in 1:length(lossYear())){
  for (j in 1:length(lossYear())+1){
    if(projectedClaimsTable[i,j]==0){
      projectedClaimsTable[i,j] = projectedClaimsTable[i,j-1]*devFactorTable[j]
    }
  }
}
projectedClaimsTable
#end of reactive
  })
  
  output$data_table <- renderDataTable({
  as.data.frame(data())
#end of output
  })
  output$plot <- renderPlot({
    transposedPCT = as.data.frame(data())
    developmentYear=c()
    for (i in (1:(length(lossYear())+1))){
      developmentYear = append(developmentYear,i)
    }
    transposedPCT = rbind(transposedPCT,developmentYear)
    transposedPCT = as.data.frame(t(transposedPCT))
    colnames(transposedPCT) = c(lossYear(),"Development Year")
    print(transposedPCT)
    p = ggplot()+labs(x = "Development Year", y = "Cumulative Claims ($)")
    lossYear()
    for (i in 1:length(lossYear())){
      aes = aes_string(x = transposedPCT[,length(lossYear())+1],y = transposedPCT[,i],color=factor(lossYear()[i]))
      p = p+geom_line(transposedPCT, mapping = aes)
      p = p+geom_text(transposedPCT,mapping=aes,label=paste(transposedPCT[,i]),size=2.5,vjust=-0.8)
    }
    print(p)
#end of output plot
  })
#end of server
}

shinyApp(ui, server)