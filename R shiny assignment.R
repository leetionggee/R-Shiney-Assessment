<<<<<<< HEAD
library(shiny)
library(readxl)
library(ggplot2)
=======
#library(shiny)
#library(readxl)
#library(ggplot2)
>>>>>>> bc3144d3688171aea953d1661ba49540b9d73d0f

ui <- fluidPage(
  titlePanel("Please input your claims data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("claims_file", "Import claims file", accept = c(".xlsx")),
      numericInput("tail_factor","Tail Factor",value=1.1),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Paid Claims Plot",
                 fluidRow(
                   column(12,
                          plotOutput("plot",width = "auto")
                   )
                 )
        ),
        tabPanel("Cumulative Paid Claims Table",
                 fluidRow(
                   column(12,
                          dataTableOutput("proj_table")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output,session) {
<<<<<<< HEAD
  
=======
>>>>>>> bc3144d3688171aea953d1661ba49540b9d73d0f
  #create reactive "loss_year()" to represent number and date of loss year
  loss_year <- reactive({
    req(input$claims_file)
    claims_table = read_xlsx(input$claims_file$datapath)
    colnames(claims_table) <- c("Loss Year","Development Year","Amount of Claims Paid ($)")
    loss_year = unique(claims_table$'Loss Year')
<<<<<<< HEAD
  })
  
  #create reactive "data()" to represent projection table
  data <- reactive({
    
    #validation of file input
    req(input$claims_file)
    claims_table = read_xlsx(input$claims_file$datapath)
    colnames(claims_table) <- c("Loss Year","Development Year","Amount of Claims Paid ($)")
    
    #create empty tables for calculation
    #paid_claims_table is incremental claims, cumulative_claims_table is cumulative claims
    paid_claims_table = matrix(0,nrow = length(loss_year()), ncol = length(loss_year())+1 )
    cumulative_claims_table = matrix(0,nrow = length(loss_year()), ncol = length(loss_year())+1 )
    
    
    #extract and format amount of paid from list into table, according to lost year
    col_ind = 1
    for (i in 1:length(loss_year())){
      for(j in 1:nrow(claims_table)){
        if(claims_table$'Loss Year'[j] == loss_year()[i]){
          paid_claims_table[i,col_ind] = claims_table$'Amount of Claims Paid ($)'[j]
          col_ind = col_ind+1
        }
        else{
          col_ind=1
        }
      }
    }
    
    #calculate cumulative claims in table
    for (i in 1:length(loss_year())){
      for (j in 1:length(loss_year())){
        if(paid_claims_table[i,j]!=0){
          if(j==1){
            cumulative_claims_table[i,j] = paid_claims_table[i,j]
          }
          else{
            cumulative_claims_table[i,j] = sum(paid_claims_table[i,1:j])
          }
        }
      }
    }
    
    #calculate development factor in table
    dev_factor_table = matrix(1,nrow = 1, ncol = length(loss_year())+1 )
    for (i in 1:length(loss_year())){ #excluding last development year
      dev1 = 0
      dev2 = 0
      if(i!=1){
        dev1 = sum(cumulative_claims_table[1:(length(loss_year())+1-i),i-1])
        dev2 = sum(cumulative_claims_table[1:(length(loss_year())+1-i),i])
        dev_factor_table[1,i] = dev2/dev1
      }
    }
    
    #factor for last development year = tail_factor input
    dev_factor_table[1,length(loss_year())+1] = input$tail_factor
    
    #calculate projected claims in table
    projected_claims_table = cumulative_claims_table
    for (i in 1:length(loss_year())){
      for (j in 1:length(loss_year())+1){
        if(projected_claims_table[i,j]==0){
          projected_claims_table[i,j] = projected_claims_table[i,j-1]*dev_factor_table[j]
        }
      }
    }
    
    #round projected claims table to 0 dp
    round_df <- function(x,digits){
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(projected_claims_table,0)
    #end of reactive
  })
  
  
  output$table_title <- renderText({
    "Cumulative Claims ($) Table"
  })
  
=======
  })
  #create reactive "data()" to represent projection table
  data <- reactive({
    #validation of file input
    req(input$claims_file)
    
    claims_table = read_xlsx(input$claims_file$datapath)
    colnames(claims_table) <- c("Loss Year","Development Year","Amount of Claims Paid ($)")
    
    #create empty tables for calculation
    #paid_claims_table is incremental claims, cumulative_claims_table is cumulative claims
    paid_claims_table = matrix(0,nrow = length(loss_year()), ncol = length(loss_year())+1 )
    cumulative_claims_table = matrix(0,nrow = length(loss_year()), ncol = length(loss_year())+1 )

    
    #extract and format amount of paid from list into table, according to lost year
    col_ind = 1
    for (i in 1:length(loss_year())){
      for(j in 1:nrow(claims_table)){
        if(claims_table$'Loss Year'[j] == loss_year()[i]){
          paid_claims_table[i,col_ind] = claims_table$'Amount of Claims Paid ($)'[j]
          col_ind = col_ind+1
        }
        else{
          col_ind=1
        }
      }
    }
    
    #calculate cumulative claims in table
    for (i in 1:length(loss_year())){
      for (j in 1:length(loss_year())){
        if(paid_claims_table[i,j]!=0){
          if(j==1){
            cumulative_claims_table[i,j] = paid_claims_table[i,j]
          }
          else{
            cumulative_claims_table[i,j] = sum(paid_claims_table[i,1:j])
          }
        }
      }
    }
    
    #calculate development factor in table
    dev_factor_table = matrix(1,nrow = 1, ncol = length(loss_year())+1 )
    for (i in 1:length(loss_year())){ #excluding last development year
      dev1 = 0
      dev2 = 0
      if(i!=1){
        dev1 = sum(cumulative_claims_table[1:(length(loss_year())+1-i),i-1])
        dev2 = sum(cumulative_claims_table[1:(length(loss_year())+1-i),i])
        dev_factor_table[1,i] = dev2/dev1
      }
    }
    #factor for last development year = tail_factor input
    dev_factor_table[1,length(loss_year())+1] = input$tail_factor
    
    #calculate projected claims in table
    projected_claims_table = cumulative_claims_table
    for (i in 1:length(loss_year())){
      for (j in 1:length(loss_year())+1){
       if(projected_claims_table[i,j]==0){
          projected_claims_table[i,j] = projected_claims_table[i,j-1]*dev_factor_table[j]
        }
      }
    }
    
    #round projected claims table to 0 dp
    round_df <- function(x,digits){
      numeric_columns <- sapply(x, mode) == 'numeric'
      x[numeric_columns] <-  round(x[numeric_columns], digits)
      x
    }
    round_df(projected_claims_table,0)
    #end of reactive
  })
  
  
  output$table_title <- renderText({
    "Cumulative Claims ($) Table"
  })
  
>>>>>>> bc3144d3688171aea953d1661ba49540b9d73d0f
  output$plot_title <- renderText({
    "Cumulative Claims ($) Plot"
  })
  
  output$proj_table <- renderDataTable({
<<<<<<< HEAD
    proj_table = as.data.frame(data())
    
    #rename columns of projection table
    dev_year_list = c()
    for (i in 1:(length(loss_year())+1)){
      dev_year_list = append(dev_year_list,paste("DY",i,sep=""))
    }
    proj_table = cbind(loss_year(),proj_table)
    colnames(proj_table) = c("Loss Year",dev_year_list)
    proj_table
    
    #end of output
=======
    
  proj_table = as.data.frame(data())
  #rename columns of projection table
  dev_year_list = c()
  for (i in 1:(length(loss_year())+1)){
    dev_year_list = append(dev_year_list,paste("DY",i,sep=""))
  }
  proj_table = cbind(loss_year(),proj_table)
  colnames(proj_table) = c("Loss Year",dev_year_list)
  proj_table
  
  #end of output
>>>>>>> bc3144d3688171aea953d1661ba49540b9d73d0f
  })
  
  output$plot <- renderPlot({
    PCT = as.data.frame(data())
    
    #create column for x-axis(development year for ggplot)
    development_year=c()
    for (i in (1:(length(loss_year())+1))){
      development_year = append(development_year,i)
    }
    PCT = rbind(development_year,PCT)
    transposedPCT = as.data.frame(t(PCT))
    colnames(transposedPCT) = c("Development Year",loss_year())
    
    #create line plot based on loss year
    p = ggplot()+labs(x = "Development Year", y = "Cumulative Claims ($)")
    for (i in 1:length(loss_year())){
      aes = aes_string(x = transposedPCT[,1],y = transposedPCT[,(i+1)],color=factor(loss_year()[i]))
      p = p+geom_smooth(aes,method="loess",se=FALSE,linewidth = 0.8)
      p = p+geom_text(transposedPCT,mapping=aes,label=paste(transposedPCT[,(i+1)]),size=3.5,vjust=-0.8,show.legend = FALSE)
    }
    p
    
<<<<<<< HEAD
    #end of output
=======
  #end of output
>>>>>>> bc3144d3688171aea953d1661ba49540b9d73d0f
  })
  #end of server
}

shinyApp(ui, server)