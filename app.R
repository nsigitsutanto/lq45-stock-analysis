# global ------------------------------------------------------------------

options(scipen = 123)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(scales)
library(readxl)
library(shinythemes)
library(readr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(glue)
library(lubridate)
library(tidytext)
library(ggplot2)
library(data.table)
library(openxlsx)
library(reshape2)
library(DT)

#call assets
lq45=read_xlsx("lq45.xlsx")
trdata=read_xlsx("transac_data.xlsx")
lq45prof=read_xlsx("lq45profile.xlsx", 
                   col_types = c("text", "text", "date", "text", "text"))

#lowering header
judul=colnames(trdata)
judul=tolower(judul)
judul=gsub(' ','',judul)
colnames(trdata)=judul

#separation for dividend and stock split
div=trdata %>% filter(grepl("Dividend", open))
div$open=gsub("Dividend","",div$open)
div$open=as.numeric(div$open)
div=div %>% select(c("date","open","code"))
colnames(div)[2]="div"

ss=trdata %>% filter(grepl("Stock", open))
ss$open=gsub("Stock Split","",ss$open)
ss=ss %>% select(c("date","open","code"))
colnames(ss)[2]="ss"

#clean trdata and ombine ss & div
data1=trdata %>% filter(!grepl("Dividend|Stock", open))
data2=merge(data1,div,all.x = T)
data3=merge(data2,ss,all.x = T)

data3$date=mdy(data3$date)
for(i in 3:8){
    data3[,i]=gsub("\\.00","",data3[,i])
    data3[,i]=gsub("\\,","",data3[,i])
    data3[,i]=as.numeric(data3[,i])
}

#calc day avg & day avg total transact
data3$dayavg=(data3$high+data3$low)/2
data3$dayavgtottransact=data3$dayavg*data3$volume
data3$quarter=quarter(data3$date)
data3$growth=((data3$close-data3$open)/data3$open)*100
data3$year=year(data3$date)
data3$month=month(data3$date)
data3=data3 %>% filter(!is.na(volume))

data4=merge(data3,lq45prof,all.x = T)

#call assets
datalq45 <- data4 %>% 
    mutate(code = as.factor(code),
           sector = as.factor(sector))

# ui ----------------------------------------------------------------------

ui <- dashboardPage(skin = "black",
    dashboardHeader(title="LQ45 IDX Stock"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(tabName = "menu1",
                     text = "Sector Overview"),
            menuItem(tabName = "menu2",
                     text = "Stock Overview"),
            menuItem(tabName = "menu3",
                     text = "Comparison")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "menu1",
                    fluidRow(
                        column(width = 12,
                               h1("LQ45 IDX Stock"),
                               h4("It is always interesting to discuss about investment. 
                        We can do invest in several products, such as stocks, mutual fund, and precious metals, including gold and silver. 
                        Talking about investment in stocks and mutual funds, investing in these two can multiply your money way better than bank's interest rates. 
                        Blue chips are named for stocks which are poven to be reliable in long-term investment. Several stocks which are classified as blue-chip stocks which are included in an index called LQ45. 
                        This index contains 45 companies which fulfill stock exchange requirement and has good liquidity which means has better reliability than other companies in stock exchange."),
                               h4("In this analysis, we analyse these 45 companies' stocks to know which company has highest growth and return of investment. 
                                  We take the data from scraping Yahoo! Finance website which has complete historical data of these stocks. 
                                  The data that we use are 6 years-range data of open and close stock price, transaction volume, dividend and stock split info from 3rd February 2014 to 31st January 2020."))
                    ),
                    fluidRow(
                        column(width = 3,
                               selectInput(inputId = "sector",
                                           label = "Choose Company Sector:",
                                           choices = levels(datalq45$sector))),
                        column(width = 3,
                               dateRangeInput(inputId = "date1", 
                                              label = "Choose Date Range:", 
                                              min = min(datalq45$date), 
                                              max = max(datalq45$date),
                                              format = "dd-mm-yyyy",
                                              weekstart = 0,
                                              autoclose = T)),
                        column(width = 2,
                               valueBoxOutput(outputId = "capmarkt",
                                              width = NULL)),
                        column(width = 4,
                               valueBoxOutput(outputId = "totcap",
                                              width = NULL))
                    ),
                    fluidRow(width=12,
                             dataTableOutput(outputId = 'tablesect')),
                    fluidRow(width=12,
                             plotlyOutput(outputId = "candlesect.plot"))
            ),
            
            tabItem(tabName = "menu2",
                    fluidRow(
                        column(width = 4,
                               dateRangeInput(inputId = "date2", 
                                           label = "Choose Date Range:", 
                                           min = min(datalq45$date), 
                                           max = max(datalq45$date),
                                           format = "dd-mm-yyyy",
                                           weekstart = 0,
                                           autoclose = T)),
                        column(width = 3,
                               selectInput(inputId = "stock",
                                           label = "Choose Stock:",
                                           choices = levels(datalq45$code),
                                           selected = "ADRO",
                                           multiple = T))
                    ),
                    fluidRow(width=12,
                             plotlyOutput(outputId = "price.plot")
                    ),
                    fluidRow(width=12,
                             plotlyOutput(outputId = "volume.plot")
                    ),
            ),
            
            tabItem(tabName = "menu3",
                    fluidRow(
                        column(width = 3,
                               selectInput(inputId = "stock1",
                                           label = "Choose First Stock:",
                                           choices = levels(datalq45$code),
                                           selected = "ADRO")),
                        column(width = 6,
                               dateRangeInput(inputId = "date3", 
                                              label = "Choose Date Range:", 
                                              min = min(datalq45$date), 
                                              max = max(datalq45$date),
                                              format = "dd-mm-yyyy",
                                              weekstart = 0,
                                              autoclose = T)),
                        column(width = 3,
                               selectInput(inputId = "stock2",
                                           label = "Choose Second Stock:",
                                           choices = levels(datalq45$code),
                                           selected = "ADRO"))
                    ),
                    fluidRow(
                        box(plotlyOutput(outputId = "stock1.plot")),
                        box(plotlyOutput(outputId = "stock2.plot"))
                        ),
                    fluidRow(
                        column(width=6,
                               dataTableOutput(outputId = "stock1.info")),
                        column(width=6,
                               dataTableOutput(outputId = "stock2.info"))
                    )
            )
        )
    )
)
# server ------------------------------------------------------------------


server <- function(input, output, session) {
    
    output$capmarkt=renderValueBox({
        capmarktpersect=datalq45 %>% 
            group_by(date,sector) %>% 
            summarise(sumperday=sum(dayavgtottransact))
        capmarktperdate=datalq45 %>% 
            group_by(date) %>% 
            summarise(totperday=sum(dayavgtottransact))
        capmarkt=merge(capmarktpersect,capmarktperdate,all.x = T)
        capmarkt=capmarkt %>% 
            filter(sector %in% input$sector) %>% 
            filter(date>=input$date1[1],date<=input$date1[2]) %>% 
            mutate(volpercent=sumperday/totperday*100)
        
        valueBox(value=comma(mean(capmarkt$volpercent)),
                 subtitle = "% Market Capitalization (Average)")
    })
    
    output$totcap=renderValueBox({
        capmarktpersect=datalq45 %>% 
            group_by(date,sector) %>% 
            summarise(sumperday=sum(dayavgtottransact))
        capmarktperdate=datalq45 %>% 
            group_by(date) %>% 
            summarise(totperday=sum(dayavgtottransact))
        capmarkt=merge(capmarktpersect,capmarktperdate,all.x = T)
        capmarkt=capmarkt %>% 
            filter(sector %in% input$sector) %>% 
            filter(date>=input$date1[1],date<=input$date1[2]) %>% 
            mutate(volpercent=sumperday/totperday*100)
        
        valueBox(value=comma(mean(capmarkt$totperday)),
                 subtitle = "Total Market Capitalization in Rupiah (Average)")
    })
    
    output$tablesect=renderDataTable({
        desc.data4=datalq45 %>% filter(sector %in% input$sector) %>% arrange(desc(growth))
        desc.data4[duplicated(desc.data4[ , c("code")]),"code"]<-NA
        desc.data4=desc.data4 %>% filter(!is.na(code)) %>% select(c(code,name,growth,date))
        colnames(desc.data4)[3]="highest growth"
        colnames(desc.data4)[4]="date of highest"
        
        asc.data4=datalq45 %>% filter(sector %in% input$sector) %>% arrange(growth)
        asc.data4[duplicated(asc.data4[ , c("code")]),"code"]<-NA
        asc.data4=asc.data4 %>% filter(!is.na(code)) %>% select(c(code,name,growth,date))
        colnames(asc.data4)[3]="lowest growth"
        colnames(asc.data4)[4]="date of lowest"
        
        tabtop=merge(desc.data4,asc.data4,all.x = T)
        colnames(tabtop)=c("Code","Name","Highest Growth (%)","Date of Highest Growth","Lowest Growth (%)","Date of Lowest Growth")
        datatable(tabtop) %>% formatRound(columns=c("Highest Growth (%)","Lowest Growth (%)"),digits = 1) 
    })
    
    output$candlesect.plot=renderPlotly({
        data.sect=datalq45 %>% 
            group_by(date,sector) %>% 
            summarise(open=mean(open),high=mean(high),low=mean(low),close=mean(`close*`),dayavgtottransact=sum(dayavgtottransact))
        
        candlesect.plot=data.sect %>%
            filter(sector %in% input$sector) %>% 
            filter(date>=input$date1[1],date<=input$date1[2]) %>% 
            plot_ly(x=~date,type = "candlestick",
                                          open=~open,close=~close,
                                          high=~high,low=~low) %>% 
            layout(title="Sector Candlestick")
        
        candlesect.plot
    })
    
    output$price.plot=renderPlotly({
        price.data=datalq45 %>% 
            filter(date>=input$date2[1],date<=input$date2[2]) %>% 
            filter(code %in% c(input$stock)) %>% 
            mutate(text = glue(
            "Date = {date}
             Stock = {open}
             Price = {`close*`}"
             ))
        
        price.plot <- ggplot(data = price.data, aes(x = date, y = `close*`)) +
            geom_line(aes(color = code)) +
            geom_point(aes(color = code, text = text), size = 0.1) +
            theme(legend.position = "none") +
            labs(title="Stock Price",x="Time",y="Close Price in Rupiah")
        
        ggplotly(price.plot, tooltip = "text")
        
    })
    
    output$volume.plot=renderPlotly({
        volume.data=datalq45 %>% 
            filter(date>=input$date2[1],date<=input$date2[2]) %>% 
            filter(code %in% c(input$stock)) %>% 
            mutate(text = glue(
                "Date = {date}
                 Stock = {open}
                 Volume = {volume}"
            ))
        
        volume.plot <- ggplot(data = volume.data, aes(x = date, y = volume)) +
            geom_line(aes(color = code)) +
            geom_point(aes(color = code, text = text), size = 0.1) +
            theme(legend.position = "none") +
            labs(title="Volume Transaction",x="Time",y="Volume in Shares")
        
        ggplotly(volume.plot, tooltip = "text")
        
    })
    
    output$stock1.plot=renderPlotly({
        data.stk1=datalq45
        data.stk1=data.stk1 %>% 
            filter(code %in% input$stock1) %>% 
            filter(date>=input$date3[1],date<=input$date3[2])
        
        stock1.plot=data.stk1 %>% plot_ly(x=~date,type = "candlestick",
                                          open=~open,close=~`close*`,
                                          high=~high,low=~low) %>% 
            layout(title="Stock 1 Candlestick")
        
        stock1.plot
    })
    
    output$stock2.plot=renderPlotly({
        data.stk2=datalq45
        data.stk2=data.stk2 %>% 
            filter(code %in% input$stock2) %>% 
            filter(date>=input$date3[1],date<=input$date3[2])
        
        stock2.plot=data.stk2 %>% plot_ly(x=~date,type = "candlestick",
                                          open=~open,close=~`close*`,
                                          high=~high,low=~low) %>% 
            layout(title="Stock 2 Candlestick")
        
        stock2.plot
    })
    
    output$stock1.info=renderDataTable({ 
        datastk1=datalq45 %>% filter(date>=input$date3[1],date<=input$date3[2])
        ss1=datastk1 %>% group_by(code) %>% count(ss) %>% filter(!is.na(ss)) %>% summarise(timesss=sum(n))
        
        datastk1=datalq45 %>% filter(date>=input$date3[1],date<=input$date3[2])
        div1=datastk1 %>% group_by(code) %>% count(div) %>% filter(!is.na(div)) %>% summarise(timesdiv=sum(n))
        
        datastk1$div[is.na(datastk1$div)] <- 0
        maxdiv1=datastk1 %>% group_by(code) %>% summarise(maxdiv=max(div))
        
        info.stk1=merge(lq45prof,ss1,all.x = T)
        info.stk1=merge(info.stk1,div1,all.x = T)
        info.stk1=merge(info.stk1,maxdiv1,all.x=T)
        info.stk1$timesss[is.na(info.stk1$timesss)] <- 0
        info.stk1$timesdiv[is.na(info.stk1$timesdiv)] <- 0
        colnames(info.stk1)=c("Code","Name","IPO Date","Sector","Sub-Sector","Times of Stock Splitting","Times of Sharing Dividend","Highest Dividend in Rupiah")
        
        info.stk1=info.stk1 %>% filter(Code %in% input$stock1)
        stk1=t(info.stk1)
        colnames(stk1)=c("Stock 1 Information")
        stk1
    })
    
    output$stock2.info=renderDataTable({ 
        datastk2=datalq45 %>% filter(date>=input$date3[1],date<=input$date3[2])
        ss2=datastk2 %>% group_by(code) %>% count(ss) %>% filter(!is.na(ss)) %>% summarise(timesss=sum(n))
        
        datastk2=datalq45 %>% filter(date>=input$date3[1],date<=input$date3[2])
        div2=datastk2 %>% group_by(code) %>% count(div) %>% filter(!is.na(div)) %>% summarise(timesdiv=sum(n))
        
        datastk2$div[is.na(datastk2$div)] <- 0
        maxdiv2=datastk2 %>% group_by(code) %>% summarise(maxdiv=max(div))
        
        info.stk2=merge(lq45prof,ss2,all.x = T)
        info.stk2=merge(info.stk2,div2,all.x = T)
        info.stk2=merge(info.stk2,maxdiv2,all.x=T)
        info.stk2$timesss[is.na(info.stk2$timesss)] <- 0
        info.stk2$timesdiv[is.na(info.stk2$timesdiv)] <- 0
        colnames(info.stk2)=c("Code","Name","IPO Date","Sector","Sub-Sector","Times of Stock Splitting","Times of Sharing Dividend","Highest Dividend in Rupiah")
        
        info.stk2=info.stk2 %>% filter(Code %in% input$stock2)
        stk2=t(info.stk2)
        colnames(stk2)=c("Stock 2 Information")
        stk2
    })
    
}

shinyApp(ui, server)