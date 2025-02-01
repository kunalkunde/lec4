library(lubridate)
library(RQuantLib)
library(tidyverse)
library(tvm)
library(shiny)

ui <- fluidPage(
    titlePanel("SOM 660 Lec 4; Sat, 01-Feb-25; Hldg Period IRR for a bond"),
    # Bond mty; Buy date, yield; Sell date, yield; Coupon rate, frequency
    fluidRow(
      column(4, dateInput(inputId = "dateOfMaturity", label = h4("Bond maturity"), value = "2033-08-14", format = "yyyy-mm-dd")),
      column(4, sliderInput(inputId = "cpnRate", label = h4("Coupon (% p.a.)"), min=0, max=10, value=7.18, step = 0.01)),
      column(4, radioButtons(inputId = "cpnPmtFreq",label = h4("Coupon frequency"),choices = c(1, 2, 4),selected = 1))
    ),
    fluidRow(
      column(4, dateInput(inputId = "dateOfPurchase", label = h4("Buy date"), value = "2025-02-01", format = "yyyy-mm-dd")),
      column(4, sliderInput(inputId = "buyYld", label = h4("Buy Yield (% p.a.)"), min=5, max=15, value=7.1774, step = 0.01))
    ),
    fluidRow(
      column(4, dateInput(inputId = "dateOfSale", label = h4("Sell date"), value = "2028-03-27"), format = "yyyy-mm-dd"),
      column(4, sliderInput(inputId = "sellYld", label = h4("Sell Yield (% p.a.)"), min=5, max=15, value=5.85, step = 0.01))
    ),
    verbatimTextOutput(outputId = "HPIRR"),
    tableOutput(outputId = "scheduleOfCashFlows")
)

server <- function(input, output, session) {
    observe({
      buyDate <- updateDateInput(session, inputId = "dateOfPurchase", max = input$sellDate-1)
      sellDate <- updateDateInput(session, inputId = "dateOfSale", min = input$buyDate+1, max = input$bondMaturity)
      mtyDate <- updateDateInput(session, inputId = "dateOfMaturity", min = input$sellDate)
    })

    mtyDate <- reactive({base::as.Date(input$dateOfMaturity,format='%Y-%m-%d')})
    buyYield <- reactive({input$buyYld/100})
    sellYield <- reactive({input$sellYld/100})
    annual_coupon <- reactive({input$cpnRate/100})
    cpn_freq <- reactive({as.integer(input$cpnPmtFreq)})
    buyDate <- reactive({base::as.Date(input$dateOfPurchase,format='%Y-%m-%d')})
    sellDate <- reactive({base::as.Date(input$dateOfSale,format='%Y-%m-%d')})
    
    cpnPmtDates <- reactive({
      tempVariable1 <- mtyDate()
      tempVariable2 <- cpn_freq()
      cpnPmtDates <- as.data.frame.Date(tempVariable1 %m+% months(x = seq.int(from = 0, to = -1200, by = -12/tempVariable2)))
    })

    startIdx <- reactive({
      tmpVariable1 <- nrow(cpnPmtDates())
      tmpVariable2 <- sellDate()      
      tmpVariable3 <- cpnPmtDates()
      repeatedSellDatesDF <- as.data.frame.POSIXct(rep(tmpVariable2, tmpVariable1))
      startIdx <- min(which( tmpVariable3-repeatedSellDatesDF <=0))
    })
    
    endIdx <- reactive({
      tmpVariable1 <- nrow(cpnPmtDates())
      tmpVariable2 <- buyDate()
      tmpVariable3 <- cpnPmtDates()
      repeatedBuyDatesDF <- as.data.frame.POSIXct(rep(tmpVariable2, tmpVariable1))
      endIdx <- min(which( tmpVariable3-repeatedBuyDatesDF <=0))
    })

    CF_dates_chrono_order <- reactive({
      tmpVariable1 <- base::as.Date(buyDate(),format='%Y-%m-%d')
      tmpVariable2 <- base::as.Date(sellDate(),format='%Y-%m-%d')
      tmpVariable3 <- cpnPmtDates()
      tmpVariable4 <- startIdx()
      tmpVariable5 <- endIdx()
      CF_dates_chrono_order <- rev(c(tmpVariable2,tmpVariable3[tmpVariable4:(tmpVariable5-1),1],tmpVariable1))    
    })

    dirtyPriceAtPurchase <- reactive({
      tmpVariable1 <- base::as.Date(buyDate(),format='%Y-%m-%d')
      tmpVariable2 <- cpnPmtDates()
      tmpVariable3 <- endIdx()      
      tmpVariable4 <- endIdx()-1      
      lastPrePurchaseCpnDate <- tmpVariable2[tmpVariable3,1] 
      NUM_days_last_cpnDt_to_purchaseDt <- as.numeric(tmpVariable1 - lastPrePurchaseCpnDate, units='secs') # needed since difftime values cannot be divided
      DENOM_days_one_cpn_to_next <- as.numeric(tmpVariable2[tmpVariable4,1]-tmpVariable2[tmpVariable3,1], units='secs') # needed since difftime values cannot be divided
      
      setEvaluationDate(as.Date(tmpVariable1))
      tmp1ToCalcBondPx <- buyYield()
      tmp2ToCalcBondPx <- buyDate()
      tmp3ToCalcBondPx <- mtyDate()
      tmp4ToCalcBondPx <- cpn_freq()
      tmp5ToCalcBondPx <- annual_coupon()
      tmp6ToCalcBondPx <- endIdx()+1
      tmp7ToCalcBondPx <- tmpVariable2[tmp6ToCalcBondPx,1]
      dirtyPriceAtPurchase <- FixedRateBondPriceByYield(settlementDays=0,yield=tmp1ToCalcBondPx,faceAmount=100,effectiveDate=as.Date(tmp2ToCalcBondPx),maturityDate=as.Date(tmp3ToCalcBondPx),period=tmp4ToCalcBondPx,calendar="UnitedStates/GovernmentBond",rates=tmp5ToCalcBondPx,dayCounter = 1,businessDayConvention = 0,compound = 1,redemption = 100, issueDate=as.Date(tmp7ToCalcBondPx))+(NUM_days_last_cpnDt_to_purchaseDt/DENOM_days_one_cpn_to_next)*100*tmp5ToCalcBondPx/tmp4ToCalcBondPx
    })
    
    dirtyPriceAtSale <- reactive({
      tmpVariable1 <- base::as.Date(sellDate(),format='%Y-%m-%d')
      tmpVariable2 <- cpnPmtDates()
      tmpVariable3 <- startIdx()      
      tmpVariable4 <- startIdx()-1      
      lastPreSaleCpnDate <- tmpVariable2[tmpVariable3,1] 
      NUM_days_last_cpnDt_to_saleDt <- as.numeric(tmpVariable1 - lastPreSaleCpnDate, units='secs') # needed since difftime values cannot be divided
      DENOM_days_one_cpn_to_next <- as.numeric(tmpVariable2[tmpVariable4,1]-tmpVariable2[tmpVariable3,1], units='secs') # needed since difftime values cannot be divided
      
      setEvaluationDate(as.Date(tmpVariable1))
      tmp1ToCalcBondPx <- sellYield()
      tmp2ToCalcBondPx <- buyDate()
      tmp3ToCalcBondPx <- mtyDate()
      tmp4ToCalcBondPx <- cpn_freq()
      tmp5ToCalcBondPx <- annual_coupon()
      tmp6ToCalcBondPx <- endIdx()+1
      tmp7ToCalcBondPx <- tmpVariable2[tmp6ToCalcBondPx,1]
      dirtyPriceAtSale <- FixedRateBondPriceByYield(settlementDays=0,yield=tmp1ToCalcBondPx,faceAmount=100,effectiveDate=as.Date(tmp2ToCalcBondPx),maturityDate=as.Date(tmp3ToCalcBondPx),period=tmp4ToCalcBondPx,calendar="UnitedStates/GovernmentBond",rates=tmp5ToCalcBondPx,dayCounter = 1,businessDayConvention = 0,compound = 1,redemption = 100, issueDate=as.Date(tmp7ToCalcBondPx))+(NUM_days_last_cpnDt_to_saleDt/DENOM_days_one_cpn_to_next)*100*tmp5ToCalcBondPx/tmp4ToCalcBondPx
    })
    
    CF_amts <- reactive({
      tempContainer <- CF_dates_chrono_order()
      first_entry_in_array <- -dirtyPriceAtPurchase()
      last_entry_in_array <- dirtyPriceAtSale()
      CF_amts <- c(first_entry_in_array, rep(100*annual_coupon()/cpn_freq(),length(tempContainer)-2), last_entry_in_array) 
    })

    hldgPeriodIRR <- reactive({
      cashFlowAmts <- CF_amts()
      cashFlowDates <- CF_dates_chrono_order()
      couponRate <- annual_coupon()

      if(couponRate==0)
      {
        xirr(cf=c(cashFlowAmts[1],cashFlowAmts[length(cashFlowAmts)]),d=as.Date(c(cashFlowDates[1],cashFlowDates[length(cashFlowDates)])),comp_freq = 1, maxiter=100, tol=0.00000001)
        # (cashFlowAmts[length(cashFlowAmts)]/-cashFlowAmts[1])^(1/(as.numeric(cashFlowDates[length(cashFlowDates)]-cashFlowDates[1], units='secs')/(365.25*24*60*60)))-1
      } else {
        xirr(cf=cashFlowAmts,d=as.Date(cashFlowDates),comp_freq = 1, maxiter=100, tol=0.00000001)
      }      
    })
    output$HPIRR <- renderPrint(as.numeric(hldgPeriodIRR()))
    
    CF_table <- reactive({
      cashFlowAmts <- CF_amts()
      cashFlowDates <- CF_dates_chrono_order()
      CF_table <- data.frame("Date"=as.character.Date(cashFlowDates,format="%Y-%m-%d"), "Amt"=cashFlowAmts)

    })
    output$scheduleOfCashFlows <- renderTable(CF_table())
    
}
shinyApp(ui = ui, server = server)
