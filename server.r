library(shiny)
library(shinydashboard)


shinyServer(
function(input, output){
  
  alfanor2 <- reactive({
    alfa <- input$alfanor2
  })
  chidf <- reactive({
    chidf <- input$chidf2
  })
  
 
  #############################################################################################################################
  output$norplottwo <- renderPlot({
    a <- c(seq(-3.5,3.5,0.01))
    b <- dnorm(a, mean = 0, sd = 1)
    alfa <- round(qnorm(p = (1-(alfanor2()/2)), 0,1),2)
    aa <- c(-alfa, a[-alfa<a & a< alfa], alfa)
    ab <- a[-alfa<a & a< alfa]
    bb <- c(0 , dnorm(ab), 0)
    
    plot(a,b, type = "l", bty="n", xlab = "x", ylab = "Probability Density", las=1)
    zo2 <- as.numeric(input$zo2)
    
    polygon(c(a[a<=-alfa],-alfa), c(b[a<=-alfa],0), col = "yellow")
    
    polygon(c(a[a>= alfa],alfa), c(b[a>= alfa],0), col = "yellow")
    
       polygon(aa, bb, col = "green")
       
   polygon(c(zo2, zo2), c(0, dnorm(zo2)) , col = "black",lwd=4  )
    
    legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
  }) 
  
  
output$nortest2 <- renderText({
  alfa <- round(qnorm(p = (1-(alfanor2()/2)), 0,1),2)
  input$actionnormal2
  isolate(if(
  abs(as.numeric(input$zo2)) >= alfa){
  print("Reject")}
  
  else{
    print("Accept")
  })

})

output$pvaluenortest2 <- renderText({
  input$actionnormal2
  isolate(
    paste("P-Value is", round(2*(1-pnorm(abs(as.numeric(input$zo2)))), 3), sep = " ")
  )
  
})
output$cvaluenortest2 <- renderText({
  
    paste("Critical Value is",  round(qnorm(p = (1-(alfanor2()/2)), 0,1),2), sep = " ")
  
  
  
})
 
#############################################################################################################################
output$tplot2 <- renderPlot({
  a <- c(seq(-4.5,4.5,0.01))
  b <- dt(a,df = input$tdf2)
  alfa <- round(qt(p =  (1-(input$alfat2 /2)), df = input$tdf2),2)
  aa <- c(-alfa, a[-alfa<a & a< alfa], alfa)
  ab <- a[-alfa<a & a< alfa]
  bb <- c(0 , dt(ab, df = input$tdf2), 0)
  plot(a,b, type = "l", xlim = c(min(a)*1.1, max(a)*1.1),  bty="n", xlab = "x", ylab = "Probability Density", las=1)
  to2 <- as.numeric(input$to2)
  
  polygon(c(a[a<=-alfa],-alfa), c(b[a<=-alfa],0), col = "yellow")
  
  polygon(c(a[a>= alfa],alfa), c(b[a>= alfa],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(to2, to2), c(0, dt(to2, df = input$tdf2)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$ttest2 <- renderText({
  
  alfa <- round(qt(p =  (1-(input$alfat2 /2)), df = input$tdf2),2)
  
  input$actiont2
  isolate(if(
   abs(as.numeric(input$to2)) >= alfa){
    print("Reject")}
    
    else{
      print("Accept")
    })
  })
output$pvaluettest2 <- renderText({
  input$actiont2
  isolate(
    paste("P-Value is", round(2*(1-pt(abs(as.numeric(input$to2)),df = input$tdf2)), 3), sep = " ")
  )
  
})
output$cvaluettest2 <- renderText({
 
  
    paste("Critical Value is",  round(qt(p = (1-(input$alfat2/2)),df = input$tdf2),2), sep = " ")
  
  
  
})

#############################################################################################################################
output$chiplot2 <- renderPlot({
  if (chidf()<=10)
    {a <- c(seq(0, 3*chidf(), 0.01))}
  else if(chidf()>10 &  chidf() <=35)
    {a <- c(seq(chidf()/5 , 2.5*chidf(), 0.01))}
  else (a <- c(seq(chidf()*0.4 , 1.7*chidf(), 0.01)))
  
  b <- dchisq(a,df = chidf())
  alfa1 <- round(qchisq(p =  (input$alfachi2)/2, df = chidf()),2)
  alfa2 <- round(qchisq(p =  1-(input$alfachi2/2), df = chidf()),2)
  aa <- c(alfa1, a[alfa1<a & a< alfa2], alfa2)
  ab <- a[alfa1<a & a< alfa2]
  bb <- c(0 , dchisq(ab, df = chidf()), 0)
  plot(a,b, type = "l",xlim = c(max(min(a),0), max(a)+5) , bty="n", xlab = "x", ylab = "Probability Density", las=1)
  
  chio2 <- as.numeric(input$chio2)
  
  polygon(c(a[a<=alfa1],alfa1), c(b[a<=alfa1],0), col = "yellow")
  
  polygon(c(a[a>= alfa2],alfa2), c(b[a>= alfa2],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(chio2, chio2), c(0, dchisq(chio2, df = chidf())) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$chitest2 <- renderText({
  
  alfa1 <- round(qchisq(p =  (input$alfachi2)/2, df = chidf()),2)
  alfa2 <- round(qchisq(p =  1-(input$alfachi2/2), df = chidf()),2)
  input$actionchi2
  (
    if(abs(as.numeric(input$chio2)) <= alfa1 )
      {
    print("Reject")}
    
    
    else if(
      abs(as.numeric(input$chio2)) >= alfa2
    ){
      print("Reject")
    }else 
      print("accept")
  )
  
})

output$pvaluechitest2 <- renderText({
  alfa1 <- round(qchisq(p =  (input$alfachi2)/2, df = chidf()),2)
  alfa2 <- round(qchisq(p =  1-(input$alfachi2/2), df = chidf()),2)
  input$actionchi2
  isolate(
    paste("P-Value is", min(2*round(1-pchisq(as.numeric(input$chio2) ,df = chidf()), 3), 2*round(pchisq(as.numeric(input$chio2) ,df = chidf()), 3)) , sep = " ")
  )
  
})
output$cvaluechitest2 <- renderText({
  
    paste(c("Upper Critical Value is:", "Lower Critical Value is"),  c(round(qchisq(p = (1-(input$alfachi2/2)),df = chidf()),2), round(qchisq(p = (input$alfachi2/2),df = chidf()),2)), sep = " ")
  
  
})


#############################################################################################################################
output$norplot1 <- renderPlot({
  a <- c(seq(-3.5,3.5,0.01))
  b <- dnorm(a, mean = 0, sd = 1)
  alfa <- round(qnorm(p =1-input$alfanor1 , 0,1),2)
  aa <- c(min(a), a[a< alfa], alfa)
  ab <- a[a< alfa]
  bb <- c(0 , dnorm(ab), 0)
  plot(a,b, type = "l" , bty="n", xlab = "x", ylab = "Probability Density", las=1)
  zo1 <- as.numeric(input$zo1)
  
  polygon(c(a[a>= alfa],alfa), c(b[a>= alfa],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(zo1, zo1), c(0, dnorm(zo1)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$nortest1 <- renderText({
  alfa <- round(qnorm(p =1-input$alfanor1 , 0,1),2)
  input$actionnormal1
  isolate(if(
    (as.numeric(input$zo1)) >= alfa){
    print("Reject")}
    
    else{
      print("Accept")
    })
  
})

output$pvaluenortest1 <- renderText({
  input$actionnormal1
  isolate(
    paste("P-Value is", round((1-pnorm((as.numeric(input$zo1)))), 3), sep = " ")
  )
  
})
output$cvaluenortest1 <- renderText({
  
  paste("Critical Value is",  round(qnorm(p = (1-input$alfanor1), 0,1),2), sep = " ")
  
  
  
})

#########################################################################################################


output$tplot1 <- renderPlot({
  a <- c(seq(-3.5,3.5,0.01))
  b <- dt(a, df = input$tdf1)
  alfa <- round(qt(p =1-input$alfat1 , df = input$tdf1),2)
  aa <- c(min(a), a[a< alfa], alfa)
  ab <- a[a< alfa]
  bb <- c(0 , dt(ab, df = input$tdf1), 0)
  plot(a,b, type = "l" , bty="n", xlab = "x", ylab = "Probability Density", las=1)
  to1 <- as.numeric(input$to1)
  
  polygon(c(a[a>= alfa],alfa), c(b[a>= alfa],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(to1, to1), c(0, dt(to1, df = input$tdf1)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "T Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$ttest1 <- renderText({
  alfa <- round(qt(p =1-input$alfat1 , df = input$tdf1),2)
  input$actiont1
  isolate(if(
    (as.numeric(input$to1)) >= alfa){
    print("Reject")}
    
    else{
      print("Accept")
    })
  
})

output$pvaluettest1 <- renderText({
  input$actiont1
  isolate(
    paste("P-Value is", round((1-pt((as.numeric(input$to1)),df = input$tdf1)), 3), sep = " ")
  )
  
})

output$cvaluettest1 <- renderText({
  
  
  paste("Critical Value is",  round(qt(p = (1-(input$alfat1)),df = input$tdf1),2), sep = " ")
  
  
  
})
#############################################################################################################################
output$chiplot1 <- renderPlot({
  chidf1 <- input$chidf1
  if (chidf1 <=10)
  {a <- c(seq(0, 3*chidf1, 0.01))}
  else if(chidf1>10 &  chidf1 <=35)
  {a <- c(seq(chidf1/5 , 2.5*chidf1, 0.01))}
  else (a <- c(seq(chidf1*0.4 , 1.7*chidf1, 0.01)))
  
  b <- dchisq(a,df = chidf1)
  alfa1 <- round(qchisq(p =  1-input$alfachi1, df = chidf1),2)
   aa <- c(min(a), a[ a < alfa1], alfa1)
  ab <- a[a< alfa1]
  bb <- c(0 , dchisq(ab, df = chidf1), 0)
  plot(a,b, type = "l",xlim = c(max(min(a),0), max(a)+5) , bty="n", xlab = "x", ylab = "Probability Density", las=1)
  
  chio1 <- as.numeric(input$chio1)
  
  polygon(c(a[a>= alfa1], alfa1), c(b[a>= alfa1],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(chio1, chio1), c(0, dchisq(chio1, df = chidf1)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$chitest1 <- renderText({
  chidf1 <- input$chidf1  
  alfa1 <- round(qchisq(p =  1-(input$alfachi1), df = chidf1),2)

  input$actionchi1
          isolate(if(
            (as.numeric(input$chio1)) >= alfa1){
            print("Reject")}
            
            else{
              print("Accept")
            })
  
})
output$pvaluechitest1 <- renderText({
  alfa1 <- round(qchisq(p =  1-input$alfachi1, df = input$chidf1),2)
  input$actionchi1
  isolate(
    paste("P-Value is", round(1-pchisq(as.numeric(input$chio1) ,df = input$chidf1), 3) , sep = " ")
  )
  
})
output$cvaluechitest1 <- renderText({
  
  
  paste("Critical Value is",  round(qchisq(p = (1-(input$alfachi1)),df = input$chidf1),2), sep = " ")
  
  
  
})


#########################################################################################################


output$norplot0 <- renderPlot({
  a <- c(seq(-3.5,3.5,0.01))
  b <- dnorm(a, mean = 0, sd = 1)
  alfa <- round(qnorm(p =input$alfanor0 , 0,1),2)
  aa <- c(alfa, a[a> alfa], max(a))
  ab <- a[a> alfa]
  bb <- c(0 , dnorm(ab), 0)
  plot(a,b, type = "l", bty="n", xlab = "x", ylab = "Probability Density", las=1)
  zo0 <- (as.numeric(input$zo0))
  
  polygon(c(a[a<= alfa],alfa), c(b[a<= alfa],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(zo0, zo0), c(0, dnorm(zo0)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$nortest0 <- renderText({
  alfa <- round(qnorm(p =input$alfanor0 , 0,1),2)
  input$actionnormal0
  isolate(if(
    (as.numeric(input$zo0)) <= alfa){
    print("Reject")}
    
    else{
      print("Accept")
    })
  
})

output$pvaluenortest0 <- renderText({
  input$actionnormal0
  isolate(
    paste("P-Value is", round((pnorm((as.numeric(input$zo0)))), 3), sep = " ")
  )
  
})
output$cvaluenortest0 <- renderText({
  
  paste("Critical Value is",  round(qnorm(p = (input$alfanor0), 0,1),2), sep = " ")
  
  
  
})
#########################################################################################################


output$tplot0 <- renderPlot({
  a <- c(seq(-3.5,3.5,0.01))
  b <- dt(a, df = input$tdf0)
  alfa <- round(qt(p =input$alfat0 , df = input$tdf0),2)
  aa <- c(alfa, a[a> alfa], max(a))
  ab <- a[a> alfa]
  bb <- c(0 , dt(ab, df = input$tdf0), 0)
  plot(a,b, type = "l", bty="n", xlab = "x", ylab = "Probability Density", las=1)
  to0 <- (as.numeric(input$to0))
  
  polygon(c(a[a<= alfa],alfa), c(b[a<= alfa],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(to0, to0), c(0, dt(to0, df = input$tdf0)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "T Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$ttest0 <- renderText({
  alfa <- round(qt(p =input$alfat0 , df = input$tdf0),2)
  input$actiont0
  isolate(if(
    (as.numeric(input$to0)) <= alfa){
    print("Reject")}
    
    else{
      print("Accept")
    })
  
})

output$pvaluettest0 <- renderText({
  input$actiont0
  isolate(
    paste("P-Value is", round((pt((as.numeric(input$to0)),df = input$tdf0)), 3), sep = " ")
  )
  
})

output$cvaluettest0 <- renderText({
  
  
  paste("Critical Value is",  round(qt(p = ((input$alfat0)),df = input$tdf0),2), sep = " ")
  
  
  
})
#############################################################################################################################
output$chiplot0 <- renderPlot({
  chidf0 <- input$chidf0
  if (chidf0 <=10)
  {a <- c(seq(0, 3*chidf0, 0.01))}
  else if(chidf0>10 &  chidf0 <=35)
  {a <- c(seq(chidf0/5 , 2.5*chidf0, 0.01))}
  else (a <- c(seq(chidf0*0.4 , 1.7*chidf0, 0.01)))
  
 
  b <- dchisq(a,df = chidf0)
  alfa2 <- round(qchisq(p =  input$alfachi0, df = chidf0),2)
  aa <- c(alfa2, a[ a > alfa2], max(a))
  ab <- a[a > alfa2]
  bb <- c(0 , dchisq(ab, df = chidf0), 0)
  plot(a,b, type = "l" , bty="n", xlab = "x", ylab = "Probability Density", las=1)
  
  chio0 <- as.numeric(input$chio0)
  
  polygon(c(a[a<= alfa2], alfa2), c(b[a<= alfa2],0), col = "yellow")
  
  polygon(aa, bb, col = "green")
  
  polygon(c(chio0, chio0), c(0, dchisq(chio0, df = chidf0)) , col = "black",lwd=4  )
  
  legend("topright",legend = c("Acceptance","Critical Area", "Z Observe"),fill = c("Green", "yellow","black" ), bty = "n")
}) 


output$chitest0 <- renderText({
  chidf0 <- input$chidf0  
  alfa2 <- round(qchisq(p = (input$alfachi0), df = chidf0),2)
  
  input$actionchi0
  isolate(if(
    (as.numeric(input$chio0)) <= alfa2){
    print("Reject")}
    
    else{
      print("Accept")
    })
  
})

output$pvaluechitest0 <- renderText({
  
  input$actionchi0
  isolate(
    paste("P-Value is", round(pchisq(as.numeric(input$chio0) ,df = input$chidf0), 3) , sep = " ")
  )
  
})

output$cvaluechitest0 <- renderText({
  
  
  paste("Critical Value is",  round(qchisq(p = input$alfachi0,df = input$chidf0),2), sep = " ")
  
  
  
})
#############################################################################################################################

})