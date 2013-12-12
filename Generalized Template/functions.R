stackedBarChart = function(rdata,columnOfInterest,chartTitle,fromDate=PILOT_DATE,categories=c("No","Yes"),colors=c("red","green"),include.na=TRUE)
{
  rdata = subset(rdata,!is.na(rdata$month))
  tempData = subset(rdata,select=c("month",columnOfInterest))
  tempData_notNA = subset(tempData,!is.na(tempData[,columnOfInterest]))
  months = seq(fromDate, Sys.Date(), by="1 month")
  monthListFromPilot = format(months,"%m/%Y")
  denomList = c()
  
  plotData = data.frame()
  for(month in 1:length(monthListFromPilot))
  {
    denominator = sum(tempData$month==monthListFromPilot[month])
    
    for(i in 1:length(categories)){
      plotData[i,month] = sum(tempData_notNA[,2][tempData_notNA$month==monthListFromPilot[month]]==categories[i]) / denominator*100
    }
    if(include.na){
      # NA
      plotData[length(categories)+1,month] = sum(is.na(tempData[,2][tempData$month==monthListFromPilot[month]])) / denominator*100
    }    
    denomList = c(denomList,denominator)
  }
  colnames(plotData) = monthListFromPilot
  myrownames = categories
  if(include.na){
    myrownames = c(myrownames,"NA")
    colors = c(colors,"purple")
  }
    
  rownames(plotData) = myrownames
  
  h = 450					
  w = 1000
  # start writing png
  pngFileName = paste('img/barchart_',columnOfInterest,'.png',sep="")
  png(pngFileName,height=h,width=w)
  par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,5,12))
  
  barPlotData = as.matrix(plotData)
  
  b = barplot(barPlotData,
              main=chartTitle,
              col=colors,
              xlab = "Month",
              ylab = "Percent"							  
  )
  box(lty=1,col='black')
  #legendpos = ((tail(b,2)[2]-tail(b,2)[1]))*length(b)/3.5*-1
  legend("topright",inset=c(-.15,0),rev(rownames(plotData)),fill=rev(colors))
  # add n at the bottom
  mtext(side = 1, text = "Total No. Records (n):", at = 0.45, adj = 1, line = 4, cex = 0.85, font=2)
  axis(side=1,at=b,labels=denomList,tick = FALSE,cex.axis=.85,line=3,font=2)
  
  # add percentage labels if percentage >=5%
  # Find the top y position of each block 
  ypos <- apply(barPlotData, 2, cumsum)
  # Move it downwards half the size of each block
  ypos <- ypos - barPlotData/2
  
  if(nrow(barPlotData)>0)
  {
    for(row in 1:nrow(barPlotData))
    {
      for(col in 1:ncol(barPlotData))
      {
        if(!is.na(barPlotData[row,col]) && !is.nan(barPlotData[row,col]) && round(barPlotData[row,col])>=5)
        {
          text(b[col],ypos[row,col],paste(round(barPlotData[row,col]),"%",sep=""))
          
        }
      }
    }
  }
  
  
  # add No. clinics participating at the top
  if(USER == "state_user")
  {
    clinicCount = c()
    for(month in 1:length(monthListFromPilot))
    {
      clinicCount = c(clinicCount,length(unique(rdata$clinic[rdata$month==monthListFromPilot[month]])))
    }
    mtext(side = 3, text = "No. centers contributing data:", at = 0.0, adj = 1, line = 1, cex = 0.85,font=2)
    axis(side=3,at=b,labels=clinicCount,hadj=1,tick = FALSE,cex.axis=.85,line=0,font=2) 
  }
  
  # end writing png
  dev.off()
  
  # Restore default clipping rect
  par(mar=c(5, 4, 4, 2) + 0.1)
  
  # output image to page
  cat(paste("<center><img src='",pngFileName,"'></center><br>",sep=""));
}
