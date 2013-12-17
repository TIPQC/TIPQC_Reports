########################################################################
## stackedBarChart #####################################################
########################################################################
# This function creates a stacked bar chart over time (months).
  # rdata - a dataframe (must include "month", columnOfInterest(below), and "clinic")
  # columnOfInterest - the column name of the data that you want to chart
  # chartTitle - chart title
  # type (optional) - chart percent/count; defaults to percent
  # ymax (optional) - the maximum value of the yaxis; defaults to 100% if percent or maxiumum value of data
  # fromDate (optional) - the first date at which to include data
  # categories (optional) - the possible values of the column of interest. Only include the ones you want to show up in the chart.
  # colors (optional) - the corresponding colors to use to represent the categories (above)
  # include.na (optional) - TRUE/FALSE. Decide whether or not to include NA values as a category in your plot.
  # include.totalrecords (optional) - TRUE/FALSE. Shows total number of records at the bottom of the plot.
########################################################################
stackedBarChart = function(rdata,columnOfInterest,chartTitle,type="percent", ymax="",fromDate=PILOT_DATE,categories=c("No","Yes"),colors=c("red","green"),include.na=TRUE,include.totalrecords=TRUE)
{
  # debugging - set all variables
#   rdata = allpbps
#   columnOfInterest = "pbp"
#   chartTitle = "Status Of All 10 PBPs"
#   type = "count"
#   ymax = 10
#   fromDate=PILOT_DATE
#   categories=c("No","Yes","In progress")
#   colors=c("red","green","lightgreen")
#   include.na=TRUE
  
  
  rdata = subset(rdata,!is.na(rdata$month))
  tempData = subset(rdata,select=c("month",columnOfInterest))
  tempData_notNA = subset(tempData,!is.na(tempData[,columnOfInterest]))
  months = seq(fromDate, Sys.Date(), by="1 month")
  monthListFromPilot = format(months,"%m/%y")
  denomList = c()
  
  plotData = data.frame()
  for(month in 1:length(monthListFromPilot))
  {
    denominator = sum(tempData$month==monthListFromPilot[month])
    
    for(i in 1:length(categories)){
      category_count = sum(tempData_notNA[,2][tempData_notNA$month==monthListFromPilot[month]]==categories[i])
      plotData[i,month] = ifelse(type=="percent",category_count/denominator*100,category_count)     
    }
    if(include.na){
      # NA
      category_count = sum(is.na(tempData[,2][tempData$month==monthListFromPilot[month]]))
      plotData[length(categories)+1,month] = ifelse(type=="percent",category_count/denominator*100,category_count)
    }    
    denomList = c(denomList,denominator)
  }
  colnames(plotData) = monthListFromPilot
  myrownames = categories
  if(include.na){
    myrownames = c(myrownames,"Blank")
    colors = c(colors,"purple")
  }
    
  rownames(plotData) = myrownames
  
  h = 450					
  w = 1000
  # start writing png
  pngFileName = paste('img/barchart_',columnOfInterest,'.png',sep="")
  png(pngFileName,height=h,width=w)
  
  # bottom, left, top, right margins
  par(xpd=T, mar=c(5, 4, 4, 2) + c(0,7,5,12))
  
  barPlotData = as.matrix(plotData)
  
  # determine ylimits
  if(ymax!=""){
    ylim = c(0,ymax)
  }else{
    if(type=="percent"){
      ymax = 100
    }else{
      ymax = max(barPlotData)
    }
  }
  ylim = c(0,ymax)
  
  b = barplot(barPlotData,
              main=chartTitle,
              col=colors,
              xlab = "Month",
              ylab = capitalize(type),
              ylim = ylim
  )
  box(lty=1,col='black')
  legendpos = ((tail(b,2)[2]-tail(b,2)[1])*1.5)+tail(b,2)[2]
  legend(legendpos,ymax,rev(rownames(plotData)),fill=rev(colors))
  #legend("topright",inset=c(-.15,0),rev(rownames(plotData)),fill=rev(colors))
  
  # if percent plot, add n at the bottom
  if(type=="percent" && include.totalrecords){
    mtext(side = 1, text = "Total No. Records (n):", at = 0.45, adj = 1, line = 4, cex = 0.85, font=2)
    axis(side=1,at=b,labels=denomList,tick = FALSE,cex.axis=.85,line=3,font=2)
  }  
  
  # add count/percentage labels on bars
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
        if(!is.na(barPlotData[row,col]) && !is.nan(barPlotData[row,col]))
        {
          # percentage labels
          if(type=="percent" && round(barPlotData[row,col])>=5)
            text(b[col],ypos[row,col],paste(round(barPlotData[row,col]),"%",sep=""))
          # raw count labels
          else if(type!="percent" && round(barPlotData[row,col])>=(.1*ymax)){
            text(b[col],ypos[row,col],round(barPlotData[row,col]))
          }
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
    axis(side=3,at=b,labels=clinicCount,hadj=1,tick = FALSE,cex.axis=.85,line=0,font=2,lty="solid") 
  }
  
  # end writing png
  dev.off()
  
  # Restore default clipping rect
  par(mar=c(5, 4, 4, 2) + 0.1)
  
  # output image to page
  cat(paste("<div style='margin:auto;width:1100px;'><div style='width: 1000px; padding-left: 97px;'><img src='",pngFileName,"'></div></div>",sep=""));
}

writeTmpTable = function(rdata){
  tmp_table = t(rdata)[pbps_list,]
  colnames(tmp_table)=rdata$month
  rownames(tmp_table)=gsub("[\\(\\)]","",regmatches(label(data[,rownames(tmp_table)]),gregexpr("^\\(.*?\\)",label(data[,rownames(tmp_table)]))))
  tmp_table[!(tmp_table %in% c("Yes","In progress"))] = as.numeric(0)
  tmp_table[tmp_table %in% c("Yes","In progress")] = as.numeric(1)
  return(tmp_table)
}

createCheckMarkTable = function(rdata,tmp_table="",yaxis="clinic",col.label="Center:",checkMarkLegend="Yes / In progress"){
  xaxis="month"
  checkmark = '&#x2713;'
  
  if(tmp_table==""){
    tmp_table = cbind(as.matrix(table(rdata[,yaxis],rdata[,xaxis])))
  }
  
  
  my_table = as.numeric(cbind(rownames(tmp_table)))
  for(month in monthListFromPilot[1:length(monthListFromPilot)])
  {
    if(month %in% colnames(tmp_table)){
      my_table = cbind(my_table,tmp_table[,month])
    }else{
      my_table = cbind(my_table,rep(0,nrow(tmp_table)))
    }      
  }
  my_table = data.frame(my_table,stringsAsFactors=FALSE)
  rownames(my_table) = rownames(tmp_table)
  colnames(my_table) = c(col.label,monthListFromPilot)
  my_table[,monthListFromPilot][my_table[,monthListFromPilot]==1] = checkmark
  my_table[my_table==0] = " "
  writeHTMLtable(my_table,col.label,checkMarkLegend)
}

writeHTMLtable <- function(my_table,col.label="Center:",checkMarkLegend="Yes / In progress",include.colnames=FALSE){
  # cheat way of adding padding to first column
  my_table[,1] = as.character(paste("&nbsp;<b>",my_table[,1],"</b>"))
  
  td.width = 600/(ncol(my_table)-1)-3
  first.td.width = 50
  #total.width = td.width*640 + first.td.width
  css = paste("style='border-collapse:collapse;margin:auto;'",sep="")
  start <- paste("<br><table border='1' ",css," >",sep="")
  
  end <- "</tr></tbody></table>"
  if(include.colnames){
    head <- paste(names(my_table),collapse="</th><th>")
    thead = paste("<thead><tr><th>",head,"</th></tr></thead>")
  }else{
    thead = ""
  }
  between = paste("</td><td align='center' style='width:",td.width,"px;'>",sep="")
  htmltable <- paste(start,thead,"<tbody><tr><td style='width:",first.td.width,"px;'>",paste(c(apply(my_table,1,paste,collapse=between)),collapse="</tr><tr><td>"),end,sep="")
  
  cat(paste("<div style='width:1100px;margin:auto;font-size:10pt;'><div style='width:655px;margin:auto;'><b>",col.label,"</b>",htmltable,"<div style='width:655px;text-align:right;'>&#x2713; = ",checkMarkLegend,"</div><div></div></div><br><br>"))
}

createSunFlowerPlot = function(data){
  cat(paste("</li><li class='subsection'><span class='header'>Sunflower Plot - Number of Centers with Yes / In Progress PBPs</span> <p>Following is a sunflower plot illustrating the number of centers with a status of 'Yes' or 'In progress' for each PBP. The y axis represents each PBP and the x axis represents each month. Each petal on a 'sunflower' represents a center that has indicated 'Yes' or 'In progress' for a given PBP and month.</p>"))
  h = 450  				
  w = 1000
  # start writing png
  pngFileName = paste('img/sunflowerplot.png',sep="")
  png(pngFileName,height=h,width=w)
  
  # bottom, left, top, right margins
  par(xpd=T, mar=c(5, 4, 4, 2) + c(0,7,0,12))
  sunflowerplot(data,xlab="Month",ylab="PBP",main="Number of Centers with Yes / In Progress PBPs",xaxt="n")
  axis(side=1,at=seq(1,length(monthListFromPilot)),labels=monthListFromPilot,line=0)
  
  # end writing png
  dev.off()
  
  # Restore default clipping rect
  par(mar=c(5, 4, 4, 2) + 0.1)  
  
  cat(paste("<div style='margin:auto;width:1100px;'><div style='width: 1000px; padding-left: 97px;'><img src='",pngFileName,"'></div></div>",sep=""));
}


