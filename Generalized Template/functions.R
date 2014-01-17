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
#   ymax = ""
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
      ymax = max(colSums(barPlotData))
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
          else if(type!="percent" && round(barPlotData[row,col])>=(.05*ymax)){
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


createCheckMarkTable = function(rdata,yaxis="clinic",col.label="Center:",checkMarkLegend="&#x2713; = Yes / In progress"){
  xaxis="month"
  checkmark = '&#x2713;'
  
  # write tmp_table with 0's and 1's
  if(USER=="state_user"){
    tmp_table = cbind(as.matrix(table(rdata[,yaxis],rdata[,xaxis])))
    # make sure all clinics included in table
    if(yaxis=="clinic"){
      clinic_df = data.frame()
      for(clinic in CLINIC_LIST){
        if (!(clinic %in% row.names(tmp_table))){
          clinic_df = rbind(clinic_df,rep(0,ncol(tmp_table)))
        }else{
          clinic_df = rbind(clinic_df,tmp_table[as.character(clinic),])
        }
      }
      rownames(clinic_df)=CLINIC_LIST
      colnames(clinic_df) = colnames(tmp_table)
      tmp_table = cbind(as.matrix(clinic_df))
    }
    
  }else{
    tmp_table = t(rdata)[pbps_list,]
    colnames(tmp_table)=rdata$month
    rownames(tmp_table)=gsub("[\\(\\)]","",regmatches(label(data[,rownames(tmp_table)]),gregexpr("^\\(.*?\\)",label(data[,rownames(tmp_table)]))))
    tmp_table[!(tmp_table %in% c("Yes","In progress"))] = as.numeric(0)
    tmp_table[tmp_table %in% c("Yes","In progress")] = as.numeric(1)
  }
  
  
  my_table = as.numeric(cbind(rownames(tmp_table)))
  # make sure months are in correct order
  my_table = order_months_as_columns(tmp_table,my_table)
  rownames(my_table) = rownames(tmp_table)
  colnames(my_table) = c(col.label,monthListFromPilot)
  my_table[,monthListFromPilot][my_table[,monthListFromPilot]==1] = checkmark
  my_table[my_table==0] = " "
  writeHTMLtable(my_table,col.label,checkMarkLegend)
}

writeHTMLtable <- function(my_table,col.label="Center:",legend="&#x2713; = Yes / In progress",include.colnames=FALSE,margin="auto",width1='1100px',width2='655px'){
  # cheat way of adding padding to first column
  my_table[,1] = as.character(paste("&nbsp;<b>",my_table[,1],"</b>"))
  
  td.width = 600/(ncol(my_table)-1)-3
  first.td.width = 50
  #total.width = td.width*640 + first.td.width
  css = paste("style='border-collapse:collapse;margin:",margin,";'",sep="")
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
  
  cat(paste("<div style='width:",width1,";margin:",margin,";,font-size:10pt;'><div style='width:",width2,";margin:",margin,";'><b>",col.label,"</b>",htmltable,"<div style='width:655px;text-align:right;margin-top:",margin,";'>",legend,"</div><div></div></div><br><br>"))
}

createAuditTable = function(rdata){
  tmp_table = t(rdata)[pbps_list,]
  colnames(tmp_table)=rdata$month
  rownames(tmp_table)=gsub("[\\(\\)]","",regmatches(label(data[,rownames(tmp_table)]),gregexpr("^\\(.*?\\)",label(data[,rownames(tmp_table)]))))
  tmp_table[!(tmp_table %in% c("Yes","In progress"))] = as.numeric(0)
  tmp_table[tmp_table %in% c("Yes","In progress")] = as.numeric(1)
  #tmp_table[tmp_table==1] = 
}

createSunFlowerPlot = function(rdata){
  cat(paste("</li><li class='subsection'><span class='header'>Sunflower Plot of Aggregate PBP Activity</span> <p>Following is a sunflower plot illustrating the number of centers with a status of 'Yes' or 'In progress' for each PBP. The y axis represents each PBP and the x axis represents each month. Each petal on a 'sunflower' represents a center that has indicated 'Yes' or 'In progress' for a given PBP and month.</p>"))
  h = 450  				
  w = 1000
  # start writing png
  pngFileName = paste('img/sunflowerplot.png',sep="")
  png(pngFileName,height=h,width=w)
  
  # bottom, left, top, right margins
  par(xpd=T, mar=c(5, 4, 4, 2) + c(0,7,0,12))
  sunflowerplot(rdata,xlab="Month",ylab="PBP",main="Number of Centers with Yes / In Progress PBPs",xaxt="n",pch=19,cex=1.75,col='orange',seg.col="orange",seg.lwd=2.5)
  axis(side=1,at=seq(1,length(monthListFromPilot)),labels=monthListFromPilot,line=0)
  
  # end writing png
  dev.off()
  
  # Restore default clipping rect
  par(mar=c(5, 4, 4, 2) + 0.1)  
  
  cat(paste("<div style='margin:auto;width:1100px;'><div style='width: 1000px; padding-left: 97px;'><img src='",pngFileName,"'></div></div>",sep=""));
}


order_months_as_columns = function(tmp_table,return_table,nodata=0){
  # make sure months are in correct order
  for(month in monthListFromPilot[1:length(monthListFromPilot)])
  {
    if(month %in% colnames(tmp_table)){
      return_table = cbind(return_table,tmp_table[,month])
    }else{
      if(nodata=="NA"){
        return_table = cbind(return_table,rep(NA,nrow(tmp_table)))
      }else{
        return_table = cbind(return_table,rep(nodata,nrow(tmp_table)))
      }
      
    }      
  }
  return_table = data.frame(return_table,stringsAsFactors=FALSE)
  return(return_table)
}


format_data = function(data){  
  # define data according to user
  if(USER == "state_user") {
    # Exclude Lake WBG data!
    data <- subset(data, record_id %nin% grep("^82-", data$record_id, value = TRUE))
  }else{
    data <- subset(data, record_id %in% grep(paste("^", USER_GROUP_ID, "-", sep = ""), data$record_id, value = TRUE))
  }
  
  # add clinic variable
  data$clinic = as.numeric(gsub("-[0-9a-zA-Z]+","",data$record_id))
  CLINIC_LIST = unique(data$clinic)
  CLINIC_LIST = sort(CLINIC_LIST)
  
  # count total records and babies before we delete duplicates; for now, field for total number of babies is different, but later on should change data dictionary
  TOTAL_RECORDS = nrow(data)
  TOTAL_BABIES = ifelse(metaData[1,"form_name"] == "nas_potentially_better_practices",sum(data$total_discharges,na.rm=TRUE),sum(data$infant_discharges,na.rm=TRUE))
  
  # delete any (both) duplicates where month, year and clinic are the same
  data$key = paste(data$month,data$year,data$clinic,sep="")
  duplicate_keys = unique(data[duplicated(data$key),"key"])
  if(!identical(duplicate_keys,character(0))){
    DUPLICATES = subset(data,data$key %in% duplicate_keys,c("clinic","record_id","month","year"))
    colnames(DUPLICATES) = c("Center","Record ID","Month","Year")   
    data = data[!(data$key %in% duplicate_keys),]
  }else{
    DUPLICATES = 0
  }
  
  # add month variable
  data$month = gsub("[\\(\\)]","",regmatches(data$month,gregexpr("\\(.*?\\)",data$month)))
  data = data[order(as.numeric(data$year),as.numeric(data$month)),]
  data$month = paste(data$month,substr(data$year, 3, 4),sep="/")
  
  # index data by record id
  rownames(data)=data$record_id
  
  # calculate how many months ago last records was entered
  MONTHS_AGO = as.numeric(gsub("/20[0-9]+", "", format(Sys.Date(), "%m/%Y"))) - as.numeric(gsub("/20[0-9]+", "", gsub("/","/20",tail(sort(unique(data$month)), n = 1))))
  MONTHS_AGO = ifelse(MONTHS_AGO<0,12+MONTHS_AGO,MONTHS_AGO)

  # GLOBAL variables
  PILOT_DATE <<- as.Date("2013-01-01")
  USER <<- USER
  DUPLICATES <<- DUPLICATES
  TOTAL_RECORDS <<- TOTAL_RECORDS
  TOTAL_BABIES <<- TOTAL_BABIES
  MONTHS_AGO <<- MONTHS_AGO
  CLINIC_LIST <<- CLINIC_LIST
  
  
  return(data)
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Extract Legend 
extract_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  grid.draw(legend)
} 



