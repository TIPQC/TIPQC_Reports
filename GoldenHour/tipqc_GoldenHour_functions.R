
# set functions

#######################################
## Pchart For Multiple Plotted Lines ## 
#######################################
# Option 1 - numerator is % nonmissing fields
# Option 2 - numerator is % fields that are "Yes"
#######################################
pchart <- function(rdata,columnList,title,option,lowerYlim=0)
{
	# Expand right side of clipping rect to make room for the legend
	par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,0,25))
	
  # define variables
	colors = c("black","blue","red","green","purple","orange","brown")
	y = c()
  y_tmp = c()
	numList = c()
	denomList = c()
	legendLabels = c()
	allLabels = label(rdata)	
  
  # "option" determines the function that calculates the numerator
	switch(as.character(option),
    '1'={
     #sum the number of data points in given column that are not NA
     numerator_function = function(x){sum(!is.na(x))}
    },
    '2'={
     #sum the number of data points in given column are equal to "Yes"
     numerator_function = function(x){sum(x=="Yes",na.rm=TRUE)}
    }
	)	
  
  # calculate percentages to be plotted
	for(column in 1:length(columnList))
	{
		legendLabels = c(legendLabels,allLabels[match(columnList[column],names(rdata))])
    pchartData = pchart_data(cleaned_data = rdata,columnOfInterest = columnList[column],numerator_function)
		y_tmp = pchartData$percent
		y=c(y,y_tmp)
      
    if(column==1){
      myplotdata = data.frame(y_tmp)
      denomList = pchartData$denominator
      colnames(myplotdata)=columnList[1]
    }else{
      myplotdata[,columnList[column]] = y_tmp
    }  
		y_tmp = c()
	}
  
  # if any points on plot < lowerYlim, adjust lowerYlim
	if(lowerYlim==30){
		if(sum(!is.nan(y),na.rm=TRUE)>0){
			lowerYlim = min(30,floor(min(y,na.rm=TRUE)/10)*10)
		}else{
			lowerYlim = 30
		}
	}
	
	
	# begin creating pchart	
	plot(myplotdata[,columnList[1]],type="o", ylim=c(lowerYlim,100), col=colors[1] ,axes=FALSE,ann=FALSE)
	box(lty=1,col='black')
	# Make x axis 
	axis(1, at=1:length(monthList), lab=monthList)
	axis(2,las=1)
	# Label the x and y axes
	title(xlab= "Month")
	title(ylab= "Percent")
	# Create a title with a bold font
	title(main=title, font.main=2)
	# add Pilot date and kickoff date
	addPilotAndKickoffDates(monthList)					
  # Create a legend
	legend("topright",inset=c(-.3,0),legendLabels, cex=.8,col=colors[1:length(legendLabels)],lty=1,bg="white")
  # add remaining lines
	for(column in 2:length(columnList))
	{
	  lines(myplotdata[,columnList[column]],type="o",col=colors[column])
	}			
	# add n at the bottom
	mtext(side = 1, text = "Total No. Records (n):", at = 0.75, adj = 1, line = 4, cex = 0.85, font=2)
  axis(side=1,at=1:length(monthList),labels=denomList,hadj=1,tick = FALSE,cex.axis=.85,line=3,font=2) 
	
	# add No. clinics participating at the top
	if(USER == "state_user")
	{
		clinicCount = c()
		for(month in 1:length(monthList))
		{
			clinicCount = c(clinicCount,length(unique(rdata$clinic[rdata$month==monthList[month]])))
		}
		mtext(side = 3, text = "No. centers contributing data:", at = 0.75, adj = 1, line = 0, cex = 0.85,font=2)
	  axis(side=3,at=1:length(monthList),labels=clinicCount,hadj=1,tick = FALSE,cex.axis=.85,line=-1,font=2) 
	}	
			
	# Restore default clipping rect
	par(mar=c(5, 4, 4, 2) + 0.1)		
  
  return(myplotdata)			
}

### Data Entry Checks ###

 
dataChecks <- function(dataCheckCategory,data,columnList)
{
	switch(dataCheckCategory, 
				'dob'={
					#month/year of birth occurs before  May 2012 or in future or is missing
          #current month
          current_month = format(Sys.Date(),"%m")
          current_year = format(Sys.Date(),"%Y")
					condition = as.numeric( (as.Date(as.character(data$dob_fake),format="%m/%d/%y"))<as.Date("2012-05-01") | (data$month>current_month && data$yob > current_year))	 |   (is.na(data$dob_fake)	| data$dob_fake=="")	
					description = "A month/year of birth that is missing, occurring before May 2012, or occurring in the future" 
				},
				'ega'={
					#missing ega
					condition = is.na(data$ega)	
					description = "A missing gestational age at delivery (if admitted to NICU)"
				},
				'apgar'={
					#APGAR scores outside of the 0-10 range
					condition = (as.numeric(data$apgar_1) < 0 | as.numeric(data$apgar_1) > 10) | (as.numeric(data$apgar_5) < 0 | as.numeric(data$apgar_5) > 10) | (is.na(data$apgar_10) == FALSE & (as.numeric(data$apgar_10) < 0 | as.numeric(data$apgar_10) > 10))
					description = "An APGAR score outside of the 0-10 range (if admitted to NICU)"
				},
				'fio2'={
					#FiO2 score outside of the 0.21-1.0 range
					condition = as.numeric(data$fio2_5) < .21 | as.numeric(data$fio2_5) > 1
					description = "An FiO2 score outside of the 0.21-1.0 range (if admitted to NICU)"	  
				},
				'sao2'={
					#SaO2 score outside of the 0-100% range
					condition = as.numeric(data$sao2_5) < 0 | as.numeric(data$sao2_5) > 100	
					description = "An SaO2 score outside of the 0-100% range (if admitted to NICU)"				  
				},
				'dod'={
					#month/year of discharge occuring before month/year of birth, more than a year after dob, or after today
					condition = (as.numeric(as.Date(as.character(data$discharge_date_fake),format="%m/%d/%y") - as.Date(as.character(data$dob_fake),format="%m/%d/%y")) < 0 | 
                         as.numeric(as.Date(as.character(data$discharge_date_fake),format="%m/%d/%y") - as.Date(as.character(data$dob_fake),format="%m/%d/%y")) > 365) | 
                      (as.numeric(Sys.Date() - as.Date(as.character(data$discharge_date_fake),format="%m/%d/%y"))<0)
					description = "A month/year of discharge occurring before month/year of birth, >1 year after birth, or in the future (if admitted to NICU)" 
				}
			)		
	
	
	
	iffyCount = sum(condition,na.rm=TRUE)
	if(USER=="state_user")
	{
		clinicList = clinicList[order(clinicList)]
		clinic_iffyCount = c()
		for(clinic in 1:length(clinicList))
		{			
			# subset the data by the given clinic
			clinic_subset = subset(condition,data$clinic==clinicList[clinic])
			
			clinic_iffyCount = c(clinic_iffyCount,sum(clinic_subset,na.rm=TRUE))
		}
		return(clinic_iffyCount)
	}		
	else
	{		
	  dataSub = subset(data,condition, select = columnList ) 	
		cat(paste("<li>",description,":</li>",sep=""))
		if(iffyCount>0)
		{
			# get labels
			mylabels = c()
			allLabels = label(data)
			for(mycolumn in 1:length(columnList))
			{
				mylabels = c(mylabels,allLabels[match(columnList[mycolumn],names(data))])
			}
			colnames(dataSub) = mylabels
			
			print(xtable(dataSub),include.rownames=FALSE,type="html")
			cat("<br><br>")
		}else
			cat("<br><i>No problems identified.</i><br><br>")
	}

}

# add vertical line at pilot date and kick-off dates to plot
addPilotAndKickoffDates<-function(monthList)
{
	# match returns a vector of the positions of (first) matches of its first argument in its second.
	pilotNum = match("05/2012",monthList)
	if(!is.na(pilotNum))
	{
		mtext(side=1,text="(Pilot Date)",at=pilotNum,line=2,cex=.85,font=2)
		abline(v=pilotNum,xpd=FALSE)
	}			
	kickoffNum = match("09/2012",monthList)
	if(!is.na(kickoffNum))
	{
		mtext(side=1,text="(Kick-off Date)",at=kickoffNum,line=2,cex=.85,font=2)			
		abline(v=kickoffNum,xpd=FALSE)	
	}
}

###################
## XbarI SECTION ## 
###################
xbarI_section = function(rdata,columnOfInterest,h=480,w=850,yaxis_label="Minutes of Life")
{	
  # get all data where columnOfInterest is not missing
  alldata = subset(rdata,!is.na(rdata[,columnOfInterest]) ,select=c("clinic","study_id",columnOfInterest,"dob_fake","record"))
  
  # exit if there is no data
  if(nrow(alldata)==0){	  
    return(cat(paste("Your center does not have any data for ",mylabel,"</li>",sep="")))
  }   
  
	## set variables ##  
  # data points to be plotted
	allplotdata = subset(alldata,select=c(columnOfInterest))
  # the column of interest label to be integrated in paragraph text and plot text
	mylabel = label(rdata[,columnOfInterest])
  # variables that are dependent on USER
	if(USER == 'state_user'){
	  rownames(allplotdata) = NULL
	  section_explanatory_text = paste("The following figures are statistical process control charts for ",mylabel,". Records are sorted by month/year of birth. Records with the same month/year of birth are then sorted by record and then center number if necessary. The first plot contains all collected data and displays the month of birth in the x-axis. The second plot has only the most recent data and has both the sequential record ID and the month of birth in the x-axis. Note that the assigned sequential record ID is not the same as the patient study ID. The sequential record ID is a sequentially assigned number for all records meeting a particular graph's selection criteria. For example, point 12 is the 12th patient in sequence for the specific parameters in a single graph.")
	}else{
	  rownames(allplotdata) = alldata$record
	  section_explanatory_text = paste("The following figures are statistical process control charts for ",mylabel,". Records are sorted by month/year of birth. Records with the same month/year of birth are then sorted by study ID. The first plot contains all collected data and displays the month of birth in the x-axis. The second plot has only the most recent data and has both the study ID and the month of birth in the x-axis. Note that study ID's may not be sequential since records are sorted by month/year of birth first and some records may be excluded. ",sep="")    
	}		                          		                                                                   
	
	# plot 1
  # xbarI chart of all data
	png_filename1 = paste('img/xbarchart_',columnOfInterest,'.png',sep="")
	png(png_filename1,height=h,width=w)
		xbarI = xbarI_built_in_plot(allplotdata,yaxis_label)
		# add month labels
		alldata$order = seq(1:nrow(alldata))
		first_record_each_month = qcc.groups(alldata$order,format(as.Date(as.character(alldata$dob_fake),format="%m/%d/%y"),format="%Y-%m"))[,1]
		monthLabels = unique(format(as.Date(as.character(alldata$dob_fake),format="%m/%d/%y"),format="%b %Y"))
		axis(1,las=2,line=-3.5,at=first_record_each_month,labels=monthLabels,cex.axis=.8,lwd=0)
	dev.off()

	# plot 2
  # xbarI chart of most recent data
	# split data into 2 most recent months and 2 previous months
	# i.e. If today is 2/4/13, split data into Jan-Feb 2013 and Nov-Dec 2013
	plot2data = xbarI_plot2_formatData(rdata,columnOfInterest,USER)	
	recentData = ifelse(nrow(plot2data$qdata)>0,TRUE,FALSE)
	if(recentData)
	{                                    
		png_filename2 = paste('img/xbarchart_',columnOfInterest,"_2.png",sep="")
		png(png_filename2,height=h,width=w)
			xbarI_built_in_plot2(data=plot2data$qdata1,data.name=plot2data$plotDividerLabels[1],newdata=plot2data$qdata2,newdata.name=plot2data$plotDividerLabels[2],cutoffMonth=plot2data$monthLabels[4],yaxis_label)  
			# add month labels
			allplot2data = plot2data$qdata
			allplot2data$order = seq(1:nrow(allplot2data))
			first_record_each_month = qcc.groups(allplot2data$order,format(as.Date(as.character(allplot2data$dob_fake),format="%m/%d/%y"),format="%Y-%m"))[,1]
			monthLabels = unique(format(as.Date(as.character(allplot2data$dob_fake),format="%m/%d/%y"),format="%b %Y"))
			axis(1,las=2,line=-2.3,at=first_record_each_month,labels=monthLabels,cex.axis=.8,lwd=0)                                 
		dev.off()		
		plot2_ifExists = paste("<center><img class='page-break-none' src='",png_filename2,"'></center></li>",sep="")
	}else{
	  plot2_ifExists = paste("<li>Your center does not have any data for ",mylabel," since ",format(as.Date(as.character(tail(alldata,1)$dob_fake,format="%m/%d/%y"),format="%Y-%m")),".</li>",sep="")
	}  

  # print results to page
	cat(section_explanatory_text)
	outlierTable(alldata,USER,columnOfInterest,mylabel,yaxis_label,xbarI)
	cat(paste("<center><img class='page-break-none' src='",png_filename1,"'></center><br>",sep=""))
  cat(plot2_ifExists)
}

################
## XbarI PLOT ## 
################
xbarI_built_in_plot = function(allplotdata,yaxis_label)
{
	q = qcc(allplotdata,type="xbar.one",stats=TRUE,chart.all=TRUE,plot=FALSE,xaxt="n",labels=rep("",nrow(allplotdata)))
	title = paste(label(allplotdata)," - All Collected Data",sep="")
	q = xbarI_plot_elements(q,title,yaxis_label)	
	return(q)
}


#####################################
## XbarI PLOT 2 - Most Recent Data ## 
#####################################
xbarI_built_in_plot2 = function(data,data.name,newdata,newdata.name,cutoffMonth,yaxis_label)
{
	if (nrow(data)>0 & nrow(newdata)>0) {
		q = qcc(data,type="xbar.one",stats=TRUE,chart.all=TRUE,newdata=newdata,data.name=data.name,newdata.name=newdata.name,plot=FALSE)
		q$newdata.name=newdata.name
	}else if(nrow(data)>0 & nrow(newdata)==0) {
		q = qcc(data,type="xbar.one",stats=TRUE,chart.all=TRUE,data.name=data.name,plot=FALSE)
	}else if(nrow(data)==0 & nrow(newdata)>0) {
		q = qcc(newdata,type="xbar.one",stats=TRUE,chart.all=TRUE,data.name=newdata.name,plot=FALSE)
	}
	title = paste(label(data)," - Data From ",format(cutoffMonth,format="%b %Y")," to Present",sep="")
  q = xbarI_plot_elements(q,title,yaxis_label)	
	return(q)
}


#########################
## XbarI PLOT Elements ## 
#########################
xbarI_plot_elements = function(q,title,yaxis_label){
  
  # round center, std dev, limits
  q$center = round(q$center,2)
  q$std.dev = round(q$std.dev,2)
  q$limits = round(q$limits,2)
 
  # set ylim
  if(yaxis_label=='mmHg'){
    my_ylim = c(0,120)
  }else{
    my_ylim = c(0,200)
  }
  
  # plot
  plot(q,add.stats=TRUE,chart.all=TRUE,title=title,cex.axis=0.8,las.axis=2,xlab="",ylab=yaxis_label,ylim=my_ylim)
  
  # Restore default clipping rect
  par(mar=c(5, 4, 4, 2) + 0.1)	
  
  return(q)
}



##############################
## XbarI PLOT 2 - PREP DATA ## 
##############################
xbarI_plot2_formatData = function(rdata,columnOfInterest,USER) 
{
	# plot 2
	# split data into 2 most recent months and 2 previous months
	# i.e. If today is 2/4/13, split data into Jan-Feb 2013 and Nov-Dec 2013
	today = Sys.Date()
	thisMonth = format(today,format="%m/01/%Y")
	monthLabels = seq(as.Date(thisMonth,format="%m/%d/%Y"),length=4,by="-1 months")
	plotDividerLabels = c()
	
  # format human-readable date intervals
  # if the year is the same on both sides of hyphen (both sides of the interval), 
  # only display year on one side of hyphen (i.e. "Jan - Feb 2013" vs "Dec 2012 - Jan 2013")
	plotDividerLabels[1] = ifelse(format(monthLabels[4],format="%Y")==format(monthLabels[3],format="%Y"),
	                                paste(format(monthLabels[4],format="%b"),format(monthLabels[3],format="%b %Y"),sep=" - "),
	                                paste(format(monthLabels[4],format="%b %Y"),format(monthLabels[3],format="%b %Y"),sep=" - ")
                                )
	plotDividerLabels[2] = ifelse(format(monthLabels[2],format="%Y")==format(monthLabels[1],format="%Y"),
	                                paste(format(monthLabels[2],format="%b"),format(today,format="%b %d, %Y"),sep=" - "),
	                                paste(format(monthLabels[2],format="%b %Y"),format(today,format="%b %d, %Y"),sep=" - ")
                                )
	
	# get data starting from 3 months ago
  qdata = subset(rdata, !is.na(rdata[,columnOfInterest]) & as.numeric(as.Date(as.character(rdata$dob_fake),format="%m/%d/%y")) >= as.Date(monthLabels[4]), select=c("clinic","study_id",columnOfInterest,"dob_fake","record"))
  if(nrow(qdata)>0)
  {
  	if(USER=='state_user'){
  		rownames(qdata)=NULL
  	}else{
  		rownames(qdata)=qdata$record
  	}
  	qdata1 = subset(qdata,as.numeric(as.Date(as.character(qdata$dob_fake),format="%m/%d/%y"))>=as.Date(monthLabels[4]) & as.numeric(as.Date(as.character(qdata$dob_fake),format="%m/%d/%y"))<as.Date(monthLabels[2]),select=c(columnOfInterest))
    qdata2 = subset(qdata,as.numeric(as.Date(as.character(qdata$dob_fake),format="%m/%d/%y"))>=as.Date(monthLabels[2]),select=columnOfInterest)
  }else{
  	qdata1 = NULL
  	qdata2 = NULL
  }
  
  myreturn = list('qdata'=qdata,'monthLabels'=monthLabels,'plotDividerLabels'=plotDividerLabels,'qdata1'=qdata1,'qdata2'=qdata2)
  
  return(myreturn)
}

#####################
## XbarI MEAN PLOT ## 
#####################
xbarI_mean_plot = function(rdata,columnOfInterest,yaxis_label)
{
	my_subset = subset(rdata,!is.na(rdata[,columnOfInterest]) & !is.na(rdata$month),select=c(columnOfInterest,'month'))
	colnames(my_subset)=c(columnOfInterest,'gmonth')
	monthListSubset = unique(my_subset$gmonth) #use global variable
	monthKey = rbind(seq(1:length(monthListSubset)))
	colnames(monthKey)=monthListSubset
	my_subset$monthKey = monthKey[,my_subset$gmonth]
	
	my_subset_groups = qcc.groups(my_subset[,columnOfInterest],my_subset$monthKey)
	rownames(my_subset_groups) = monthListSubset
	# exclude rows where group size is <2
	my_subset_groups = my_subset_groups[which(rowSums(!is.na(my_subset_groups))>1),]
	q = qcc(my_subset_groups,type='xbar')
	q$center = round(q$center,2)
	q$std.dev = round(q$std.dev,2)
	my_title = paste(label(rdata[,columnOfInterest])," - Monthly Means",sep="")
	plot(q,title=my_title,xlab="Month",ylab=yaxis_label)
  return(q)
}

#######################
## STACKED BAR CHART ## 
#######################
## rdata: should have columns "month", "clinic" and the columnOfInterest
## columnOfInterest: column to be plotted
## chartTitle: title of the chart
## (optional) pilotDate: global variable PILOT_DATE is used if not set
## (optional) categories: categories to be plotted in order from bottom to top
## (optional) colors: colors to use (bottom to top)
## (optional) include.na: should NA be plotted? default = yes
stackedBarChart = function(rdata,columnOfInterest,chartTitle,fromDate=PILOT_DATE,categories=c("No","Yes"),colors=c("red","green"),include.na=TRUE)
{
  tempData = subset(rdata,select=c("month",columnOfInterest))
  tempData_notNA = subset(tempData,!is.na(tempData[,columnOfInterest]))
  months = seq(fromDate, Sys.Date(), by="1 month")
  monthListFromPilot = format(months,"%m/%Y")
  denomList = c()
  
  plotData = data.frame();
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



#######################
## TABLE OF OUTLIERS ## 
#######################
outlierTable = function(alldata,USER,columnOfInterest,mylabel,yaxis_label,qcc_object){
	if(yaxis_label=='mmHg'){
		my_outliertable_condition = alldata[,columnOfInterest]>qcc_object$limits[,"UCL"] | alldata[,columnOfInterest]<qcc_object$limits[,"LCL"] 
		my_outliertable_condition_text = ' > UCL or &lt; LCL'
	}else{
		my_outliertable_condition = alldata[,columnOfInterest]>180 | alldata[,columnOfInterest]>qcc_object$limits[,"UCL"]
		my_outliertable_condition_text = ' > 180 or > UCL'
	}
	if(USER=='state_user'){
		cat(paste("<li>The table of outliers displays the cases where ",tolower(yaxis_label),my_outliertable_condition_text,". Please review any cases listed from your center to ensure that cases and measures meet local criteria for inclusion in Golden Hour project, and that these outliers are not the result of data entry errors.</li><br><center><h4>Outliers (",yaxis_label,my_outliertable_condition_text,")</h4>",sep=""))
		outliers = subset(alldata,my_outliertable_condition,select=c("clinic","study_id",columnOfInterest))
		names(outliers)=c("Participating Center","Study ID",mylabel)
		print(xtable(outliers, align = c("c", "c", "c","r"),digits=0), type = "html", include.rownames = FALSE)
		cat("</center><br>")
	}else{
		outliers = subset(alldata,my_outliertable_condition,select=c("study_id",columnOfInterest))
		if(nrow(outliers)>0)
		{
			cat("<li>The table of outliers displays the cases where the ",tolower(yaxis_label),my_outliertable_condition_text,". Please review any cases listed from your center to ensure that cases and measures meet local criteria for inclusion in Golden Hour project, and that these outliers are not the result of data entry errors.</li><br><center><h4>Outliers (",yaxis_label,my_outliertable_condition_text,")</h4>",sep="")                                
			names(outliers)=c("Study ID",mylabel)
			print(xtable(outliers, align = c("c","c","r"),digits=0), type = "html", include.rownames = FALSE)
			cat("</center><br>")
		}else
		{
			cat("<li>Congratulations, there are no outliers (cases where the ",tolower(yaxis_label),my_outliertable_condition_text,") in the data for your center.</li><br>",sep="")
		}

		cat("<br>")
	}
	
}

#############################################
## ADD A "SHIFT" TO A PCHART OR XBAR CHART ## 
#############################################
addShift = function(points,groupSizes,monthList,fromDate,toDate,qcc_type="p",dataSet=NA,columnOfInterest=NA)
{
  monthKey = rbind(seq(1:length(monthList)))
  colnames(monthKey)=monthList
  
	# pull all dates that meet criteria from monthList
	# toDate = "end" means there is no upper limit. We grab from fromDate to the most recent existing date.
	if(toDate!="end"){
		chunkDates = as.Date(paste(monthList,"/01",sep=""),format="%m/%Y/%d")>=as.Date(fromDate) & as.Date(paste(monthList,"/01",sep=""),format="%m/%Y/%d")<=as.Date(toDate)
	}else{
		chunkDates = as.Date(paste(monthList,"/01",sep=""),format="%m/%Y/%d")>=as.Date(fromDate)
	}

	# if we have data that meet date criteria, calculate CL's and add to graph
	if(sum(chunkDates)!=0)
	{
    # add vertical line
		if(toDate!="end"){
			chunkNum = match(as.character(format(as.Date(toDate),"%m/%Y")),monthList)
			if(!is.na(chunkNum))
			{
				abline(v=chunkNum,xpd=FALSE)
			}				
		}
    if(qcc_type=="p"){
		  chunkqcc<-qcc(data=points[chunkDates],sizes=groupSizes[chunkDates],type=qcc_type,plot=FALSE)
		  xx = monthKey[chunkDates]
    }else{
      if(toDate!="end"){
        my_subset = subset(dataSet,as.Date(paste(dataSet$month,"/01",sep=""),format="%m/%Y/%d")>=as.Date(fromDate) & as.Date(paste(dataSet$month,"/01",sep=""),format="%m/%Y/%d")<=as.Date(toDate),select=c(columnOfInterest,'month'))
      }else{
        my_subset = subset(dataSet,as.Date(paste(dataSet$month,"/01",sep=""),format="%m/%Y/%d")>=as.Date(fromDate),select=c(columnOfInterest,'month'))
      }
      my_subset$monthKey = monthKey[,my_subset$month]
      
      my_subset_groups = qcc.groups(my_subset[,columnOfInterest],my_subset$monthKey)
      #rownames(my_subset_groups) = monthList
      # exclude rows where group size is <2
      my_subset_groups = my_subset_groups[which(rowSums(!is.na(my_subset_groups))>1),]
      xx = rownames(my_subset_groups)
      chunkqcc = qcc(my_subset_groups,type='xbar',plot=FALSE)
      
      # red dots
      for(i in 1:length(chunkqcc$violations$beyond.limits))
      {
        xxx = as.numeric(rownames(my_subset_groups)[chunkqcc$violations$beyond.limits[i]])
        yy = chunkqcc$statistics[as.character(xxx)]
        points(xxx,yy,type="o",pch=16,cex=1,col="red")
      }
      
      # orange dots
      for(i in 1:length(chunkqcc$violations$violating.runs))
      {
        xxx = as.numeric(rownames(my_subset_groups)[chunkqcc$violations$violating.runs[i]])
        yy = chunkqcc$statistics[as.character(xxx)]
        points(xxx,yy,type="o",pch=16,cex=1,col="orange")
      }
    }
		
		
		center = rep(chunkqcc$center,length(xx))
		
		LCL = chunkqcc$limits[, "LCL"]
		if(length(LCL)==1 && length(xx)!=1)
			LCL = rep(LCL,length(xx))	
    
		UCL = chunkqcc$limits[, "UCL"]
		if(length(UCL)==1 && length(xx)!=1)
			UCL = rep(UCL,length(xx))
    
    if(qcc_type=="p"){
      center = center*100
      LCL = LCL*100
      UCL = UCL*100
    }
    
		lines(x=xx,y=center)
		lines(x=xx,y=LCL,lty=2,type="s",lwd=1)
		lines(x=xx,y=UCL,lty=2,type="s",lwd=1)
		# if this is the last chunk, add CL labels
		if(toDate=="end"){
			mtext(side=4,line=.20,at=tail(center,1),text=paste("CL",round(tail(center,1)),sep=" - "),adj=0,cex=0.9,las=2)
			mtext(side=4,line=.10,at=tail(LCL,1),text=paste("LCL",round(tail(LCL,1)),sep=" - "),adj=0,cex=0.8,las=2)
			mtext(side=4,line=.10,at=tail(UCL,1),text=paste("UCL",round(tail(UCL,1)),sep=" - "),adj=0,cex=0.8,las=2)
		}
	}
	return(chunkqcc)
}
					
#############################
## MCHART (XBAR MEAN PLOT) ## 
#############################
mchart <- function(data,column,Ylim=c(0,100),yaxis_label="Minutes of Life")
{
  cat("<br><li>The following figure is an xbar mean plot grouped by the infant's month of birth. Control limits are calculated using the following formula: CL &plusmn; 3*SD. Sample SD's vary according to sample size.</li>")
  title = label(data[,column])
  
  # Expand right side of clipping rect to make room for the legend
  par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,0,7))
  
  data_remove_nas = subset(data,!is.na(data[,column]))
  
  # get monthly means & counts
  monthly_means = aggregate(data_remove_nas[,column],list(month=data_remove_nas$month),mean)
  monthly_counts = aggregate(data_remove_nas[,column],list(month=data_remove_nas$month),length)
  dataper <- data.frame(month = monthList, stringsAsFactors = FALSE)
  dataper$month <- factor(dataper$month, levels = unique(dataper$month),ordered = TRUE)
  
  # organize all needed plot data in dataframe
  plotdata = merge(dataper, monthly_means, by = "month", all = TRUE)
  plotdata = merge(plotdata, monthly_counts, by = "month", all = TRUE) 
  plotdata = plotdata[order(plotdata$month),]
  
  # give row & column names to plotdata
  colnames(plotdata) = c("month","y","count")
  rownames(plotdata) = NULL
  # counts of NA are actually counts of zero
  plotdata[is.na(plotdata$count),"count"]=0 
  
  # fix ylim in case of outliers
  ylower = min(Ylim[1],min(plotdata$y[!is.na(plotdata$y)])-5)
  yupper = max(Ylim[2],min(plotdata$y[!is.na(plotdata$y)])+5)
  Ylim = c(ylower,yupper)
  
  plot(plotdata$y,type="o",axes=FALSE,ann=FALSE,ylim=Ylim,pch=19,cex=0.8)
  box(lty=1,col='black')
  # Make x axis 
  axis(1, at=1:length(monthList), lab=monthList)
  axis(2,las=1)
  # Label the x and y axes
  title(xlab= "Month")
  title(ylab= yaxis_label)
  # Create a title with a bold font
  title(main=paste(title," - Monthly Means",sep=""), font.main=2)
  
  # add No. clinics participating at the top
  if(USER == "state_user")
  {
    clinicCount = c()
    for(month in 1:length(monthList))
    {
      clinicCount = c(clinicCount,length(unique(data_remove_nas$clinic[data_remove_nas$month==monthList[month]])))
    }
    mtext(side = 3, text = "No. centers contributing data:", at = 0.0, adj = 1, line = 0.5, cex = 0.85,font=2)
    axis(side=3,at=1:length(monthList),labels=clinicCount,hadj=1,tick = FALSE,cex.axis=.85,line=-.5,font=2) 
  }
  # add total num records at bottom
  mtext(side = 1, text = "Total No. Records (n):", at = 0.45, adj = 1, line = 4, cex = 0.85, font=2)
  axis(side=1,at=1:length(monthList),labels=plotdata$count,tick = FALSE,cex.axis=.85,line=3,font=2)
  
  return(plotdata)
}


################################################
## Prep pchart data for plot with single line ## 
################################################
pchart_data = function(cleaned_data,columnOfInterest,numerator_function){
  
  # calculate numerator and denominator
  if(nrow(cleaned_data)){
    denom = aggregate(cleaned_data[,columnOfInterest],by=list(month=cleaned_data$month),length)
    data_columnOfInterest_notNA = subset(cleaned_data,!is.na(cleaned_data[,columnOfInterest]))
    numer = aggregate(data_columnOfInterest_notNA[,columnOfInterest],by=list(month=data_columnOfInterest_notNA$month),numerator_function)
    clinicCount = aggregate(cleaned_data$clinic,by=list(month=cleaned_data$month),function(x){return(length(unique(x)))})
  }else{
    denom = numer = clinicCount = data.frame("11/2013",NA)
    colnames(denom) = colnames(clinicCount) = colnames(numer) = c("month","x")
  }  
  
  # put together dataframe of data per month
  dataper <- data.frame(month = monthList, stringsAsFactors = FALSE)
  dataper$month <- factor(dataper$month, levels = unique(dataper$month),ordered = TRUE)
  dataper = merge(dataper,clinicCount,by="month",all=TRUE)
  dataper = merge(dataper,numer, by = "month", all = TRUE)
  dataper = merge(dataper,denom,by="month",all=TRUE)  
  colnames(dataper)=c("month","clinicCount","numerator","denominator")
  # order by month
  dataper = dataper[order(dataper$month),]
  # counts of NA are actually counts of zero
  dataper[is.na(dataper$denominator),"denominator"]=0 
  dataper[is.na(dataper$numerator),"numerator"]=0 
  # calculate percentages  
  dataper$percent = ifelse(dataper$denominator==0,NaN,dataper$numerator/dataper$denominator*100)

  return(dataper)
}
###################################################
## Plot Pchart with Single Line & Control Limits ## 
###################################################
pchart_plot = function(dataper,title,shifts="default",nlabel="Total no. records (n):",parmar=c(5, 4, 4, 2) + 0.1+c(0,7,0,25)){
  # Expand right side of clipping rect to make room for the legend
  par(xpd=T, mar=parmar)
  # begin creating pchart  
  plot(dataper$percent,type="o", ylim=c(0,100), ,axes=FALSE,ann=FALSE)
  box(lty=1,col='black')
  # Make x axis 
  axis(1, at=1:length(dataper$month), lab=dataper$month)
  axis(2,las=1)
  # Label the x and y axes
  title(xlab= "Month")
  title(ylab= "Percent")
  # Create a title with a bold font
  title(main=title, font.main=2)
  # add Pilot date and kickoff date
  addPilotAndKickoffDates(dataper$month)  
  
  ## pilot date (5/1/2012-8/31/2012)
  addShift(points=dataper$numerator,groupSizes=dataper$denominator,dataper$month,"2012-05-01","2012-09-01")
  if(shifts=="default"){    
    ## kick-off date forward
    addShift(points=dataper$numerator,groupSizes=dataper$denominator,dataper$month,"2012-09-01","end")
  }
  
  # add n text at bottom
  mtext(side = 1, text = nlabel, at = 0.75, adj = 1, line = 4, cex = 0.85, font=2)
  axis(side=1,at=1:length(dataper$month),labels=dataper$denominator,hadj=1,tick = FALSE,cex.axis=.85,line=3,font=2) 
  
  # add No. clinics participating at the top (if state user)
  if(USER == "state_user"){
    mtext(side = 3, text = "No. centers contributing data:", at = 0.75, adj = 1, line = 0, cex = 0.85,font=2)
    axis(side=3,at=1:length(monthList),labels=dataper$clinicCount,hadj=1,tick = FALSE,cex.axis=.85,line=-1,font=2) 
  }
}

#############################################
## Print Run Time to Page IF in Debug Mode ## 
#############################################
check_run_time=function(previous.time){
  if(exists("debug_mode")){
    end.time<-Sys.time()
    end.times <- format(end.time, "%a %b %d, %Y at %X")
    run.time<-difftime(end.time,previous.time,units="secs")
    cat("<br/>Run time:", run.time,'secs.<br/>') 
    
    return(end.time)
  }    
}


