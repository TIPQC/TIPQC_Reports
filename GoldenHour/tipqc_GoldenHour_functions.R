
# set functions
# Option 1 - numerator is % nonmissing fields
# Option 2 - numerator is % fields that are "1"
pchart <- function(rdata,columnList,title,option,USER,lowerYlim=0)
{
	# Expand right side of clipping rect to make room for the legend
	par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,0,25))
	#par(mar=c(5, 4, 4, 2) + 0.1)
					
	colors = c("black","blue","red","green","purple","orange","brown")
	y = c()
	numList = c()
	denomList = c()
	#monthList = unique(rdata$month[!is.na(rdata$month)])
	months = seq(as.Date("2012-05-01"), Sys.Date(), by="1 month")
	monthList = format(months,"%m/%Y")
	addControlLimits=0
	
	# get labels for legend
	mylabels = c()
	allLabels = label(rdata)
	for(column in 1:length(columnList))
	{
		mylabels = c(mylabels,allLabels[match(columnList[column],names(rdata))])
		
		for(month in 1:length(monthList))
		{
			monthSubset = rdata[rdata$month==monthList[month],]
			colNum = match(columnList[column],names(rdata)) #get the column number of the column name
			denominator = sum(rdata$month==monthList[month],na.rm=TRUE) # sum total number of data points for given month
			# set numerator
			switch(option, 
				'1'={
					#sum the number of data points in given column for given month that are not NA
					numerator = sum(!is.na(rdata[,colNum][rdata$month==monthList[month]])) 								  
				},
				'2'={
					#sum the number of data points in given column for given month that are equal to "1"
					numerator = sum(rdata[,colNum][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
				},
				'3'={
					fio2_5_colNum = match("fio2_5",names(rdata)) #get the column number of the column name
					#sum the number of data points where fio2>.21 & sao2>=80 & sa02<=85 & fio2!=1
					s1 = subset(rdata,rdata$fio2_5>.21,select=c("fio2_5","sao2_5","month"))
					s2 = subset(s1,s1$sao2_5>=80 & !is.na(month))
					s3 = subset(s2,s2$sao2_5<=85)
					s4 = subset(s3,s3$fio2_5!=1)
					# numerator for given month
					numerator = 0
					if(nrow(s4)>0)
					{
						for(i in 1:nrow(s4))
						{
							if(s4[i,3]==monthList[month]) 
								numerator = numerator+1
						}
					}
					
					# total number of data points where fio2>.21 for given month
					denominator = sum(rdata[,fio2_5_colNum][rdata$month==monthList[month]]>.21,na.rm=TRUE)  
					#
					mylabels = c()
				},
				'4'={
					numerator = sum(rdata[,colNum][rdata$month==monthList[month]]>=36 & rdata[,colNum][rdata$month==monthList[month]]<=37.5,na.rm=TRUE) 
				},
				'5'={
					#sum the number of data points where surfactant received
					numerator = sum(rdata[,'surfactant_received'][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
					#sum the number of data points where surfactant eligible
					denominator =  sum(rdata[,'surfactant_eligible'][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
					
				},
				'6'={
					# OXYGEN MANAGEMENT PCHART 1
					# denominator - all cases EXCEPT where sao2 and/or fio2 are NA, and EXCEPT where room air and above average
					dSub = subset(monthSubset,!is.na(monthSubset$sao2_5) & !is.na(monthSubset$fio2_5))
					dSub = subset(dSub,!(dSub$sao2_5>=95 & dSub$fio2_5==.21))
					denominator = nrow(dSub)
					#numerator - 75%<SaO2<95% (sao2 & fio2 may NOT be NA)
					numerator = nrow(subset(dSub,dSub$sao2_5>75 & dSub$sao2_5<95,select=c("sao2_5")))	
					addControlLimits=1		
					#expand bottom margin 		
					par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(7,7,0,25))
				},
				'7'={
					# OXYGEN MANAGEMENT PCHART 2
					# denominator: all cases where fio2>.21 (sao2 & fio2 may NOT be NA)
					dSub = subset(monthSubset,!is.na(monthSubset$sao2_5) & !is.na(monthSubset$fio2_5) & monthSubset$fio2_5 > .21, select=c('sao2_5','fio2_5','month'))
					denominator = nrow(dSub)
					#numerator: 80%<=SaO2<=85% and fio2>.21 (sao2 & fio2 may NOT be NA)
					numerator = nrow(subset(dSub,dSub$sao2_5>=80 & dSub$sao2_5<=85,select=c("sao2_5","fio2_5","month")))		
					addControlLimits=1	
					#expand bottom margin	
					par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(7,7,0,25))	
					
				},		
				{
					#default
				   	numerator = sum(!is.na(rdata[,colNum][rdata$month==monthList[month]]))
				}
			)			
			if(denominator==0)
			{
				percent = NaN
			}else
			{
				percent = numerator/denominator*100
			}
				
			
			y = c(y,percent) # make array of y values for plot
		}
		
	}
	if(lowerYlim==30){
		if(length(y[!is.nan(y)&!is.na(y)])>0){
			lowerYlim = min(30,floor(min(y[!is.nan(y)&!is.na(y)])/10)*10)
		}else{
			lowerYlim = 30
		}
	}
		
	
	
	y = c()
	# begin creating pchart	
	for(column in 1:length(columnList))
	{	
		for(month in 1:length(monthList))
		{
			monthSubset = rdata[rdata$month==monthList[month],]
			colNum = match(columnList[column],names(rdata)) #get the column number of the column name
			denominator = sum(rdata$month==monthList[month],na.rm=TRUE) # sum total number of data points for given month
			# set numerator
			switch(option, 
				'1'={
					#sum the number of data points in given column for given month that are not NA
					numerator = sum(!is.na(rdata[,colNum][rdata$month==monthList[month]])) 								  
				},
				'2'={
					#sum the number of data points in given column for given month that are equal to "1"
					numerator = sum(rdata[,colNum][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
				},
				'3'={
					fio2_5_colNum = match("fio2_5",names(rdata)) #get the column number of the column name
					#sum the number of data points where fio2>.21 & sao2>=80 & sa02<=85 & fio2!=1
					s1 = subset(rdata,rdata$fio2_5>.21,select=c("fio2_5","sao2_5","month"))
					s2 = subset(s1,s1$sao2_5>=80 & !is.na(month))
					s3 = subset(s2,s2$sao2_5<=85)
					s4 = subset(s3,s3$fio2_5!=1)
					# numerator for given month
					numerator = 0
					if(nrow(s4)>0)
					{
						for(i in 1:nrow(s4))
						{
							if(s4[i,3]==monthList[month]) 
								numerator = numerator+1
						}
					}
					
					# total number of data points where fio2>.21 for given month
					denominator = sum(rdata[,fio2_5_colNum][rdata$month==monthList[month]]>.21,na.rm=TRUE)  
					#
					mylabels = c()
					mytext = "Total No. Records \n with FiO2>0.21 (n):";
					addControlLimits=1
				},
				'4'={
					numerator = sum(rdata[,colNum][rdata$month==monthList[month]]>=36 & rdata[,colNum][rdata$month==monthList[month]]<=37.5,na.rm=TRUE) 
					addControlLimits=1
				},
				'5'={
					#sum the number of data points where surfactant received
					numerator = sum(rdata[,'surfactant_received'][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
					#sum the number of data points where surfactant eligible
					denominator =  sum(rdata[,'surfactant_eligible'][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
					mytext = "Total No. Surfactant \nEligible Records (n):";
					addControlLimits=1
					
				},
				'6'={
					# OXYGEN MANAGEMENT PCHART 1
					# denominator - all cases EXCEPT where sao2 and/or fio2 are NA, and EXCEPT where room air and above average
					dSub = subset(monthSubset,!is.na(monthSubset$sao2_5) & !is.na(monthSubset$fio2_5))
					dSub = subset(dSub,!(dSub$sao2_5>=95 & dSub$fio2_5==.21))
					denominator = nrow(dSub)
					#numerator - 75%<SaO2<95% (sao2 & fio2 may NOT be NA)
					numerator = nrow(subset(dSub,dSub$sao2_5>75 & dSub$sao2_5<95,select=c("sao2_5")))	
					addControlLimits=1		
					#expand bottom margin 		
					par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(7,7,0,25))
				},
				'7'={
					# OXYGEN MANAGEMENT PCHART 2
					# denominator: all cases where fio2>.21 (sao2 & fio2 may NOT be NA)
					dSub = subset(monthSubset,!is.na(monthSubset$sao2_5) & !is.na(monthSubset$fio2_5) & monthSubset$fio2_5 > .21, select=c('sao2_5','fio2_5','month'))
					denominator = nrow(dSub)
					#numerator: 80%<=SaO2<=85% and fio2>.21 (sao2 & fio2 may NOT be NA)
					numerator = nrow(subset(dSub,dSub$sao2_5>=80 & dSub$sao2_5<=85,select=c("sao2_5","fio2_5","month")))		
					addControlLimits=1	
					#expand bottom margin	
					par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(7,7,0,25))	
					
				},				
				{
					#default
				   	numerator = sum(!is.na(rdata[,colNum][rdata$month==monthList[month]]))
				}
			)		
			if(denominator==0)
			{
				percent = NaN
			}else
			{
				percent = numerator/denominator*100
			}
				
			
			y = c(y,percent) # make array of y values for plot
			if(column==1)
			{
				numList = c(numList,numerator)
				denomList = c(denomList,denominator)	
			}
		}
		if(column==1)
		{
			plot(y,type="o", ylim=c(lowerYlim,100), col=colors[column] ,axes=FALSE,ann=FALSE)
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
			if(length(columnList)!=1)
			{	
				# put legend on chart			
				#legend(1.5,30,mylabels, cex=.8,col=colors[1:length(mylabels)],lty=1,bg="white")
				# put legend outside chart
				#legend(length(monthList)+.5,100,mylabels, cex=.8,col=colors[1:length(mylabels)],lty=1,bg="white")
				legend("topright",inset=c(-.3,0),mylabels, cex=.8,col=colors[1:length(mylabels)],lty=1,bg="white")
			}		
				
			# add control limits
			if(addControlLimits)
			{
				## pilot date (5/1/2012-8/31/2012)
				addShift(points=numList,groupSizes=denomList,monthList,"2012-05-01","2012-09-01")
				
				if(option==5 && USER=="state_user")
				{
					# 09/2012 - 01/2013
					addShift(points=numList,groupSizes=denomList,monthList,"2012-09-01","2013-01-01")
					
					# 01/2013 - forward
					addShift(points=numList,groupSizes=denomList,monthList,"2013-01-01","end")
	
					
				}else{				
					## kick-off date forward
					addShift(points=numList,groupSizes=denomList,monthList,"2012-09-01","end")					
				}
				
			}

		} else
		{
			lines(y,type="o",col=colors[column])
		}
		y=c()			
		
	}	
	# add n at the bottom
	if(exists("mytext"))
	{
		mtext(side = 1, text = mytext, at = 0.75, adj = 1, line = 4, cex = 0.85, font=2)
	}else
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
	if(option==6){
		# add "Room air and above average" margin
		mtext(side = 1, text = "Room air and above average:", at = 0.75, adj = 1, line = 5, cex = 0.85, font=2)
		roomAirAboveAverage = c()
		for(month in 1:length(monthList)){
			monthSubset = rdata[rdata$month==monthList[month],]
			# number of infants where saO2>=95% and FiO2=.21
			mycount = nrow(subset(monthSubset,monthSubset$sao2_5>=95 & !is.na(monthSubset$sao2_5) & monthSubset$fio2_5 == .21 & !is.na(monthSubset$fio2_5),select=c('sao2_5','fio2_5','month')))
			roomAirAboveAverage = c(roomAirAboveAverage,mycount)
		}
		axis(side=1,at=1:length(monthList),labels=roomAirAboveAverage,hadj=1,tick = FALSE,cex.axis=.85,line=4,font=2) 
		
		# add missing sa02/fio2 count in margin
		mtext(side = 1, text = "Missing SaO2 and/or FiO2:", at = 0.75, adj = 1, line = 6, cex = 0.85, font=2)
		missing = c()
		for(month in 1:length(monthList)){
			monthSubset = rdata[rdata$month==monthList[month],]
			# number of infants where saO2 and/or FiO2 is NA
			mycount = nrow(subset(monthSubset,is.na(monthSubset$sao2_5) | is.na(monthSubset$fio2_5),select=c('sao2_5','fio2_5','month')))
			missing = c(missing,mycount)
		}
		axis(side=1,at=1:length(monthList),labels=missing,hadj=1,tick = FALSE,cex.axis=.85,line=5,font=2) 
		
	}else if(option==7){
		# add "Max O2 and below target" margin
		mtext(side = 1, text = "Max O2 and below target:", at = 0.75, adj = 1, line = 5, cex = 0.85, font=2)
		maxO2belowTarget = c()
		for(month in 1:length(monthList)){
			monthSubset = rdata[rdata$month==monthList[month],]
			# number of infants where saO2<85% and FiO2=1.0
			mycount = nrow(subset(monthSubset,monthSubset$sao2_5<85 & !is.na(monthSubset$sao2_5) & monthSubset$fio2_5 == 1 & !is.na(monthSubset$fio2_5),select=c('sao2_5','fio2_5','month')))
			maxO2belowTarget = c(maxO2belowTarget,mycount)
		}
		axis(side=1,at=1:length(monthList),labels=maxO2belowTarget,hadj=1,tick = FALSE,cex.axis=.85,line=4,font=2) 
		
		# add missing sa02/fio2 count in margin
		mtext(side = 1, text = "Missing SaO2 and/or FiO2:", at = 0.75, adj = 1, line = 6, cex = 0.85, font=2)
		missing = c()
		for(month in 1:length(monthList)){
			monthSubset = rdata[rdata$month==monthList[month],]
			# number of infants where saO2 and/or FiO2 is NA
			mycount = nrow(subset(monthSubset,is.na(monthSubset$sao2_5) | is.na(monthSubset$fio2_5),select=c('sao2_5','fio2_5','month')))
			missing = c(missing,mycount)
		}
		axis(side=1,at=1:length(monthList),labels=missing,hadj=1,tick = FALSE,cex.axis=.85,line=5,font=2)

		
	}
	
	# Restore default clipping rect
	par(mar=c(5, 4, 4, 2) + 0.1)		
	
			
}

### Data Entry Checks ###

 
dataChecks <- function(dataCheckCategory,USER,rdata,columnList,clinicList)
{
	switch(dataCheckCategory, 
				'dob'={
					#month/year of birth occurs before  May 2012 or in future or is missing
          #current month
          current_month = format(Sys.Date(),"%m")
          current_year = format(Sys.Date(),"%Y")
					condition = as.numeric(as.Date(as.character(rdata$dob_fake),format="%m/%d/%y"))<as.Date("2012-05-01") | (rdata$month>current_month && rdata$yob > current_year)	 | is.na(rdata$dob_fake)	| rdata$dob_fake==""	
					description = "A month/year of birth that is missing, occurring before May 2012, or occurring in the future" 
				},
				'ega'={
					#missing ega
					condition = is.na(rdata$ega)	
					description = "A missing gestational age at delivery (if admitted to NICU)"
				},
				'apgar'={
					#APGAR scores outside of the 0-10 range
					condition = as.numeric(rdata$apgar_1) < 0 | as.numeric(rdata$apgar_1) > 10 | as.numeric(rdata$apgar_5) < 0 | as.numeric(rdata$apgar_5) > 10 | (is.na(rdata$apgar_10) == FALSE & (as.numeric(rdata$apgar_10) < 0 | as.numeric(rdata$apgar_10) > 10))
					description = "An APGAR score outside of the 0-10 range (if admitted to NICU)"
				},
				'fio2'={
					#FiO2 score outside of the 0.21-1.0 range
					condition = as.numeric(rdata$fio2_5) < .21 | as.numeric(rdata$fio2_5) > 1
					description = "An FiO2 score outside of the 0.21-1.0 range (if admitted to NICU)"	  
				},
				'sao2'={
					#SaO2 score outside of the 0-100% range
					condition = as.numeric(rdata$sao2_5) < 0 | as.numeric(rdata$sao2_5) > 100	
					description = "An SaO2 score outside of the 0-100% range (if admitted to NICU)"				  
				},
				'dod'={
					#month/year of discharge occuring before month/year of birth, more than a year after dob, or after today
					condition = as.numeric(as.Date(as.character(rdata$discharge_date_fake),format="%m/%d/%y") - as.Date(as.character(rdata$dob_fake),format="%m/%d/%y")) < 0 | as.numeric(as.Date(as.character(rdata$discharge_date_fake),format="%m/%d/%y") - as.Date(as.character(rdata$dob_fake),format="%m/%d/%y")) > 365 | 	as.numeric(Sys.Date() - as.Date(as.character(rdata$discharge_date_fake),format="%m/%d/%y"))<0
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
			clinic_subset = subset(condition,rdata$clinic==clinicList[clinic])
			
			clinic_iffyCount = c(clinic_iffyCount,sum(clinic_subset,na.rm=TRUE))
		}
		return(clinic_iffyCount)
	}		
	else
	{		
		dataSub = subset(rdata,condition, select = columnList)		
		cat(paste("<li>",description,":</li>",sep=""))
		if(iffyCount>0)
		{
			# get labels
			mylabels = c()
			allLabels = label(rdata)
			for(mycolumn in 1:length(columnList))
			{
				mylabels = c(mylabels,allLabels[match(columnList[mycolumn],names(rdata))])
			}
			colnames(dataSub) = mylabels
			
			print(xtable(dataSub),include.rownames=FALSE,type="html")
			cat("<br><br>")
		}else
			cat("<br><i>No problems identified.</i><br><br>")
	}

}

# add pilot date and kick-off dates to plot
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

qcc_xbar_one_plot = function(allplotdata,totalGroupNum)
{
	par("mar"=c(5, 4, 4, 2) + 0.1+c(5*totalGroupNum, 0,0,0),"bg"  = "lightgray")
	rownames(allplotdata)=NULL
	indices <- 1:nrow(allplotdata)
	
	plot(indices,allplotdata$iv_access_age,type="n",ylim=c(0,200),xlim=range(indices),xlab="Record",ylab="Minutes of Life",main="Age IV Solution Started (Minutes of Life) - All Collected Data")
	                               
	# add white box to be plotted on
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
	                                       col = "white")
	# plot on box
	lines(indices, allplotdata$iv_access_age, type = "b", pch=20) 
	qcc_xbar_one_plot_elements(allplotdata,1,TRUE)
}

qcc_xbar_one_plot_elements = function(data,groupNum,last=FALSE,mylabel='')
{
	plt <- par()$plt; usr <- par()$usr
    px <- diff(usr[1:2])/diff(plt[1:2])
    xfig <- c(usr[1]-px*plt[1], usr[2]+px*(1-plt[2]))
    at.col <- xfig[1] + diff(xfig[1:2])*c(0.10, 0.40, 0.65)
    startLine = 5*groupNum
	if(nrow(data)<=0)
	{
		if(mylabel!="")
		{
			mtext(paste(mylabel,":",sep=""), side = 1, line = startLine, adj = 0, at=at.col[1],font=2)
		}   
		mtext("No Data",side=1,line=startLine+1,adj=0,at=at.col[1])
		return()
	}
		
	q = qcc(data,type="xbar.one",plot=FALSE)
	std.dev <- q$std.dev
	lcl = q$limits[, "LCL"]
	ucl = q$limits[, "UCL"]
	violations <- q$violations
	
	
	
	# control limits
	xx = as.numeric(rownames(data))
	center = rep(q$center,length(xx))
	lines(x=xx,y=center)
	LCL = q$limits[, "LCL"]
	if(length(LCL)==1 && length(xx)!=1)
		LCL = rep(LCL,length(xx))
	lines(x=xx,y=LCL,lty=2,type="s",lwd=1)
	UCL = q$limits[, "UCL"]
	if(length(UCL)==1 && length(xx)!=1)
		UCL = rep(UCL,length(xx))
	lines(x=xx,y=UCL,lty=2,type="s",lwd=1)
	if(last)
	{
		mtext(side=4,line=.10,at=tail(center,1),text="CL",adj=0,cex=0.8)
		mtext(side=4,line=.10,at=tail(LCL,1),text="LCL",adj=0,cex=0.8)
		mtext(side=4,line=.10,at=tail(UCL,1),text="UCL",adj=0,cex=0.8)	
	}	
	
	# red dots
	for(i in 1:length(q$violations$beyond.limits))
	{
		xx = as.numeric(rownames(data)[q$violations$beyond.limits[i]])
		yy = data[q$violations$beyond.limits[i],]
		points(xx,yy,type="o",pch=16,cex=.9,col="red")
	}
	
	# orange dots
	for(i in 1:length(q$violations$violating.runs))
	{
		xx = as.numeric(rownames(data)[q$violations$violating.runs[i]])
		yy = data[q$violations$violating.runs[i],]
		points(xx,yy,type="o",pch=16,cex=.9,col="orange")
	}
	# vertical line (break-point)
	if(mylabel!='')
	{
		abline(v=as.numeric(rownames(data)[1])-.5,xpd=FALSE)
		mtext(side=3,text=mylabel,at=as.numeric(rownames(data)[1])+(as.numeric(tail(rownames(data),1))-as.numeric(rownames(data)[1]))/2,line=0,cex=.85,font=2)
	}
		
	
	
	
       # write info at bottom
       roundTo = 2
       startLine = 5*groupNum
       if(mylabel!="")
       {
       		mtext(paste(mylabel,":",sep=""), 
             side = 1, line = startLine, adj = 0, at=at.col[1],font=2)
       }       
       mtext(paste("Number of records = ", nrow(data), sep = ""), 
             side = 1, line = startLine+1, adj = 0, at=at.col[1])
       if (length(q$center) == 1)
          mtext(paste("Center = ", round(signif(q$center, options()$digits),roundTo), sep = ""),
                side = 1, line = startLine+2, adj = 0, at=at.col[1])
       else 
          mtext("Center is variable", 
                side = 1, line = startLine+2, adj = 0, at=at.col[1])
       mtext(paste("StdDev = ", round(signif(std.dev, options()$digits),roundTo), sep = ""),
             side = 1, line = startLine+3, adj = 0, at=at.col[1])
       target <- q$target
       if (!is.null(target))
          { if (length(target) == 1)
                mtext(paste("Target = ", signif(target, options()$digits), 
                            sep = ""),
                      side = 1, line = startLine+1, adj = 0, at=at.col[2]
                     )
            else 
                mtext("Target is variable", 
                      side = 1, line = startLine+1, adj = 0, at=at.col[2])
          }
       if (length(unique(lcl)) == 1)
          mtext(paste("LCL = ", round(signif(lcl, options()$digits),roundTo), sep = ""), 
                side = 1, line = startLine+2, adj = 0, at=at.col[2])
       else 
          mtext("LCL is variable", side = 1, line = startLine+2, adj = 0, at=at.col[2])
       if (length(unique(ucl)) == 1)
          mtext(paste("UCL = ", round(signif(ucl, options()$digits),roundTo), sep = ""),
                side = 1, line = startLine+3, adj = 0, at=at.col[2])
       else 
          mtext("UCL is variable", side = 1, line = startLine+3, adj = 0, at=at.col[2])
       if (!is.null(violations))
          { mtext(paste("Number beyond limits =",
                        length(unique(violations$beyond.limits))), 
                  side = 1, line = startLine+2, adj = 0, at = at.col[3])
            mtext(paste("Number violating runs =",
                        length(unique(violations$violating.runs))), 
                  side = 1, line = startLine+3, adj = 0, at = at.col[3])
           }
     

}
xbarI_section = function(rdata,USER,columnOfInterest,h=480,w=850,yaxis_label="Minutes of Life")
{
	mylabel = label(rdata[,columnOfInterest])
	
	# all data where columnOfInterest is not missing
	alldata = subset(rdata,!is.na(rdata[,columnOfInterest]) ,select=c("clinic","study_id",columnOfInterest,"dob_fake","record"))
	if(nrow(alldata)>0)
	{                              
		allplotdata = subset(alldata,select=c(columnOfInterest))
		if(USER=='state_user'){
			rownames(allplotdata)=NULL 
		}else{
			rownames(allplotdata) = alldata$record
		}				                                                               
		
		# plot 1
		png_filename1 = paste('xbarchart_',columnOfInterest,'.png',sep="")
		png(png_filename1,height=h,width=w)
			xbarI = xbarI_built_in_plot(allplotdata,yaxis_label)
			# add month labels
			alldata$order = seq(1:nrow(alldata))
			first_record_each_month = qcc.groups(alldata$order,format(as.Date(as.character(alldata$dob_fake),format="%m/%d/%y"),format="%Y-%m"))[,1]
			monthList = unique(format(as.Date(as.character(alldata$dob_fake),format="%m/%d/%y"),format="%b %Y"))
			axis(1,las=2,line=-3.5,at=first_record_each_month,labels=monthList,cex.axis=.8,lwd=0)
		dev.off()
	
		# plot 2
		# split data into 2 most recent months and 2 previous months
		# i.e. If today is 2/4/13, split data into Jan-Feb 2013 and Nov-Dec 2013
		plot2data = xbarI_plot2_formatData(rdata,columnOfInterest,USER)
		
		if(nrow(plot2data$qdata)>0)
		{                                    
			recentData = TRUE;
			png_filename2 = paste('xbarchart_',columnOfInterest,"_2.png",sep="")
			png(png_filename2,height=h,width=w)
				xbarI_built_in_plot2(data=plot2data$qdata1,data.name=plot2data$plotDividerLabels[1],newdata=plot2data$qdata2,newdata.name=plot2data$plotDividerLabels[2],cutoffMonth=plot2data$monthLabels[4],yaxis_label)  
				# add month labels
				allplot2data = plot2data$qdata
				allplot2data$order = seq(1:nrow(allplot2data))
				first_record_each_month = qcc.groups(allplot2data$order,format(as.Date(as.character(allplot2data$dob_fake),format="%m/%d/%y"),format="%Y-%m"))[,1]
				monthList = unique(format(as.Date(as.character(allplot2data$dob_fake),format="%m/%d/%y"),format="%b %Y"))
				axis(1,las=2,line=-2.3,at=first_record_each_month,labels=monthList,cex.axis=.8,lwd=0)                                 
			dev.off()
				
		}else
		{
			recentData = FALSE;
		}                          
		
		if(USER=='state_user')
		{
			
			cat("The following figures are statistical process control charts for ",mylabel,". Records are sorted by month/year of birth. Records with the same month/year of birth are then sorted by record and then center number if necessary. The first plot contains all collected data and displays the month of birth in the x-axis. The second plot has only the most recent data and has both the sequential record ID and the month of birth in the x-axis. Note that the assigned sequential record ID is not the same as the patient study ID. The sequential record ID is a sequentially assigned number for all records meeting a particular graph's selection criteria. For example, point 12 is the 12th patient in sequence for the specific parameters in a single graph.")
				
			
		}else
		{
			cat(paste("The following figures are statistical process control charts for ",mylabel,". Records are sorted by month/year of birth. Records with the same month/year of birth are then sorted by study ID. The first plot contains all collected data and displays the month of birth in the x-axis. The second plot has only the most recent data and has both the study ID and the month of birth in the x-axis. Note that study ID's may not be sequential since records are sorted by month/year of birth first and some records may be excluded.",sep=""))		
		}
		outlierTable(alldata,USER,columnOfInterest,mylabel,yaxis_label,xbarI)
		
		cat(paste("<center><img class='page-break-none' src='",png_filename1,"'></center><br>",sep=""))
		if(recentData)
		{
			cat(paste("<center><img class='page-break-none' src='",png_filename2,"'></center></li>",sep=""))
		}else
		{
			cat(paste("<li>Your center does not have any data for ",mylabel," since ",format(as.Date(as.character(tail(alldata,1)$dob_fake,format="%m/%d/%y"),format="%Y-%m")),".</li>",sep=""))
		}
		
    # DISABLED BECAUSE A DIFFERENT FUNCTION IS USED (mchart) THAT ALLOWS FOR MANUALLY ADDING SHIFTS
		# add mean xbar plot if state user
		#if(USER=='state_user')
		#{
		#	png_filename3 = paste('xbarchart_',columnOfInterest,"_mean",sep="")
		#	png(png_filename3,height=h,width=w)
		#		xbarI_mean_plot(rdata,columnOfInterest,yaxis_label)
		#	dev.off()
		#	cat("<br><li>The following figure is an xbar mean plot grouped by the infant's month of birth. This plot is only displayed in the statewide report. Control limits are calculated using the following formula: CL &plusmn; 3*SD. Sample SD's vary according to sample size.</li>")
		#	cat(paste("<br><center><img class='page-break-none' src='",png_filename3,"'></center></li>",sep=""))
		#}
	}else
	{
		cat(paste("Your center does not have any data for ",mylabel,"</li>",sep=""))
	}
}


 
xbarI_built_in_plot = function(allplotdata,yaxis_label)
{
	q = qcc(allplotdata,type="xbar.one",stats=TRUE,chart.all=TRUE,plot=FALSE,xaxt="n",labels=rep("",nrow(allplotdata)))
	
	# round center, std dev, limits
	q$center = round(q$center,2)
	q$std.dev = round(q$std.dev,2)
	q$limits = round(q$limits,2)
	
	title = label(allplotdata)
	
	if(yaxis_label=='mmHg'){
		my_ylim = c(0,120)
	}else{
		my_ylim = c(0,200)
	}
	
	plot(q,add.stats=TRUE,chart.all=TRUE,title=paste(title," - All Collected Data",sep=""),xlab="",ylab=yaxis_label,ylim=my_ylim)
	
	# Restore default clipping rect
	par(mar=c(5, 4, 4, 2) + 0.1)	
	
	return(q)
}
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

	# round center, std dev, limits
	q$center = round(q$center,2)
	q$std.dev = round(q$std.dev,2)
	q$limits = round(q$limits,2)
	
	title = label(data)
	combined_data = rbind(data,newdata)
	if(yaxis_label=='mmHg'){
		my_ylim = c(0,120)
	}else{
		my_ylim = c(0,200)
	}
	plot(q,add.stats=TRUE,chart.all=TRUE,title=paste(title," - Data From ",format(cutoffMonth,format="%b %Y")," to Present",sep=""),cex.axis=0.8,las.axis=2,xlab="",ylab=yaxis_label,ylim=my_ylim)
	#axis(1,at=seq(1:nrow(combined_data)),labels=rownames(combined_data),cex.axis=.8,las=2,line=0)
	
	# Restore default clipping rect
	par(mar=c(5, 4, 4, 2) + 0.1)	
}
xbarI_plot2_formatData = function(rdata,columnOfInterest,USER) 
{
	# plot 2
	# split data into 2 most recent months and 2 previous months
	# i.e. If today is 2/4/13, split data into Jan-Feb 2013 and Nov-Dec 2013
	today = Sys.Date()
	thisMonth = format(today,format="%m/01/%Y")
	monthLabels = seq(as.Date(thisMonth,format="%m/%d/%Y"),length=4,by="-1 months")
	plotDividerLabels = c()
	
	if(format(monthLabels[4],format="%Y")==format(monthLabels[3],format="%Y")){
		plotDividerLabels[1] = paste(format(monthLabels[4],format="%b"),format(monthLabels[3],format="%b %Y"),sep=" - ")
	}else{
		plotDividerLabels[1] = paste(format(monthLabels[4],format="%b %Y"),format(monthLabels[3],format="%b %Y"),sep=" - ")
	}
	if(format(monthLabels[2],format="%Y")==format(monthLabels[1],format="%Y")) {
		plotDividerLabels[2] = paste(format(monthLabels[2],format="%b"),format(today,format="%b %d, %Y"),sep=" - ")
	}else{
		plotDividerLabels[2] = paste(format(monthLabels[2],format="%b %Y"),format(today,format="%b %d, %Y"),sep=" - ")
	}

	
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
xbarI_mean_plot = function(rdata,columnOfInterest,yaxis_label)
{
	my_subset = subset(rdata,!is.na(rdata[,columnOfInterest]) & !is.na(rdata$month),select=c(columnOfInterest,'month'))
	colnames(my_subset)=c(columnOfInterest,'gmonth')
	monthList = unique(my_subset$gmonth)
	monthKey = rbind(seq(1:length(monthList)))
	colnames(monthKey)=monthList
	my_subset$monthKey = monthKey[,my_subset$gmonth]
	
	my_subset_groups = qcc.groups(my_subset[,columnOfInterest],my_subset$monthKey)
	rownames(my_subset_groups) = monthList
	# exclude rows where group size is <2
	my_subset_groups = my_subset_groups[which(rowSums(!is.na(my_subset_groups))>1),]
	q = qcc(my_subset_groups,type='xbar')
	q$center = round(q$center,2)
	q$std.dev = round(q$std.dev,2)
	my_title = paste(label(rdata[,columnOfInterest])," - Monthly Means",sep="")
	plot(q,title=my_title,xlab="Month",ylab=yaxis_label)
  return(q)
}
# not used as of 4/2/13
pchartWithBars = function(rdata,columnOfInterest,h=480,w=850)
{
	monthList = unique(rdata$month)
	percent_complete = c()
	percent_yes = c()
	denomList = c()
	plotData = data.frame()
	# pull out data with NA for month
	rdata=subset(rdata,!is.na(rdata$month))
	
	for(month in 1:length(monthList))
	{
		# number completed
		num_complete_tmp = sum(!is.na(rdata[,columnOfInterest][rdata$month==monthList[month]])) 
		denom_complete_tmp = sum(rdata$month==monthList[month])
		
		# number answered "Yes"
		num_yes_tmp = sum(rdata[,columnOfInterest][rdata$month==monthList[month]]=="Yes",na.rm=TRUE) 
		denom_yes_tmp = num_complete_tmp
		
		percent_complete = c(percent_complete,round(num_complete_tmp/denom_complete_tmp*100,1))
		percent_yes = c(percent_yes,round(num_yes_tmp/denom_yes_tmp*100,1))
		denomList = c(denomList,denom_complete_tmp)
		
		plotData[1,month] = round((denom_yes_tmp-num_yes_tmp)/denom_complete_tmp*100,1)
		plotData[2,month] = round(num_yes_tmp/denom_complete_tmp*100,1)
		plotData[3,month] = round(sum(is.na(rdata[,columnOfInterest][rdata$month==monthList[month]])) /denom_complete_tmp*100,1)		
	}
		colnames(plotData) = monthList
		rownames(plotData) = c("No","Yes","NA")
    
	pngFileName = paste('pchartWithBars_',columnOfInterest,".png",sep="")
	
	png(pngFileName,height=h,width=w)
		# Expand right side of clipping rect to make room for the legend
		par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,0,10))
		b = barplot(percent_complete,border=NA,col="#0000ff22",xlab="Month",ylab="Percent",main=label(rdata[,columnOfInterest]),xlim=c(0,100))
        lines(x=b,y=percent_yes)
        legend("topright",c("Data % Completion","% 'Yes' of Completed"),bty="n",col=c("#0000ff22","black"),pch=c(15,-1),lty = c(0,1))
    dev.off()
    
    # Restore default clipping rect
    par(mar=c(5, 4, 4, 2) + 0.1)
    
    cat(paste("<br><br><center><img src='",pngFileName,"'></center>",sep=""))
    
}

stackedBarChart = function(rdata,columnOfInterest,chartTitle)
{
	tempData = subset(rdata,select=c("month",columnOfInterest))
	tempData_notNA = subset(tempData,!is.na(tempData[,columnOfInterest]))
	#monthList = unique(rdata$month[!is.na(rdata$month)])
	months = seq(as.Date("2012-09-01"), Sys.Date(), by="1 month")
	monthList = format(months,"%m/%Y")
	denomList = c()

	plotData = data.frame();
	for(month in 1:length(monthList))
	{
			denominator = sum(tempData$month==monthList[month])
			
			# NO
			plotData[1,month] = sum(tempData_notNA[,2][tempData_notNA$month==monthList[month]]=="No") / denominator*100
			# YES 
			plotData[2,month] = sum(tempData_notNA[,2][tempData_notNA$month==monthList[month]]=="Yes") / denominator*100
			# NA
			plotData[3,month] = sum(is.na(tempData[,2][tempData$month==monthList[month]])) / denominator*100

			denomList = c(denomList,denominator)
	}
	colnames(plotData) = monthList
	rownames(plotData) = c("No","Yes","N/A")

	h = 450					
	w = 1000
	# start writing png
	pngFileName = paste('barchart_',columnOfInterest,'.png',sep="")
	png(pngFileName,height=h,width=w)
	par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,5,12))

	barPlotData = as.matrix(plotData)

	b = barplot(barPlotData,
			  main=chartTitle,
			  col=c("red","green","purple"),
			  xlab = "Month",
			  ylab = "Percent"							  
	 )
	 box(lty=1,col='black')
	 #legendpos = ((tail(b,2)[2]-tail(b,2)[1]))*length(b)/3.5*-1
	 legend("topright",inset=c(-.15,0),rev(rownames(plotData)),fill=rev(c("red","green","purple")))
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
			for(month in 1:length(monthList))
			{
					clinicCount = c(clinicCount,length(unique(rdata$clinic[rdata$month==monthList[month]])))
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
			cat("Congratulations, there are no outliers (cases where the ",tolower(yaxis_label),my_outliertable_condition_text,") in the data for your center.<br>",sep="")
		}

		cat("<br>")
	}
	
}

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
					

mchart <- function(data,column,USER,Ylim=c(0,100))
{
  cat("<br><li>The following figure is an xbar mean plot grouped by the infant's month of birth. This plot is only displayed in the statewide report. Control limits are calculated using the following formula: CL &plusmn; 3*SD. Sample SD's vary according to sample size.</li>")
  title = label(data[,column])
  
  # Expand right side of clipping rect to make room for the legend
  par(xpd=T, mar=c(5, 4, 4, 2) + 0.1+c(0,7,0,7))
  
  data_remove_nas = subset(data,!is.na(data[,column]))
  
  monthList = format.Date(seq.Date(from = as.Date("2012-05-01"),to = Sys.Date(), by = "month"), "%m/%Y")
  
  # get monthly means
  monthly_means = aggregate(data_remove_nas[,column],list(month=data_remove_nas$month),mean)
  monthly_counts = aggregate(data_remove_nas[,column],list(month=data_remove_nas$month),length)
  dataper <- data.frame(month = monthList, stringsAsFactors = FALSE)
  dataper$month <- factor(dataper$month, levels = unique(dataper$month),
                          ordered = TRUE)
  plotdata = merge(dataper, monthly_means, by = "month", all = TRUE)
  plotdata = merge(plotdata, monthly_counts, by = "month", all = TRUE)
  plotdata = plotdata[order(plotdata$month),]
  colnames(plotdata) = c("month","y","count")
  rownames(plotdata) = NULL
  
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
  title(ylab= "Minutes of Life")
  # Create a title with a bold font
  title(main=paste(title," - Monthly Means",sep=""), font.main=2)
  
  # exclude rows where group size is <2
  #plotdata = subset(plotdata,plotdata$count>1)
  # add Pilot date and kickoff date markers
  #addPilotAndKickoffDates(monthList)  		
  ## pilot date (5/1/2012-8/31/2012)
  #pilotToKickoffqcc = addShift(points=plotdata$y,groupSizes=plotdata$count,monthList,"2012-05-01","2012-09-01","xbar",data_remove_nas,column)
  
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






### scratch paper
pchart2<-function(rdata,columnList=c("month","fio2_5","sao2_5"),conditionList=cbind(rdata$fio2_5>.21 & !is.na(rdata$fio2_5) & rdata$sao2_5>=80 & rdata$sao2_5<=85 & !is.na(rdata$sao2_5),!is.na(rdata$sao2_5)),title,option,USER)
{
	monthList = unique(rdata$month)
	mydata = matrix(0,length(monthList),ncol(conditionList))
	for(condition in 1:ncol(conditionList))
	{
		for(month in 1:length(monthList))
		{
			sub1 = rdata[conditionList[,condition],columnList]
			sub2 = sub1[sub1$month==monthList[month],]
			numerator = nrow(sub2)
			denominator = nrow(rdata[rdata$fio2>.21 & rdata$month==monthList[month],columnList])
			mydata[month,condition] = numerator/denominator*100
		}
	}
}


