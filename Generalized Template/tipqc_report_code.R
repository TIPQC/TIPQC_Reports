


## Probably Turn in to "PBP Section" Function ##

# get pbps
pbps_list = gsub("_how_audit","",colnames(data)[grep("_how_audit",colnames(data))])



# pbps key
pbp_key <<- data.frame(pbps_list)
for(i in 1:length(pbps_list)){
  pbp_key[i,'label'] = label(data[,pbps_list[i]])
  # order by the number in () in the label of the data column
  pbp_key[i,'order'] = gsub("[\\(\\)]","",regmatches(label(data[,pbps_list[i]]),gregexpr("^\\(.*?\\)",label(data[,pbps_list[i]]))))
}
pbp_key=pbp_key[with(pbp_key,order(as.numeric(order))),]
pbps_list <<- as.vector(pbp_key$pbps_list)

# define data subset with only pbps and a few necessary variables
pbp_subset_all = subset(data)

# months to be included in charts
months = seq(PILOT_DATE, Sys.Date(), by="1 month")
monthListFromPilot <<- format(months,"%m/%y")

# delete any data outside of the date range we're interested in
pbp_subset_all = pbp_subset_all[(pbp_subset_all$month %in% monthListFromPilot),]

# only pbps only subset
pbp_subset = subset(pbp_subset_all,select=c("month","year","clinic",pbps_list))

# audited table
# audit num if any - if yes/in progress AND audit=yes AND nonmissing audit num given
audit_list=paste(pbps_list,"_audit",sep="")
audit_subset = as.matrix(pbp_subset_all)
for(pbp in pbps_list){
  audit_subset[!(audit_subset[,pbp] %in% c("Yes","In progress")),pbp] = " "
  audit_subset[is.na(audit_subset[,pbp]),pbp] = " "
  audit_colname = paste(pbp,"_audit",sep="")  
  audit_subset[audit_subset[audit_subset[,audit_colname] %in% c("No") | is.na(audit_subset[,audit_colname]),"record_id"]  , pbp] = " "
  audit_subset[audit_subset[is.na(audit_subset[,paste(pbp,"_no_audit",sep="")]) & audit_subset[,audit_colname] %in% c("Yes"),"record_id"] , pbp] = "<span style='color:red;font-size:7pt;'>missing</span>"
  audit_subset[audit_subset[!(is.na(audit_subset[,paste(pbp,"_no_audit",sep="")])),"record_id"] , pbp] = audit_subset[audit_subset[!(is.na(audit_subset[,paste(pbp,"_no_audit",sep="")])),"record_id"], paste(pbp,"_no_audit",sep="")]
}
tmp_table = audit_subset[,pbps_list]
colnames(tmp_table) = pbp_key$order
rownames(tmp_table) = audit_subset[,"month"]
tmp_table=t(tmp_table)
audit_table = as.numeric(cbind(rownames(tmp_table)))
audit_table = order_months_as_columns(tmp_table,audit_table,nodata=" ")
rownames(audit_table) = rownames(tmp_table)
colnames(audit_table) = c("PBP",monthListFromPilot)






###############################
###############################
###############################
existing_pbps = as.data.frame(matrix(ncol=7))
colnames(existing_pbps) = c("month","pbp_activity","audit","num_audited","num_compliant","clinic","pbp")
for(pbp in pbps_list){
  audit = paste(pbp,"_audit",sep="")
  num_audited = paste(pbp,"_no_audit",sep="")
  num_compliant = paste(pbp,"_no_compliant",sep="")
  tmp = subset(data,data[,pbp]=="Yes" | data[,pbp]=="In progress")
  subsetOfColumns = cbind(as.data.frame(as.matrix(tmp[,c("month",pbp,audit,num_audited,num_compliant,"clinic")])),pbp)
  colnames(subsetOfColumns) <- colnames(existing_pbps) 
  existing_pbps = rbind(existing_pbps,subsetOfColumns)
}
existing_pbps = existing_pbps[-1,]
colnames(existing_pbps) = c("month","pbp_activity","audit","num_audited","num_compliant","clinic","pbp")
rownames(existing_pbps)=NULL
existing_pbps = data.frame(existing_pbps)
existing_pbps$pbpactivity_audit = gsub("NA","Blank",paste(existing_pbps$pbp_activity,"/ Audit =",existing_pbps$audit))
###############################
###############################
###############################










# compliance table
compliance_subset = audit_subset
for(pbp in pbps_list){
  percent_compliant_colname = paste(pbp,"_percent_compliant",sep="")
  no_compliant_colname = paste(pbp,"_no_compliant",sep="")
  no_audit_colname = paste(pbp,"_no_audit",sep="")
  audit_colname = paste(pbp,"_audit",sep="") 
  compliance_subset = cbind(compliance_subset,as.numeric(compliance_subset[,no_compliant_colname]) / as.numeric(compliance_subset[,no_audit_colname]))
  colnames(compliance_subset) = c(colnames(compliance_subset)[1:length(colnames(compliance_subset))-1],percent_compliant_colname)
  # mark instances where audit was performed, but no compliance data given
  compliance_subset[compliance_subset[,audit_colname %in% c("Yes") & is.na(no_compliant_colname)],percent_compliant_colname] = "missing"
}
tmp_table = compliance_subset[,paste(pbps_list,"_percent_compliant",sep="")]
colnames(tmp_table) = pbp_key$order
rownames(tmp_table) = compliance_subset[,"month"]
tmp_table=t(tmp_table)
compliance_table = c()
compliance_table = order_months_as_columns(tmp_table,compliance_table,nodata="NA")
rownames(compliance_table) = rownames(tmp_table)
colnames(compliance_table) = c(monthListFromPilot)


# one chart showing Activity Of All 10 PBPs
allpbps = matrix(ncol=5)
for(pbp_num in pbp_key$order){
  pbp = as.character(pbp_key[pbp_num,"pbps_list"])
  # month, pbp activity, clinic, pbp num, month key
  tmp = cbind(pbp_subset[,c("month",pbp,"clinic")],rep(pbp_num,nrow(pbp_subset)),match(pbp_subset$month,monthListFromPilot))
  allpbps = rbind(allpbps,as.matrix(tmp))
}
allpbps = allpbps[-1,]
colnames(allpbps)=c("month","pbp","clinic","pbp_num","month_key")
rownames(allpbps)=NULL
allpbps = data.frame(allpbps)
allpbps_y_inprog = allpbps[allpbps$pbp %in% c("Yes","In progress"),]


checkmark = '&#x2713;'

# begin charts
if(USER=="state_user"){
  # one percentage chart showing Activity Of All PBPs
  chartTitle = paste("Activity of All ",length(pbps_list)," PBPs",sep="")
  cat(paste("<ul><li class='subsection'><span class='header'>Aggregate PBP Activity</span> <p>Following is a stacked bar chart illustrating the activity of all PBPs for all participating clinics over time. This chart only shows the <i>activity</i> of the implementation of PBPs (Yes, In progress, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. That is, if a single center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,categories=c("No","In progress","Yes"),colors=c("red","cyan","green"))  
  cat("<br>")
  # sunflower plot
  createSunFlowerPlot(allpbps_y_inprog[,c("month_key","pbp_num")])
  # stacked bar chart for each PBP
  cat(paste("</li><li class='subsection'><span class='header'>Activity of Each PBP</span> <p>Following are stacked bar charts illustrating the activity of each PBP for all participating clinics over time. These charts only show the <i>activity</i> of the implementation of PBPs (In progress, Yes, No, or Blank (missing data)). All data for each PBP is included regardless of whether or not the PBP was audited that month. However, duplicate records are excluded. That is, if a single center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. The table after each bar chart illustrates which centers entered 'Yes' or 'In progress' for the activity of each PBP for each month.</p>"))
  # if state_user, show a percentage plot and table of Yes/Inprogress for each pbp
  for(pbp in pbps_list){
    stackedBarChart(pbp_subset,pbp,label(data[,pbp]),categories=c("No","Yes","In progress"),colors=c("red","green","cyan"),include.totalrecords=FALSE)
    createCheckMarkTable(rdata=subset(pbp_subset,pbp_subset[,pbp] %in% c("Yes","In progress")),yaxis="clinic",col.label="Center:")
  }
  cat("</li>")
  ###############################
  ###############################
  ###############################
  # compliant pbps
  
  clinicList = unique(existing_pbps$clinic)
  clinicList = sort(clinicList)
  bubbleChartColors = rainbow(length(clinicList))
  for(i in 1:length(clinicList)){
    existing_pbps[existing_pbps$clinic==clinicList[i],"color"]=bubbleChartColors[i]
  }
  group.colors = rainbow(length(clinicList))
  names(group.colors)=clinicList
 
  #remove NAs
  existing_pbps$month_key = match(existing_pbps$month,monthListFromPilot)  
  existing_pbps = subset(existing_pbps,with(existing_pbps,!is.na(month_key) & (!is.na(num_audited) & !is.na(num_compliant))))
  
  max_circle_size = 80
  max_num_audited = suppressWarnings(max(as.numeric(paste(existing_pbps$num_audited))[!is.na(as.numeric(paste(existing_pbps$num_audited)))]))
  sizing_element = max_circle_size / max_num_audited

  for(pbp_index in 1:length(pbps_list)){
    audited_pbps = subset(existing_pbps,(existing_pbps$audit=="Yes" & !is.na(existing_pbps$num_audited)) & (!is.na(existing_pbps$num_compliant) & existing_pbps$pbp==pbps_list[pbp_index]))
    # convert variables to numeric
    audited_pbps[,"num_audited"] = as.numeric(paste(audited_pbps[,"num_audited"]))
    audited_pbps[,"num_compliant"] = as.numeric(paste(audited_pbps[,"num_compliant"]))
    audited_pbps$perc_compliant = audited_pbps$num_compliant/audited_pbps$num_audited
    #audited_pbps$month_key = match(audited_pbps$month,monthListFromPilot)    
    # remove NA's
    #audited_pbps = subset(audited_pbps,with(audited_pbps,!is.na(month_key) & (!is.na(num_audited) & !is.na(num_compliant))))
    
  
      # bottom, left, top, right margins
      par(xpd=T, mar=c(5, 4, 4, 2) + c(0,7,0,12))
    
      audited_pbps$date = as.Date(gsub("/","/01/",audited_pbps$month),format="%m/%d/%y")
      #cat("min: ",min(audited_pbps$num_audited)*sizing_element,"<br>")
      #cat("max: ",max(audited_pbps$num_audited)*sizing_element,"<br>")  
    
      gg = ggplot(audited_pbps, aes(x=date, y=perc_compliant*1, size=num_audited, label=clinic, colour = factor(clinic)),legend=FALSE)+
      scale_size(range = c(min(audited_pbps$num_audited)*sizing_element, max(audited_pbps$num_audited)*sizing_element)) + 
      geom_point() +
      theme_bw(base_size=18) +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            title = element_text(size=14)) +
      scale_x_date(name="Month",breaks = date_breaks("months"),labels = date_format("%m/%y"),limits = c(as.Date("2013-1-1"), Sys.Date())) +
      scale_y_continuous(name="Compliance Rate", limits=c(0,1)) +
      scale_colour_manual(values=group.colors) + 
      labs(title=label(data[,pbps_list[pbp_index]]))
    assign(paste('bubblechart',pbp_index,sep=""),gg)
    assign(paste('bubbledata',pbp_index,sep=""),audited_pbps)
    pngFileName = paste('img/bubblechart_',pbp_index,'.png',sep="") 
    
    ggsave(gg,file=pngFileName,scale=1,height=6,width=12.5,dpi=72)
  }
  #library(gridExtra)
  png("img/bubble_chart.png",height=2000,width=800)  
  multiplot(bubblechart1, bubblechart2, bubblechart3, bubblechart4, bubblechart5,cols=1)
  dev.off()
  cat(paste("<div style='margin: auto; width: 1100px; padding-left: 250px;padding-bottom:50px;'><img src='img/bubble_chart.png'></div>",sep=""));
  
  png("img/bubble_chart2.png",height=2000,width=800)  
  multiplot(bubblechart6, bubblechart7, bubblechart8, bubblechart9,cols=1)
  dev.off()
  cat(paste("<div style='margin: auto; width: 1100px; padding-left: 250px;padding-bottom:50px;'><img src='img/bubble_chart2.png'></div>",sep=""));
  
  ###############################
  ###############################
  ###############################
}else{
  # one raw count chart showing Activity Of All PBPs
  chartTitle = paste("Activity of All ",length(pbps_list)," PBPs",sep="")
  cat(paste("<ul><li class='subsection'><span class='header'>PBP Activity</span> <p>Following is a stacked bar chart of counts illustrating the activity of all PBPs over time. This chart only shows the <i>activity</i> of PBPs (In progress, Yes, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. This means that if your center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. Please see the Report Summary section of this report for any duplicate records. The table below the graph indicates the activity state for each project PBP by month.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,type="count",ymax=10,categories=c("No","In progress","Yes"),colors=c("red","cyan","green"))  
  # table of Yes / In progress
  createCheckMarkTable(rdata=pbp_subset,yaxis="pbp",col.label="PBP:")  
  cat("</li>")
  # local - show raw count of audited and % compliant
  sectionTitle = paste("PBP Measurement")
  cat(paste("<li class='subsection'><span class='header'>",sectionTitle,"</span> <p>The following stacked bar chart is a breakdown of the Yes/In Progress activity of all PBPs, based on whether or not each PBP was audited that month. Blue shades indicate PBPs with an 'In progress' activity. Green shades indicate an activity of 'Yes'. The varying shades indicate whether or not the PBP was audited (Yes, No, or Blank). The following table indicates the number of observations (denominator) stored in REDCap for each PBP by month.  Note this only indicates the sample size during months when data were entered.  The numbers do <i>not</i> reflect performance or compliance with the PBP processes. PBPs with an activity of Yes/In progress that are indicated as having been audited for a given month, but don't have the number audited entered in REDCap are indicated in red as 'missing'.</p>"))
  audited_list = paste(pbps_list,"_audit",sep="")
  
  stackedBarChart(existing_pbps,"pbpactivity_audit","No. of Audited PBPs with Activity of Yes/In Progress",type="count",
    categories=c("In progress / Audit = No", "In progress / Audit = Yes","In progress / Audit = Blank","Yes / Audit = No","Yes / Audit = Yes","Yes / Audit = Blank"),
    colors=c("blue","cyan","lightblue","darkgreen","green","lightgreen"),ymax=10,include.na=FALSE)
  writeHTMLtable(audit_table,col.label="PBP:",legend="Numbers indicate # audited. 'Missing' means that PBP was audited, but # audited is missing in REDCap.",include.colnames=FALSE)
  cat("</li>")
  
  
  # percent compliant
  sectionTitle = paste("PBP Audit Compliance")
  cat(paste("<li class='subsection'><span class='header'>",sectionTitle,"</span> <p>The following 'heat' map visualizes the performance or compliance rate of measured PBPs over time.  States of higher performance or compliance are represented by brighter green as noted in the scale to the right.  Performance or compliance is calculated as numerator divided by denominator using data entered in REDCap.  Note all PBPs are represented, but only PBPs with data for a given month are color-coded.  Colorless PBP:months indicate only that no data was entered for that month in REDCap.</p>"))
  compliance_matrix = t(data.matrix(compliance_table))
  h = 450  				
  w = 1000
  # start writing png
  pngFileName = paste('img/heatmap.png',sep="")
  png(pngFileName,height=h,width=w)
    color_gradient = c("#002BE5","#0032DD","#003AD6","#0042CF","#004AC8","#0051C1","#0059BA","#0061B2","#0069AB","#0071A4","#00789D","#008096","#00888F","#009087","#009880","#009F79","#00A772","#00AF6B","#00B764","#00BF5D")
    levelplot(compliance_matrix,col.regions=color_gradient,xlab="Month",ylab="PBP",main="Compliance of Audited PBPs")
  dev.off()
  cat(paste("<div style='margin:auto;width:1100px;'><div style='width: 1000px; padding-left: 97px;'><img src='",pngFileName,"'></div></div>",sep=""));
  cat("</li>")




  



}
