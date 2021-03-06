


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
  # one count chart showing Activity Of All PBPs
  chartTitle = paste("Activity Count of All ",length(pbps_list)," PBPs",sep="")
  cat(paste("<ul><li class='subsection'><span class='header'>Aggregate PBP Activity</span> <p>Following is a stacked bar chart illustrating the activity of all PBPs for all participating clinics over time. This chart only shows the <i>activity</i> of the implementation of PBPs (Yes, In progress, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. That is, if a single center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,categories=c("No","In progress","Yes"),colors=c("red","cyan","green"),type="count")  
  cat("<br>")
  # sunflower plot
  createSunFlowerPlot(allpbps_y_inprog[,c("month_key","pbp_num")])
  # stacked bar chart for each PBP
  cat(paste("</li><li class='subsection'><span class='header'>Activity of Each PBP</span> <p>Following are stacked bar charts illustrating the activity of each PBP for all participating clinics over time. These charts only show the <i>activity</i> of the implementation of PBPs (In progress, Yes, No, or Blank (missing data)). All data for each PBP is included regardless of whether or not the PBP was audited that month. However, duplicate records are excluded. That is, if a single center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. The table after each bar chart illustrates which centers entered 'Yes' or 'In progress' for the activity of each PBP for each month.</p>"))
  # if state_user, show a count plot and table of Yes/Inprogress for each pbp
  for(pbp in pbps_list){
    stackedBarChart(pbp_subset,pbp,label(data[,pbp]),categories=c("No","Yes","In progress"),colors=c("red","green","cyan"),include.totalrecords=FALSE,type="count")
    createCheckMarkTable(rdata=subset(pbp_subset,pbp_subset[,pbp] %in% c("Yes","In progress")),yaxis="clinic",col.label="Center:")
  }
  cat("</li>")
  ###############################
  ###############################
  ###############################
  # compliant pbps
 
  #remove NAs
  existing_pbps$month_key = match(existing_pbps$month,monthListFromPilot)  
  existing_pbps = subset(existing_pbps,with(existing_pbps,!is.na(month_key) & (!is.na(num_audited) & !is.na(num_compliant))))
  
  # convert variables to numeric
  existing_pbps[,"num_audited"] = as.numeric(paste(existing_pbps[,"num_audited"]))
  existing_pbps[,"num_compliant"] = as.numeric(paste(existing_pbps[,"num_compliant"]))
  existing_pbps[,"clinic"] = as.numeric(paste(existing_pbps[,"clinic"]))
  existing_pbps$perc_compliant = existing_pbps$num_compliant/existing_pbps$num_audited
  
  # clinic colors
  clinicList = unique(existing_pbps$clinic)
  clinicList = sort(clinicList)
  group.colors = rainbow(length(clinicList))
  names(group.colors)=clinicList
  
  #add date variable
  existing_pbps$date = as.Date(gsub("/","/01/",existing_pbps$month),format="%m/%d/%y")
  
  # determine bubble sizes
  max_circle_size = 40
  min_circle_size = 3
  min_num_audited = suppressWarnings(min(as.numeric(paste(existing_pbps$num_audited))[!is.na(as.numeric(paste(existing_pbps$num_audited)))]))
  max_num_audited = suppressWarnings(max(as.numeric(paste(existing_pbps$num_audited))[!is.na(as.numeric(paste(existing_pbps$num_audited)))]))
  legend_seq = ifelse(max_num_audited>250,50,25)
  
  # build legend
  legendPlot = ggplot(existing_pbps, aes(x=date, y=perc_compliant*1, size=num_audited, label=num_audited, colour = factor(clinic)),legend=FALSE)+
    scale_size("Number Audited: ", limits=c(min_num_audited,max_num_audited),range=c(min_circle_size,max_circle_size), breaks=c(1,seq(legend_seq,max_num_audited,by=legend_seq))) + 
    geom_point(na.rm=TRUE) +
    theme_bw(base_size=18) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          title = element_text(size=14),
          legend.position = "right",
          legend.box = "horizontal",
          legend.key = element_blank(),
          legend.background = element_rect(color = "black"),
          legend.margin = unit(x=c(0,1.5,0,0),units="in"),
          panel.margin = unit(x=c(0,1.5,0,0),units="in")) +
    scale_x_date(name="Month",breaks = date_breaks("months"),labels = date_format("%m/%y"),limits = c(as.Date("2013-1-1"), Sys.Date())) +
    scale_y_continuous(name="Compliance Rate", limits=c(0,1)) +
    scale_colour_manual("Center: ",values=group.colors) + 
    labs(title="Legend")
  
  png("img/bubble_legend.png",width=280,height=600)
    extract_legend(legendPlot)
  dev.off()
  
  cat(paste("<div class='page-break-before'></div"))
  cat(paste("<li class='subsection'><span class='header'>Status of Each PBP</span> <p>Following are bubble charts illustrating the compliance rate of all PBPs for all centers over time. The number of observations (denominator) stored in REDCap is indicated by the size of the bubble. Centers are differentiated by color. </p>"))
  cat(paste("<div style='width:1000px;margin:auto;'><div style='margin: auto; float:right;'><div style='text-align: center; font-family: Arial; font-size: 12pt;'>Bubble Chart Legend:</div><img src='img/bubble_legend.png'></div>",sep=""))

  # build bubble charts for each pbp
  for(pbp_index in 1:length(pbps_list)){
    audited_pbps = subset(existing_pbps,(existing_pbps$audit=="Yes" & !is.na(existing_pbps$num_audited)) & (!is.na(existing_pbps$num_compliant) & existing_pbps$pbp==pbps_list[pbp_index]))
    
    # bubble chart
    gg = ggplot(audited_pbps, aes(x=date, y=perc_compliant*1, size=num_audited, label=num_audited, colour = factor(clinic)),legend=FALSE)+
    scale_size("Number Audited",limits=c(min_num_audited,max_num_audited),range=c(min_circle_size,max_circle_size)) + 
    geom_point(na.rm=TRUE) +
    theme_bw(base_size=18) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          title = element_text(size=14),
          legend.position="none") +
    scale_x_date(name="Month",breaks = date_breaks("months"),labels = date_format("%m/%y"),limits = c(as.Date("2013-1-1"), Sys.Date())) +
    scale_y_continuous(name="Compliance Rate", limits=c(0,1)) +
    labs(title=label(data[,pbps_list[pbp_index]])) +
    scale_color_manual(guide="none",values=group.colors)
    
    assign(paste('bubblechart',pbp_index,sep=""),gg)
    
    assign(paste('bubbledata',pbp_index,sep=""),audited_pbps)
    pngFileName = paste('img/bubblechart_',pbp_index,'.png',sep="") 
    
    
        
    ggsave(gg,file=pngFileName,scale=1,height=4.6,width=10,dpi=72)
    
    # if 5th plot, add padding so legend doesn't fload "up"
    if(pbp_index==4 | pbp_index==8){
      cat(paste("<div style='float:left;padding-right:100px;'><img src='",pngFileName,"'></div>",sep=""))
    }else{
      cat(paste("<div style='float:left;'><img src='",pngFileName,"'></div>",sep=""))
    }
    
    # if 6th plot, print legend again
    if(pbp_index==5 | pbp_index==9){
      cat(paste("</div><div class='page-break-before' style='width:1000px;margin:auto;'><div style='margin: auto; float:right;'><div style='text-align: center; font-family: Arial; font-size: 12pt;'>Bubble Chart Legend:</div><img src='img/bubble_legend.png'></div>",sep=""))
    }
  }  
  cat(paste("</div></li></ul>"))
  ###############################
  ###############################
  ###############################
}else{
  # one raw count chart showing Activity Of All PBPs
  chartTitle = paste("Activity Count of All ",length(pbps_list)," PBPs",sep="")
  cat(paste("<ul><li class='subsection'><span class='header'>PBP Activity - Counts</span> <p>Following is a stacked bar chart of counts illustrating the activity of all PBPs over time. This chart only shows the <i>activity</i> of PBPs (In progress, Yes, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. This means that if your center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. Please see the Report Summary section of this report for any duplicate records.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,type="count",ymax=10,categories=c("No","In progress","Yes"),colors=c("red","cyan","green"))  
  cat("</li>")
  
  # Heat Map of Activity Level
  allpbpsBlank = allpbps
  allpbpsBlank$pbp = factor(allpbps$pbp, levels=c("Blank","No","In progress","Yes"))
  allpbpsBlank[is.na(allpbpsBlank$pbp),"pbp"]="Blank"
  
  heatmap = ggplot(allpbpsBlank, aes(month, pbp_num)) + geom_tile(aes(fill = pbp), colour = "black")  +   
    scale_fill_manual("PBP Activity Level: ", values= c("purple","green","cyan","red"), limits=c("Blank","Yes","In progress","No")) +
    scale_x_discrete(name="Month", limits = monthListFromPilot) +
    scale_y_discrete(name="PBP", limits=pbp_key$order) +
    labs(title="PBP Activity Level") + 
    theme_bw(base_size=18) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          title = element_text(size=14),
          legend.position = "right",
          legend.key = element_blank(),
          legend.background = element_rect(color = "black"),
          plot.title = element_text(vjust=4,face="bold"),
          plot.margin=unit(x=c(1,0,0,0),units="in"))
  
  pngFileName = "img/heatmap.png";
  ggsave(heatmap,file=pngFileName,scale=1,height=5.5,width=13,dpi=72)
  cat(paste("<li class='subsection'><span class='header'>PBP Activity - Heat Map</span> <p>Following is a heat map illustrating the activity of all PBPs over time. This chart only shows the <i>activity</i> of PBPs (In progress, Yes, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. This means that if your center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. Please see the Report Summary section of this report for any duplicate records.</p>"))
  cat(paste("<div style='margin:auto;width:1100px;'><div style='width: 1000px; padding-left: 180px;'><img src='",pngFileName,"'></div></div>",sep=""));
  cat("</li>")
  
  
  
  
  
  
  




  



}
