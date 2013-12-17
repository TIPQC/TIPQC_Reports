# GLOBAL variables
PILOT_DATE <<- as.Date("2012-09-01")
USER <<- USER

# define data according to user
if(USER == "state_user") {
  # Exclude Lake WBG data!
  hospdata <- subset(data, record_id %nin% grep("^82-", data$record_id, value = TRUE))
}else{
  hospdata <- subset(data, record_id %in% grep(paste("^", USER_GROUP_ID, "-", sep = ""), data$record_id, value = TRUE))
}
data=hospdata

# add clinic variable
data$clinic = as.numeric(gsub("-[0-9a-zA-Z]+","",data$record_id))

# add month variable
data$month = gsub("[\\(\\)]","",regmatches(data$month,gregexpr("\\(.*?\\)",data$month)))
data$month = paste(data$month,substr(data$year, 3, 4),sep="/")


## Probably Turn in to "PBP Section" Function ##

# get pbps
pbps_list = gsub("_how_audit","",colnames(data)[grep("_how_audit",colnames(data))])
pbps = data.frame(pbps_list)
for(i in 1:length(pbps_list)){
  pbps[i,'label'] = label(data[,pbps_list[i]])
  # order by the number in () in the label of the data column
  pbps[i,'order'] = gsub("[\\(\\)]","",regmatches(label(data[,pbps_list[i]]),gregexpr("^\\(.*?\\)",label(data[,pbps_list[i]]))))
}
pbps=pbps[with(pbps,order(as.numeric(order))),]
pbps_list <<- as.vector(pbps$pbps)

# define data subset with only pbps and a few necessary variables
pbp_subset = subset(data,select=c("month","year","clinic",pbps_list))

# delete any (both) duplicates where month and clinic are the same
pbp_subset$key = paste(pbp_subset$month,pbp_subset$clinic,sep="")
duplicate_keys = unique(pbp_subset[duplicated(pbp_subset$key),"key"])
pbp_subset = pbp_subset[!(pbp_subset$key %in% duplicate_keys),c("month","year","clinic",pbps_list)]

# months to be included in charts
months = seq(PILOT_DATE, Sys.Date(), by="1 month")
monthListFromPilot <<- format(months,"%m/%y")

# delete any data outside of the date range we're interested in
pbp_subset = pbp_subset[(pbp_subset$month %in% monthListFromPilot),]

# one chart showing Status Of All 10 PBPs
allpbps = matrix(ncol=5)
for(pbp in pbps$pbps){
  pbp_num = gsub("[\\(\\)]","",regmatches(label(data[,pbp]),gregexpr("^\\(.*?\\)",label(data[,pbp]))))
  # month, pbp status, clinic, pbp num, month key
  tmp = cbind(pbp_subset[,c("month",pbp,"clinic")],rep(pbp_num,nrow(pbp_subset)),match(pbp_subset$month,monthListFromPilot))
  allpbps = rbind(allpbps,as.matrix(tmp))
}
allpbps = allpbps[-1,]
colnames(allpbps)=c("month","pbp","clinic","pbp_num","month_key")
rownames(allpbps)=NULL
allpbps = data.frame(allpbps)
allpbps_y_inprog = allpbps[allpbps$pbp %in% c("Yes","In progress"),]
# rdata = allpbps
# columnOfInterest = "pbp"
# chartTitle = "Status Of All 10 PBPs"
# categories=c("No","Yes","In progress")
# colors=c("red","green","lightgreen")
# fromDate=PILOT_DATE
# include.na=TRUE


checkmark = '&#x2713;'

# begin charts
if(USER=="state_user"){
  # one percentage chart showing Status Of All PBPs
  chartTitle = paste("Status of All ",length(pbps_list)," PBPs")
  cat(paste("<ul><li class='subsection'><span class='header'>",chartTitle,"</span> <p>Following is a stacked bar chart illustrating the status of all PBPs for all participating clinics over time. This chart only shows the <i>status</i> of the implementation of PBPs (In progress, Yes, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. That is, if a single center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,categories=c("No","Yes","In progress"),colors=c("red","green","cyan"))  
  cat("<br>")
  # sunflower plot
  createSunFlowerPlot(allpbps_y_inprog[,c("month_key","pbp_num")])
  # stacked bar chart for each PBP
  cat(paste("</li><li class='subsection'><span class='header'>Status of Each PBP</span> <p>Following are stacked bar charts illustrating the status of each PBP for all participating clinics over time. These charts only show the <i>status</i> of the implementation of PBPs (In progress, Yes, No, or Blank (missing data)). All data for each PBP is included regardless of whether or not the PBP was audited that month. However, duplicate records are excluded. That is, if a single center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. The table after each bar chart illustrates which centers entered 'Yes' or 'In progress' for the status of each PBP for each month.</p>"))
  # if state_user, show a percentage plot for each pbp
  for(pbp in pbps$pbps){
    stackedBarChart(pbp_subset,pbp,label(data[,pbp]),categories=c("No","Yes","In progress"),colors=c("red","green","cyan"),include.totalrecords=FALSE)
    createCheckMarkTable(rdata=subset(pbp_subset,pbp_subset[,pbp] %in% c("Yes","In progress")),yaxis="clinic",col.label="Center:")
  }
  cat("</li>")
}else{
  # one raw count chart showing Status Of All PBPs
  chartTitle = paste("Status of All ",length(pbps_list)," PBPs")
  cat(paste("<ul><li class='subsection'><span class='header'>",chartTitle,"</span> <p>Following is a stacked bar chart of counts illustrating the status of all PBPs over time. This chart only shows the <i>status</i> of the implementation of PBPs (In progress, Yes, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. This means that if your center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. Please see _____________________ for any duplicate records.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,type="count",ymax=10,categories=c("No","Yes","In progress"),colors=c("red","green","cyan"))
  
  
  ####table
  tmp_table = writeTmpTable(pbp_subset)
  createCheckMarkTable(rdata=pbp_subset,tmp_table=tmp_table,yaxis="pbp",col.label="PBP:")
  
#   my_table = as.numeric(cbind(rownames(tmp_table)))
#   for(month in monthListFromPilot[1:length(monthListFromPilot)])
#   {
#     if(month %in% colnames(tmp_table)){
#       my_table = cbind(my_table,tmp_table[,month])
#     }else{
#       my_table = cbind(my_table,rep(0,nrow(tmp_table)))
#     }      
#   }
#   my_table = data.frame(my_table,stringsAsFactors=FALSE)
#   rownames(my_table) = rownames(tmp_table)
#   colnames(my_table) = c("pbp",monthListFromPilot)
#   my_table[,monthListFromPilot][my_table[,monthListFromPilot]==1] = checkmark
#   my_table[my_table==0] = " "
#   writeHTMLtable(my_table,col.label="PBP:")
  #####
  
  
  # local - show raw count of audited and % compliant
  audited_list = paste(pbps_list,"_audit",sep="")
  existing_pbps = matrix(ncol=6)
  for(pbp in pbps$pbps){
    audit = paste(pbp,"_audit",sep="")
    num_audited = paste(pbp,"_no_audit",sep="")
    num_compliant = paste(pbp,"_no_compliant",sep="")
    tmp = subset(data,data[,pbp]=="Yes" | data[,pbp]=="In progress")
    existing_pbps = rbind(existing_pbps,as.matrix(tmp[,c("month",pbp,audit,num_audited,num_compliant,"clinic")]))
  }
  existing_pbps = existing_pbps[-1,]
  colnames(existing_pbps) = c("month","pbp","audit","num_audited","num_compliant","clinic")
  rownames(existing_pbps)=NULL
  existing_pbps = data.frame(existing_pbps)
  existing_pbps$pbpstatus_audit = gsub("NA","Blank",paste(existing_pbps$pbp,"/ Audit =",existing_pbps$audit))
  stackedBarChart(existing_pbps,"pbpstatus_audit","No. of Audited PBPs with Status of Yes/In Progress",type="count",
    categories=c("In progress / Audit = No", "In progress / Audit = Yes","In progress / Audit = Blank","Yes / Audit = No","Yes / Audit = Yes","Yes / Audit = Blank"),
    colors=c("blue","cyan","lightblue","darkgreen","green","lightgreen"),ymax=10,include.na=FALSE)
  
  
  audited_pbps = subset(existing_pbps,existing_pbps$audit=="Yes" & !is.na(existing_pbps$num_audited))
  rownames(audited_pbps) = NULL
  audited_pbps$Yes = "Yes"
  audited_pbps$No = "No"
  # put data in format for barchart
  compliant_pbps = matrix(ncol=3)
  colnames(compliant_pbps) = c("month","compliant","clinic")
  audited_pbps_nonmissing_compliance = audited_pbps[!is.na(audited_pbps$num_compliant),]
  for(row in 1:nrow(audited_pbps_nonmissing_compliance)){   
    #compliant
    num_compliant = as.numeric(audited_pbps_nonmissing_compliance[row,"num_compliant"])
    if(num_compliant>0){
      for(num in 1:num_compliant){
        compliant_pbps = rbind(compliant_pbps,as.matrix(audited_pbps_nonmissing_compliance[row,c("month","Yes","clinic")]))
      }
    }   
    #not compliant
    noncompliant = as.numeric(as.character(audited_pbps_nonmissing_compliance[row,"num_audited"])) - as.numeric(as.character(audited_pbps_nonmissing_compliance[row,"num_compliant"]))
    if(noncompliant>0){
      for(num in 1:noncompliant){
        
        compliant_pbps = rbind(compliant_pbps,as.matrix(audited_pbps_nonmissing_compliance[row,c("month","No","clinic")]))
      }
    } 
    rm(num_compliant,noncompliant)
  }
  compliant_pbps = compliant_pbps[-1,]
  rownames(compliant_pbps)=NULL
  compliant_pbps = data.frame(compliant_pbps)
  stackedBarChart(compliant_pbps,"compliant","% Compliant of Audited PBPs",include.na=FALSE)
}
