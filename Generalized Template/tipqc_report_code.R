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
data = data[order(as.numeric(data$year),as.numeric(data$month)),]
data$month = paste(data$month,substr(data$year, 3, 4),sep="/")

# index data by record id
rownames(data)=data$record_id


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

# delete any (both) duplicates where month and clinic are the same
pbp_subset_all$key = paste(pbp_subset_all$month,pbp_subset_all$clinic,sep="")
duplicate_keys = unique(pbp_subset_all[duplicated(pbp_subset_all$key),"key"])
pbp_subset_all = pbp_subset_all[!(pbp_subset_all$key %in% duplicate_keys),]

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
audit_table = order_months_as_columns(tmp_table,audit_table)
rownames(audit_table) = rownames(tmp_table)
colnames(audit_table) = c("PBP",monthListFromPilot)



# one chart showing Status Of All 10 PBPs
allpbps = matrix(ncol=5)
for(pbp_num in pbp_key$order){
  pbp = as.character(pbp_key[pbp_num,"pbps_list"])
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
  # if state_user, show a percentage plot and table of Yes/Inprogress for each pbp
  for(pbp in pbps_list){
    stackedBarChart(pbp_subset,pbp,label(data[,pbp]),categories=c("No","Yes","In progress"),colors=c("red","green","cyan"),include.totalrecords=FALSE)
    createCheckMarkTable(rdata=subset(pbp_subset,pbp_subset[,pbp] %in% c("Yes","In progress")),yaxis="clinic",col.label="Center:")
  }
  cat("</li>")
}else{
  # one raw count chart showing Status Of All PBPs
  chartTitle = paste("Status of All ",length(pbps_list)," PBPs")
  cat(paste("<ul><li class='subsection'><span class='header'>",chartTitle,"</span> <p>Following is a stacked bar chart of counts illustrating the status of all PBPs over time. This chart only shows the <i>status</i> of the implementation of PBPs (In progress, Yes, No, or Blank (missing data)). All data is included regardless of whether or not a PBP was audited that month. However, duplicate records are excluded. This means that if your center has more than one record for a given month, both records for that month are excluded from the data until the data entry error is corrected. Please see _____________________ for any duplicate records.</p>"))
  stackedBarChart(allpbps,"pbp",chartTitle,type="count",ymax=10,categories=c("No","Yes","In progress"),colors=c("red","green","cyan"))  
  # table of Yes / In progress
  createCheckMarkTable(rdata=pbp_subset,yaxis="pbp",col.label="PBP:")  
  cat("</li>")
  # local - show raw count of audited and % compliant
  sectionTitle = paste("Status of All ",length(pbps_list)," PBPs")
  cat(paste("<li class='subsection'><span class='header'>",sectionTitle,"</span> <p>The following stacked bar chart is a breakdown of the Yes/In Progress status of all PBPs, based on whether or not each PBP was audited that month. Blue shades indicate PBPs with an 'In progress' status. Green shades indicate a status of 'Yes'. The varying shades indicate whether or not the PBP was audited (Yes, No, or Blank). The table displays the number of records audited for each PBP each month. PBPs with a status of Yes/In progress that are indicated as having been audited for a given month, but don't have the number audited entered in REDCap are indicated in red as 'missing'.</p>"))
  audited_list = paste(pbps_list,"_audit",sep="")
  existing_pbps = matrix(ncol=6)
  for(pbp in pbps_list){
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
    categories=c("Yes / Audit = No","Yes / Audit = Yes","Yes / Audit = Blank","In progress / Audit = No", "In progress / Audit = Yes","In progress / Audit = Blank"),
    colors=c("darkgreen","green","lightgreen","blue","cyan","lightblue"),ymax=10,include.na=FALSE)
  writeHTMLtable(audit_table,col.label="PBP:",legend="Numbers indicate # audited. 'Missing' means that PBP was audited, but # audited is missing in REDCap.",include.colnames=FALSE)
  cat("</li>")
  
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
