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

# get pbpbs
pbps_list = gsub("_how_audit","",colnames(data)[grep("_how_audit",colnames(data))])
pbps = data.frame(pbps_list)
for(i in 1:length(pbps_list)){
  pbps[i,'label'] = label(data[,pbps_list[i]])
  # order by the number in () in the label of the data column
  pbps[i,'order'] = gsub("[\\(\\)]","",regmatches(label(data[,pbps_list[i]]),gregexpr("^\\(.*?\\)",label(data[,pbps_list[i]]))))
}
pbps=pbps[with(pbps,order(as.numeric(order))),]
pbps_list = as.vector(pbps$pbps)

# define data subset with only pbps and a few necessary variables
pbp_subset = subset(data,select=c("month","year","clinic",pbps_list))

# delete any (both) duplicates where month and clinic are the same
pbp_subset$key = paste(pbp_subset$month,pbp_subset$clinic,sep="")
duplicate_keys = unique(pbp_subset[duplicated(pbp_subset$key),"key"])
pbp_subset = pbp_subset[!(pbp_subset$key %in% duplicate_keys),c("month","year","clinic",pbps_list)]

# one chart showing Status Of All 10 PBPs
allpbps = matrix(ncol=3)
for(pbp in pbps$pbps){
  allpbps = rbind(allpbps,as.matrix(pbp_subset[,c("month",pbp,"clinic")]))
}
allpbps = allpbps[-1,]
colnames(allpbps)=c("month","pbp","clinic")
rownames(allpbps)=NULL
allpbps = data.frame(allpbps)
rdata = allpbps
columnOfInterest = "pbp"
chartTitle = "Status Of All 10 PBPs"
categories=c("No","Yes","In progress")
colors=c("red","green","lightgreen")
fromDate=PILOT_DATE
include.na=TRUE


# begin charts
if(USER=="state_user"){
  # one percentage chart showing Status Of All PBPs
  chartTitle = paste("Status of All ",length(pbps_list)," PBPs")
  stackedBarChart(allpbps,"pbp",chartTitle,categories=c("No","Yes","In progress"),colors=c("red","green","blue"))
  # if state_user, show a percentage plot for each pbp
  for(pbp in pbps$pbps){
    stackedBarChart(pbp_subset,pbp,label(data[,pbp]),categories=c("No","Yes","In progress"),colors=c("red","green","blue"))
    ##
    tmp_table = cbind(as.matrix(with(subset(pbp_subset,pbp_subset[,pbp] %in% c("Yes","In progress")),table(clinic,month))))
    months = seq(fromDate, Sys.Date(), by="1 month")
    monthListFromPilot = format(months,"%m/%y")
    my_table = tmp_table[,monthListFromPilot[1]]
    for(month in monthListFromPilot[2:length(monthListFromPilot)])
    {
      if(month %in% colnames(tmp_table)){
        my_table = cbind(my_table,tmp_table[,month])
      }else{
        my_table = cbind(my_table,rep(0,nrow(tmp_table)))
      }      
    }
    my_table = data.frame(my_table)
    rownames(my_table) = rownames(tmp_table)
    colnames(my_table) = monthListFromPilot
    ##
    ##
    ##
  }
}else{
  # one raw count chart showing Status Of All PBPs
  chartTitle = paste("Status of All ",length(pbps_list)," PBPs")
  stackedBarChart(allpbps,"pbp",chartTitle,type="count",ymax=10,categories=c("No","Yes","In progress"),colors=c("red","green","blue"))
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
  stackedBarChart(existing_pbps,"audit","No. of Audited PBPs within Existing/In Progress",type="count",ymax=10)
  
  audited_pbps = subset(existing_pbps,existing_pbps$audit=="Yes" & !is.na(existing_pbps$num_audited))
  rownames(audited_pbps) = NULL
  audited_pbps$Yes = "Yes"
  audited_pbps$No = "No"
  # put data in format for barchart
  compliant_pbps = matrix(ncol=3)
  colnames(compliant_pbps) = c("month","compliant","clinic")
  for(row in 1:nrow(audited_pbps)){   
    #compliant
    num_compliant = as.numeric(audited_pbps[row,"num_compliant"])
    if(num_compliant>0){
      for(num in 1:num_compliant){
        compliant_pbps = rbind(compliant_pbps,as.matrix(audited_pbps[row,c("month","Yes","clinic")]))
      }
    }   
    #not compliant
    noncompliant = as.numeric(as.character(audited_pbps[row,"num_audited"])) - as.numeric(as.character(audited_pbps[row,"num_compliant"]))
    if(noncompliant>0){
      for(num in 1:noncompliant){
        
        compliant_pbps = rbind(compliant_pbps,as.matrix(audited_pbps[row,c("month","No","clinic")]))
      }
    } 
    rm(num_compliant,noncompliant)
  }
  compliant_pbps = compliant_pbps[-1,]
  rownames(compliant_pbps)=NULL
  compliant_pbps = data.frame(compliant_pbps)
  stackedBarChart(compliant_pbps,"compliant","% Compliant of Audited PBPs",include.na=FALSE)
}
