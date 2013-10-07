# label data
label(data$study_id)="Study ID"
label(data$dob)="Babys DOB (MM/DD/YYYY)"
label(data$ega)="Estimated gestational age (EGA) at \nbirth (no. of completed weeks)"
label(data$pre_resusc_checklist)="Pre-resuscitation checklist completed?"
label(data$pre_resusc_briefing)="Pre-resuscitation briefing performed?"
label(data$apgar_1)="1 minute APGAR score"
label(data$apgar_5)="5 minute APGAR score"
label(data$apgar_10)="OPTIONAL: 10 minute APGAR score"
label(data$fio2_5)="FiO2 at 5 min (0.21 - 1.0)"
label(data$sao2_5)="SaO2 at 5 min (%)"
label(data$temp_5)="OPTIONAL: Temperature at 5 min (degrees C)"
label(data$post_resusc_briefing)="Post-resuscitation debriefing within \nGolden Hour performed?"
label(data$birthweight)="Birth Weight (grams)"
label(data$admit_temp)="Admission temperature (degrees C)"
label(data$surfactant_eligible)="Surfactant eligible by unit protocol?"
label(data$surfactant_received)="Surfactant received?"
label(data$iv_access_obtained)="IV access obtained?"
label(data$iv_access_age)="Age first access obtained (min of life)"
label(data$glucose_win60)="Glucose measurement obtained \nwithin 60 minutes?"
label(data$glucose_iv_started)="Glucose IV solution started?"
label(data$glucose_iv_started_age)="Age IV solution started (min of life)"
label(data$antibiotics_ordered)="Antibiotics ordered?"
label(data$antibiotics_started)="Age antibiotics started (min of life)"
label(data$resp_support_code)="Respiratory support code at 60 min \nof life"
label(data$pco2_indicated)="PCO2 on first blood gas indicated?"
label(data$pco2_value)="PCO2 on first blood gas (mmHg)"
label(data$parents_commun)="Communication with parents?"
label(data$discharge_date)="OPTIONAL: Date of discharge (MM/DD/YYYY)"
label(data$discharge_disp)="OPTIONAL: Disposition at discharge"
label(data$oxygen_required)="OPTIONAL: Oxygen required at 36 weeks CGA?"
label(data$ivh3_4pvl)="OPTIONAL: IVH 3 or 4/PVL?"
label(data$rop)="OPTIONAL: ROP requiring intervention?"
label(data$golden_hour_complete)="Complete?"

#############################################################################
# NOTE: "data" data frame object will already exist in the workspace image
#############################################################################

#############################################################################
# Pull out one hospital's data --> referenced using USER_GROUP_ID object
if(USER != "state_user") {
   hospdata <- subset(data, study_id %in% grep(paste("^", USER_GROUP_ID, "-", sep = ""),
      data$study_id, value = TRUE))
}
if(USER == "state_user") {
   # Exclude Lake WBG data!
   hospdata <- subset(data, study_id %nin% grep("^82-", data$study_id, value = TRUE))
}
       
thisreport <- Sys.Date()   # should be the date the report is requested

# add clinic using study_id
hospdata$clinic = as.numeric(gsub("-[0-9a-zA-Z]+", "", hospdata$study_id))

# add record using study_id
hospdata$record = as.numeric(gsub("[0-9a-zA-Z]+-", "", hospdata$study_id))

#order data by dob, record, clinic
# figure out if data is in Y-m-d format or m/d/y format
if(grep("-",data$dob[1]))
{
	myformat = "%Y-%m-%d"
}else
	myformat = "%m/%d/%y"
rdata = hospdata[with(hospdata,order(as.Date(as.character(dob),format=myformat),record,clinic)) , ]
# format rdata dob
rdata$dob = format(as.Date(as.character(rdata$dob),format=myformat),"%m/%d/%y")
label(rdata$dob)="Babys DOB (MM/DD/YYYY)"
rdata$discharge_date = format(as.Date(as.character(rdata$discharge_date),format=myformat),"%m/%d/%y")
label(rdata$discharge_date)="OPTIONAL: Date of discharge (MM/DD/YYYY)"



#add month column (calculated using dob)
colNum = ncol(rdata)+1
for(row in 1:nrow(rdata))
{
	rdata[row,colNum] = format(as.Date(as.character(rdata$dob[row]),format="%m/%d/%y"), format="%m/%Y")
}
#name month column
colnames(rdata)[colNum] = "month"
