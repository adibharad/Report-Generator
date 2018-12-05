# SCA Report Generator

# clearing workspace
#rm(list = ls())

# setting directory
setwd("/home/aditya/Desktop/R/SHAW SCA")


# saving data to the file 
sink(file = "SHAW_SCA.TXT", split = TRUE)

# getting rep_name from user
 AN <- readline(prompt = "Enter Agent Name: ")



# Loading Packages
#----------------------------------------------------------------------------------

library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("gdata", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("readr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("bindr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library("tidyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")


# Loading all the Files
#------------------------------------------------------------------------------------
B = read.csv("Payroll.csv", na.strings = c("","NA"))
C = read.csv("Rep_Key.csv", na.strings = c("","NA"))
D = read.csv("SCALog.csv", na.strings = c("","NA"))
E = read.csv("Coaching Data.csv", na.strings = c("","NA"))

Agent_Info <- B
Agent_Info <- Agent_Info %>%
select('ID', 'USERNAME') %>%
filter(Agent_Info$FULL.NAME == AN)

#-----------------------------------------------------------------------------------

CBS_ID <- C
CBS_ID$UID <- as.character(CBS_ID$UID)
Agent_Info$USERNAME <- as.character(Agent_Info$USERNAME)
CBS_ID <- CBS_ID %>%
select('CBS') %>%
filter(CBS_ID$PayRoll == Agent_Info$ID & CBS_ID$UID == Agent_Info$USERNAME)



#----------------------------------------------------------------------------------

Agent_Info <- merge(Agent_Info, CBS_ID)

#-----------------------------------------------------------------------------------

SC_Analysis <- D
SC_Analysis$BookRep <- as.character(SC_Analysis$BookRep)
Agent_Info$CBS <- as.character(Agent_Info$CBS)
SC_Analysis <- SC_Analysis %>%
select('Account', 'ScNumber', 'BookRep', 'Status', 'AuditCode', 'Notes') %>%
filter(SC_Analysis$BookRep == Agent_Info$CBS)


# moving to new variable

SC_Analysis_1 <- SC_Analysis


# extracting each type of info

# outage
outage <- SC_Analysis_1
outage <- outage %>%
select('Account', 'ScNumber', 'BookRep', 'Status', 'AuditCode', 'Notes') %>%
filter(outage$Status == "Outage")

# cancelled
cancelled <- SC_Analysis_1
cancelled <- cancelled %>%
select('Account', 'ScNumber', 'BookRep', 'Status', 'AuditCode', 'Notes') %>%
filter(cancelled$Status == "Cancelled")

# pending_cancel
pending_cancel <- SC_Analysis_1
pending_cancel <- pending_cancel %>%
select('Account', 'ScNumber', 'BookRep', 'Status', 'AuditCode', 'Notes') %>%
filter(pending_cancel$Status == "Pending Cancel")

#no_change
no_change <- SC_Analysis_1
no_change <- no_change %>%
select('Account', 'ScNumber', 'BookRep', 'Status', 'AuditCode', 'Notes') %>%
filter(no_change$Status == "No Change")

no_change <- na.omit(no_change)

# filtering out duplicates

outage <- distinct(outage, .keep_all = FALSE)
no_change <- distinct(no_change, .keep_all = FALSE)
pending_cancel <- distinct(pending_cancel, .keep_all = FALSE)
cancelled <- distinct(cancelled, .keep_all = FALSE)

# binding all audited data

final_data <- rbind(outage, no_change, pending_cancel, cancelled)
final_data <- distinct(final_data, .keep_all = FALSE)

# accumalating total info for the rep

Agent_Name <- E
Agent_Name <- Agent_Name %>%
select('TSR.Rep', 'SUPERVISOR', 'SC.to.Call.Ratio', 'X..of.Preventable.SC') %>%
filter(Agent_Name$TSR.Rep == AN)

# merging all agent infomaton 

Agent_Info <- cbind(Agent_Name, Agent_Info)


# plotting service calls based on status

png("SC_plot.png") 
plot(final_data$Status,
      main = "Distribution of Service Calls Audited by Status",
      ylab = "Number of calls audited",
      xlab = "SC status",
      col = "grey"
    )
dev.off()

# printing report

print(Agent_Info)
#print(sc_plot)
print((final_data))


sink()




