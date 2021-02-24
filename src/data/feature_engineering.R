# SET DIRECTORY TO SOURCE FILE LOCATION: Session> Set Working Directory> To Source File Location


########################
### Read in raw Data ###
########################

library(tidyverse)

covid <- read.csv("../../data/interim/full_data.csv")

#####################
### Data Cleaning ###
#####################


# Reformat dates from excel format
date_cols <- c("Closed.other.non.essential.businesses", "Began.to.reopen.businesses.statewide", "Stay.at.home..shelter.in.place" , "End.stay.at.home.shelter.in.place" , "Mandate.face.mask.use.by.all.individuals.in.public.spaces", "Mandate.face.mask.use.by.employees.in.public.facing.businesses", "State.ended.statewide.mask.use.by.individuals.in.public.spaces" )

# Convert missing date values to NA
for (c in date_cols) {
  covid[[c]] <- ifelse(covid[[c]] == 0, NA, covid[[c]])
}

for (c in date_cols) {
  covid[[c]] <- as.Date(covid[[c]], origin = "1899-12-30")
}

# Convert State of Emergency from "2020-02-30" to Date format (for some reason I couldn't just add it to `date_cols`)
covid$`State.of.emergency`<- as.Date(covid$`State.of.emergency`)


############################################################
### Build indicators for state-level policy date columns ###
############################################################
covid$NoStateEmergency <- ifelse(is.na(covid$`State.of.emergency`), 1, 0)

covid$NoCloseBusi <- ifelse(is.na(covid$`Closed.other.non.essential.businesses`), 1, 0)

covid$NoReopenBusi <- ifelse(is.na(covid$`Began.to.reopen.businesses.statewide`), 1, 0)

covid$SIP <- ifelse(is.na(covid$`Stay.at.home..shelter.in.place`), 0, 1)

covid$NoENDSIP <- ifelse(is.na(covid$`End.stay.at.home.shelter.in.place`), 1, 0)

covid$EndedSIP <- ifelse(is.na(covid$`End.stay.at.home.shelter.in.place`), 0, 1)

covid$NoFaceMask <- ifelse(is.na(covid$`Mandate.face.mask.use.by.all.individuals.in.public.spaces`), 1, 0)



covid$NoFaceMaskEmploy <- ifelse(is.na(covid$`Mandate.face.mask.use.by.employees.in.public.facing.businesses`), 1, 0)

covid$NoEndFaceMask <- ifelse(is.na(covid$`State.ended.statewide.mask.use.by.individuals.in.public.spaces`), 1, 0)

##########################
### Transform Features ###
##########################

# state emergency should be reasonable marker of when COVID began entering state

# SIP - StateEmergency (how long before people had to shelter in place)
covid$SIP_StateEmergency <- covid$`Stay.at.home..shelter.in.place` - covid$`State.of.emergency` 


# CloseNonEssential - StateEmergency (how long were non-essential businesses open for )
covid$CloseNonEssential_StateEmergency <- covid$`Closed.other.non.essential.businesses` - covid$`State.of.emergency`


# BeganReopenBusiness - StateEmergency (how long before business reopened)
covid$ReopenBusi_StateEmergency <- covid$`Began.to.reopen.businesses.statewide` - covid$`State.of.emergency`


# MandateFaceMask - StateEmergency (how long before masks were mandatory)
covid$FaceMask_StateEmergency <- covid$`Mandate.face.mask.use.by.all.individuals.in.public.spaces` - covid$`State.of.emergency`

# MandateFaceMaskEmployee - StateEmergency (How long before employees in public businesses needed to wear mask)
covid$FaceMaskEmploy_StateEmergency <- covid$`Mandate.face.mask.use.by.employees.in.public.facing.businesses` - covid$`State.of.emergency`


# BeganReopenBusuness - CloseBusiness (how long businesses were closed for)
covid$ReopenBusi_CloseBusi <- covid$`Began.to.reopen.businesses.statewide` - covid$`Closed.other.non.essential.businesses`


# ENDSIP - SIP (how long did people shelter in place)
covid$ENDSIP_SIP <- covid$`End.stay.at.home.shelter.in.place` - covid$`Stay.at.home..shelter.in.place`


# CurrentDate - EndSIP (for states who ended SIP how long has it been)
covid$Curr_ENDSIP <- as.Date("2020-10-26", origin = "1899-12-30") - covid$`End.stay.at.home.shelter.in.place`


# CurrentDate - StateEndMaskReq (very few points prob not useful)
covid$Curr_ENDMask <- as.Date("2020-10-26", origin = "1899-12-30") - covid$`State.ended.statewide.mask.use.by.individuals.in.public.spaces`


# Some necessary transformations for EDA, needed for plot_usmap() function
covid <- covid %>% rename(state = State) 
covid <- mutate(covid, NoFaceMask_factor = factor(NoFaceMask))
covid <- mutate(covid, NoFaceMaskEmploy_factor = factor(NoFaceMaskEmploy))
covid <- mutate(covid, SIP_factor = factor(SIP))


##########################################
###Save as R object in data/processed ###
##########################################

saveRDS(covid, file = "../../data/processed/main_state_data.RDS")
