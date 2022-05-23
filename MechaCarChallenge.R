#import library
library(dplyr)

#read csv
mechacar_mpg <- read.csv('MechaCar_mpg.csv',check.names = F, stringsAsFactors = F)
head(mechacar_mpg)

# linear regression
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, 
   data=mechacar_mpg)

#summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,
           data=mechacar_mpg))

# read csv
suspension_coil <-read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F) 
head(suspension_coil)

#total summary
total_summary <- suspension_coil %>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), 
                                               SD = sd(PSI), .groups = "drop")

#lot summary
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot)%>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI),
                                                                            SD = sd(PSI), .groups = "drop")

#test mean
t.test(suspension_coil$PSI,mu= 1500)

#test lot mean
lot1 <- subset(suspension_coil,Manufacturing_Lot == 'Lot1')
t.test(lot1$PSI,mu= 1500)
lot2 <- subset(suspension_coil,Manufacturing_Lot == 'Lot2')
t.test(lot2$PSI,mu= 1500)
lot3 <- subset(suspension_coil,Manufacturing_Lot == 'Lot3')
t.test(lot3$PSI,mu= 1500)
