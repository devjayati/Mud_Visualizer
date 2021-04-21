#Cleaning up rows and columns for mudviz.csv
#setwd("~/") #make sure the csv files and cleanup.R are in the same folder
#mudviz <- read.csv("~/mudviz.csv")
#subsetting the main dataset for (1) accuracy [4:26], (2) time [27], (3) demographics [30:34]
#(4) usability [81:90]
#IGNORING expertise variables for now 
group <- mudviz[,"Group_name"]
accuracy <- mudviz[,4:26]
time <- mudviz[,27]
demographics <- mudviz[,30:34]
usability <- mudviz[,81:90]
mudviz$User_name <- 1:nrow(mudviz) 
User_name <- mudviz$User_name
mudviz_new <- cbind(group, User_name, accuracy, time, usability, demographics)
#extracting rows 
sub_mudviz <- mudviz_new[1:26,]
sub_mudviz$group <- "mudviz"
#################################################
#Cleaning up rows and columns for plain.csv
#plain <- read.csv("~/plain.csv")
#subsetting the main dataset for (1) accuracy [4:26], (2) time [27], (3) demographics [30:34]
#(4) usability [81:90]
group <- plain[,"Group_name"]
accuracy <- plain[,4:26]
time <- plain[,27]
demographics <- plain[,30:34]
usability <- plain[,81:90]
plain$User_name <- 1:nrow(plain) 
User_name <- plain$User_name
plain_new <- cbind(group, User_name, accuracy, time, usability, demographics)
#extracting rows 
sub_plain <- plain_new[1:26,]
sub_plain$group <- "plain"
################################################
#Combining the two datasets
copy_combined <- rbind(sub_mudviz, sub_plain)
combined <- rbind(sub_mudviz, sub_plain)
################################################

#combined$variable <- ifelse(combined$variable %in% c('checked'), , combined$variable)
combined$How_many_remote_servers_websites_on_the_Internet_can_the_smart_coffee_maker_communicate_with <- ifelse(combined$How_many_remote_servers_websites_on_the_Internet_can_the_smart_coffee_maker_communicate_with %in% 2,1,0)
combined$undefined_IP4_800_80 <- ifelse(combined$undefined_IP4_800_80 %in% c('checked'),1,0)
combined$undefined_IPv6_800_80 <- ifelse(combined$undefined_IPv6_800_80 %in% c('checked'),1,0)                                
combined$undefined_IPv4_777_888 <- ifelse(combined$undefined_IPv4_777_888 %in% c('unchecked'),1,0)                     
combined$undefined_IPv6_777_888 <- ifelse(combined$undefined_IPv6_777_888 %in% c('unchecked'),1,0) 
combined$undefined_IPv4_Any_source_port_Any_destination_port <- ifelse(combined$undefined_IPv4_Any_source_port_Any_destination_port %in% c('unchecked'),1,0) 
combined$undefined_IPv6_Any_source_port_Any_destination_port <- ifelse(combined$undefined_IPv6_Any_source_port_Any_destination_port %in% c('unchecked'),1,0)                                                                                                               
combined$Which_local_device_can_the_coffee_maker_communicate_with <- ifelse(combined$Which_local_device_can_the_coffee_maker_communicate_with %in% c('Motion'),1,0)                                                                                                        
combined$When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_source_port_numbers_should_it_use <- ifelse(combined$When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_source_port_numbers_should_it_use %in% "777_123",1,0)
combined$When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_destination_port_numbers_should_it_use <- ifelse(combined$When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_destination_port_numbers_should_it_use %in% "888_225",1,0)
combined$When_the_motion_sensor_communicates_with_the_coffee_maker_locally_over_UDP_which_internet_layer_protocol_source_port_number_and_destination_port_number_would_it_use <- ifelse(combined$When_the_motion_sensor_communicates_with_the_coffee_maker_locally_over_UDP_which_internet_layer_protocol_source_port_number_and_destination_port_number_would_it_use %in% "IPv4_123_50000",1,0)                                                                                               
combined$Can_the_smart_fridge_communicate_with_any_other_local_devices_beside_itself <- ifelse(combined$Can_the_smart_fridge_communicate_with_any_other_local_devices_beside_itself %in% c('No'),1,0)                                                                                       
combined$How_many_local_devices_and_remote_servers_can_the_Vase_device_communicate_with <- ifelse(combined$How_many_local_devices_and_remote_servers_can_the_Vase_device_communicate_with %in% c('0_local_device__3_remote_servers'),1,0)                                                                                   
combined$undefined_www_vaseupdater_com <- ifelse(combined$undefined_www_vaseupdater_com %in% c('unchecked'),1,0) 
combined$undefined_www_vasefileserver_com <- ifelse(combined$undefined_www_vasefileserver_com %in% c('unchecked'),1,0) 
combined$undefined_www_vasedb_com <- ifelse(combined$undefined_www_vasedb_com %in% c('unchecked'),1,0) 
combined$undefined_www_vasefirmware_com <- ifelse(combined$undefined_www_vasefirmware_com %in% c('checked'),1,0) 
combined$undefined_www_vaselinks_com <- ifelse(combined$undefined_www_vaselinks_com %in% c('checked'),1,0) 
combined$undefined_www_vasedatabase_com <- ifelse(combined$undefined_www_vasedatabase_com %in% c('checked'),1,0) 
combined$undefined_TCP_IPv4_Any_Source_Port_443 <- ifelse(combined$undefined_TCP_IPv4_Any_Source_Port_443 %in% c('checked'),1,0)
combined$undefined_TCP_IPv6_Any_Source_Port_443 <- ifelse(combined$undefined_TCP_IPv6_Any_Source_Port_443 %in% c('checked'),1,0)
combined$undefined_UDP_IPv4_Any_Source_Port_Any_Destination_Port <- ifelse(combined$undefined_UDP_IPv4_Any_Source_Port_Any_Destination_Port %in% c('unchecked'),1,0)
combined$undefined_UDP_IPv6_Any_Source_Port_Any_Destination_Port <- ifelse(combined$undefined_UDP_IPv6_Any_Source_Port_Any_Destination_Port %in% c('unchecked'),1,0)
#renaming columns (should have done this sooner)
names(combined)[names(combined) == "How_many_remote_servers_websites_on_the_Internet_can_the_smart_coffee_maker_communicate_with"] <- "u1"
names(combined)[names(combined) == "undefined_IP4_800_80"] <- "u2"
names(combined)[names(combined) == "undefined_IPv6_800_80"] <- "u3"
names(combined)[names(combined) == "undefined_IPv4_777_888"] <- "u4"
names(combined)[names(combined) == "undefined_IPv6_777_888"] <- "u5"
names(combined)[names(combined) == "undefined_IPv4_Any_source_port_Any_destination_port"] <- "u6"
names(combined)[names(combined) == "undefined_IPv6_Any_source_port_Any_destination_port"] <- "u7"
names(combined)[names(combined) == "Which_local_device_can_the_coffee_maker_communicate_with"] <- "u8"
names(combined)[names(combined) == "When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_source_port_numbers_should_it_use"] <- "u9"
names(combined)[names(combined) == "When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_destination_port_numbers_should_it_use"] <- "u10"
names(combined)[names(combined) == "When_the_motion_sensor_communicates_with_the_coffee_maker_locally_over_UDP_which_internet_layer_protocol_source_port_number_and_destination_port_number_would_it_use"] <- "u11"
names(combined)[names(combined) == "Can_the_smart_fridge_communicate_with_any_other_local_devices_beside_itself"] <- "u12"
names(combined)[names(combined) == "How_many_local_devices_and_remote_servers_can_the_Vase_device_communicate_with"] <- "u13"
names(combined)[names(combined) == "undefined_www_vaseupdater_com"] <- "u14"
names(combined)[names(combined) == "undefined_www_vasefileserver_com"] <- "u15"
names(combined)[names(combined) == "undefined_www_vasedb_com"] <- "u16"
names(combined)[names(combined) == "undefined_www_vasefirmware_com"] <- "u17"
names(combined)[names(combined) == "undefined_www_vaselinks_com"] <- "u18"
names(combined)[names(combined) == "undefined_www_vasedatabase_com"] <- "u19"
names(combined)[names(combined) == "undefined_TCP_IPv4_Any_Source_Port_443"] <- "u20"
names(combined)[names(combined) == "undefined_TCP_IPv6_Any_Source_Port_443"] <- "u21"
names(combined)[names(combined) == "undefined_UDP_IPv4_Any_Source_Port_Any_Destination_Port"] <- "u22"
names(combined)[names(combined) == "undefined_UDP_IPv6_Any_Source_Port_Any_Destination_Port"] <- "u23"


#cleaning up usability variables
usability_var <- combined [,27:36]
#n <- c('Daily'=3, 'Very Often'=2, 'Often'=1, 'Never'=-3)
#usability_var$I_think_that_I_would_like_to_perform_this_analysis_frequently <- c("10_Strongly_agree"=10, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, "1_Strongly_disagree"=1)  
#combined$I_think_that_I_would_like_to_perform_this_analysis_frequently <- ifelse(combined$I_think_that_I_would_like_to_perform_this_analysis_frequently %in% c('10_Strongly_agree'),10,combined$I_think_that_I_would_like_to_perform_this_analysis_frequently)                                                                                                       
#combined$I_found_the_analysis_unnecessarily_complex <- ifelse(combined$I_found_the_analysis_unnecessarily_complex %in% c('10_Strongly_agree'),10,combined$I_found_the_analysis_unnecessarily_complex)
#combined$I_thought_the_analysis_was_easy <- ifelse(combined$I_thought_the_analysis_was_easy %in% c('10_Strongly_agree'),10,combined$I_thought_the_analysis_was_easy)                                                                                                                                                                                                                                                              
#combined$I_think_that_I_would_need_the_support_of_a_technical_person_to_be_able_to_perform_the_analysis <- ifelse(combined$I_think_that_I_would_need_the_support_of_a_technical_person_to_be_able_to_perform_the_analysis %in% c('10_Strongly_agree'),"10",combined$I_think_that_I_would_need_the_support_of_a_technical_person_to_be_able_to_perform_the_analysis)
# [84] "I_think_that_I_would_need_the_support_of_a_technical_person_to_be_able_to_perform_the_analysis"                                                                      
# [85] "I_found_the_various_components_in_this_analysis_were_well_integrated"                                                                                                
# [86] "I_thought_there_was_too_much_inconsistency_in_this_analysis"                                                                                                         
# [87] "I_would_imagine_that_most_people_would_learn_to_use_this_analysis_very_quickly"                                                                                      
# [88] "I_found_this_analysis_very_cumbersome_to_perform"                                                                                                                    
# [89] "I_felt_very_confident_performing_the_analysis"                                                                                                                       
# [90] "I_needed_to_learn_a_lot_of_things_before_I_could_get_going_with_the_analysis"

#converting demographics 
combined$What_is_your_age <- as.numeric(factor(combined$What_is_your_age))
names(combined)[names(combined) == "What_is_your_age"] <- "age"
names(combined)[names(combined) == "Which_gender_do_you_most_identify_with"] <- "gender"
combined$gender <- as.numeric(factor(combined$gender))
names(combined)[names(combined) == "What_is_the_highest_degree_or_level_of_school_you_have_completed_If_you_are_currently_enrolled_in_school_please_indicate_the_highest_degree_you_have_received_"] <- "education"
combined$education <- as.numeric(factor(combined$education))
names(combined)[names(combined) == "What_is_your_current_employment_status"] <- "employment"
combined$employment <- as.numeric(factor(combined$employment))
names(combined)[names(combined) == "What_is_your_annual_income"] <- "income"
#Step 1: Accuracy measures
totacc <- combined$u1 + combined$u2 + combined$u3 + combined$u4 + combined$u5 + combined$u6 + combined$u7 + combined$u8 + combined$u9 + combined$u10 + combined$u11 + combined$u12 + combined$u13 + combined$u14 + combined$u15 + combined$u16 + combined$u17 + combined$u18 + combined$u19 + combined$u20 + combined$u21 + combined$u22 + combined$u23
acc <- (totacc*100)/23
combined <- cbind(combined, acc)
totacc_mud <- acc[1:26]
totacc_plain <- acc[27:52]
#prelim test
t.test(totacc_mud, totacc_plain, conf.level = 0.95)
#However, since we dont know if these two groups are normally distributed(they are not of course, test: plot(kdensity(totacc_mud)))
#we perform wilcox test. The number of values have to be same
#totacc_mud1 <- totacc[1:16]
#wilcox.test(totacc_mud1, totacc_plain) #okay in theory, but assumes equal variance
#checking if variances are different
var(totacc_mud)
var(totacc_plain)
shapiro.test(totacc_mud)
shapiro.test(totacc_plain)
#nope, go to sign rank test
#visuals first
library("ggpubr")
ggboxplot(combined, x = "group", y = "acc", color = "group", palette = c("#00AFBB", "#E7B800"), order = c("plain", "mudviz"), ylab = "Accuracy", xlab = "Groups")
#move to signed rank sum test
signedtest <- wilcox.test(totacc_mud, totacc_plain, paired = TRUE)
#alternative hypothesis test for mud greater than plain
#signedtest <- wilcox.test(totacc_mud, totacc_plain, paired = TRUE, alternative = "greater")
signedtest
z <- abs(qnorm((signedtest$p.value)/2))
effect_size <- z/sqrt(26)
#Step 2: Time comparison between mudviz and plain file 
names(combined)[names(combined) == "ResponseTime"] <- "time"
norm_time <- (combined$time - min(combined$time))/(max(combined$time) - min(combined$time)) #normalizing time variable
combined <- cbind(combined, norm_time)
time_mud <- norm_time[1:26]
time_plain <- norm_time[27:52]
#t.test(time_mud, time_plain, conf.level = 0.95)
#same as accuracy - not normal
#time_mud1 <- norm_time[1:16]
ggboxplot(combined, x = "group", y = "norm_time", color = "group", palette = c("#00AFBB", "#E7B800"), order = c("plain", "mudviz"), ylab = "Time", xlab = "Groups")
signedtest2 <- wilcox.test(time_mud, time_plain, paired = TRUE)
signedtest2
z2 <- abs(qnorm(signedtest2$p.value/2))
effect_size2 <- z2/sqrt(26)
#wilcox.test(time_mud, time_plain)
#Step 3: Use the link below to see usability metrics for the SUS scale
#https://www.usability.gov/how-to-and-tools/methods/system-usability-scale.html
#renaming usability variables
names(combined)[names(combined) == "I_think_that_I_would_like_to_perform_this_analysis_frequently"] <- "sus1"
names(combined)[names(combined) == "I_found_the_analysis_unnecessarily_complex"] <- "sus2"
names(combined)[names(combined) == "I_thought_the_analysis_was_easy"] <- "sus3"
names(combined)[names(combined) == "I_think_that_I_would_need_the_support_of_a_technical_person_to_be_able_to_perform_the_analysis"] <- "sus4"
names(combined)[names(combined) == "I_found_the_various_components_in_this_analysis_were_well_integrated"] <- "sus5"
names(combined)[names(combined) == "I_thought_there_was_too_much_inconsistency_in_this_analysis"] <- "sus6"
names(combined)[names(combined) == "I_would_imagine_that_most_people_would_learn_to_use_this_analysis_very_quickly"] <- "sus7"
names(combined)[names(combined) == "I_found_this_analysis_very_cumbersome_to_perform"] <- "sus8"
names(combined)[names(combined) == "I_felt_very_confident_performing_the_analysis"] <- "sus9"
names(combined)[names(combined) == "I_needed_to_learn_a_lot_of_things_before_I_could_get_going_with_the_analysis"] <- "sus10"
#combined$sus1 <- ifelse(combined$sus1 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus1 %in% c("1_Strongly_disagree"), "1", combined$sus1))
#combined$sus2 <- ifelse(combined$sus2 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus2 %in% c("1_Strongly_disagree"), "1", combined$sus2))
#copy_combined$sus3 <- ifelse(copy_combined$sus3 %in% c("10_Strongly_agree"), as.factor("10"), ifelse(copy_combined$sus3 %in% c("1_Strongly_disagree"), as.factor("1"), copy_combined$sus3))
#combined$sus4 <- ifelse(combined$sus4 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus4 %in% c("1_Strongly_disagree"), "1", combined$sus4))
#combined$sus5 <- ifelse(combined$sus5 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus5 %in% c("1_Strongly_disagree"), "1", combined$sus5))
#combined$sus6 <- ifelse(combined$sus6 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus6 %in% c("1_Strongly_disagree"), "1", combined$sus6))
#combined$sus7 <- ifelse(combined$sus7 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus7 %in% c("1_Strongly_disagree"), "1", combined$sus7))
#combined$sus8 <- ifelse(combined$sus8 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus8 %in% c("1_Strongly_disagree"), "1", combined$sus8))
#combined$sus9 <- ifelse(combined$sus9 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus9 %in% c("1_Strongly_disagree"), "1", combined$sus9))
#combined$sus10 <- ifelse(combined$sus10 %in% c("10_Strongly_agree"), "10", ifelse(combined$sus10 %in% c("1_Strongly_disagree"), "1", combined$sus10))
#combined$sus1 <- as.numeric(combined$sus1)
#combined$sus2 <- as.numeric(combined$sus2)
#copy_combined$sus3 <- as.numeric(copy_combined$sus3)
#combined$sus4 <- as.numeric(combined$sus4)
#combined$sus5 <- as.numeric(combined$sus5)
#combined$sus6 <- as.numeric(combined$sus6)
#combined$sus7 <- as.numeric(combined$sus7)
#combined$sus8 <- as.numeric(combined$sus8)
#combined$sus9 <- as.numeric(combined$sus9)
#combined$sus10 <- as.numeric(combined$sus10)
#convert likert 10 to 5
# Y = (B - A) * (x - a) / (b - a) + A.
# Y = (5 - 1) * (x - 1) / (10 - 1) + 1.
combined$sus1 <- round(0.44*combined$sus1 + 0.56,0)
combined$sus2 <- round(0.44*combined$sus2 + 0.56,0)
combined$sus3 <- round(0.44*combined$sus3 + 0.56,0)
combined$sus4 <- round(0.44*combined$sus4 + 0.56,0)
combined$sus5 <- round(0.44*combined$sus5 + 0.56,0)
combined$sus6 <- round(0.44*combined$sus6 + 0.56,0)
combined$sus7 <- round(0.44*combined$sus7 + 0.56,0)
combined$sus8 <- round(0.44*combined$sus8 + 0.56,0)
combined$sus9 <- round(0.44*combined$sus9 + 0.56,0)
combined$sus10 <- round(0.44*combined$sus10 + 0.56,0)
#SUS analysis
#odditems
combined$sus1 <- combined$sus1 - 1
combined$sus3 <- combined$sus3 - 1
combined$sus5 <- combined$sus5 - 1
combined$sus7 <- combined$sus7 - 1
combined$sus9 <- combined$sus9 - 1
#evenitems
combined$sus2 <- 5 - combined$sus2
combined$sus4 <- 5 - combined$sus4
combined$sus6 <- 5 - combined$sus6
combined$sus8 <- 5 - combined$sus8
combined$sus10 <- 5 - combined$sus10

totalsus <- combined$sus1 + combined$sus2 + combined$sus3 + combined$sus4 + combined$sus5 + combined$sus6 + combined$sus7 + combined$sus8 + combined$sus9 + combined$sus10
totalsusfinal <- 2.5*totalsus
sus_mud <- totalsusfinal[1:26]
sus_plain <- totalsusfinal[27:52]
combined <- cbind(combined, totalsusfinal)
ggboxplot(combined, x = "group", y = "totalsusfinal", color = "group", palette = c("#00AFBB", "#E7B800"), order = c("plain", "mudviz"), ylab = "Usability", xlab = "Groups")
signedtest3 <- wilcox.test(sus_mud, sus_plain, conf.level = 0.95, alternative = "two.sided", paired = TRUE) #set to paired, two sided test
signedtest3
z3 <- abs(qnorm(signedtest3$p.value/2))
effect_size3 <- z3/sqrt(26)

#norm_totalsus <- (totalsus - min(totalsus))/(max(totalsus) - min(totalsus))

#Step 4: Reporting demographics, do we need comparison? The group is tiny 
#converting demographics 
combined$What_is_your_age <- as.numeric(factor(combined$What_is_your_age))
names(combined)[names(combined) == "What_is_your_age"] <- "age"
names(combined)[names(combined) == "Which_gender_do_you_most_identify_with"] <- "gender"
combined$gender <- as.numeric(factor(combined$gender))
names(combined)[names(combined) == "What_is_the_highest_degree_or_level_of_school_you_have_completed_If_you_are_currently_enrolled_in_school_please_indicate_the_highest_degree_you_have_received_"] <- "education"
combined$education <- as.numeric(factor(combined$education))
names(combined)[names(combined) == "What_is_your_current_employment_status"] <- "employment"
combined$employment <- as.numeric(factor(combined$employment))
names(combined)[names(combined) == "What_is_your_annual_income"] <- "income"
combined = subset(combined, select = -c(income) )
plot(table(combined$age))
plot(table(combined$gender))
plot(table(combined$education))
plot(table(combined$employment))


#Step 5: Expertise 
#Extract expertise 
exp_mud <- mudviz[1:26,38:77]
exp_plain <- plain[1:26,38:77]
expertise <- rbind(exp_mud, exp_plain)
#rename variables
names(expertise)[names(expertise) == "undefined_Pretending_to_be_someone_or_a_company_to_steal_users_information"] <- "Kphish1"                                                                                          
names(expertise)[names(expertise) == "undefined_Making_a_fake_website_that_looks_legitimate_to_steal_user_information"] <- "Kphish2" 
names(expertise)[names(expertise) == "undefined_Sending_spam_emails_Defrauding_someone_online"] <- "Kphish3" 
names(expertise)[names(expertise) == "undefined_Other_methods_for_stealing_information"] <- "Kphish4" 
names(expertise)[names(expertise) == "undefined_Hacking_someones_computer"] <- "Kphish5" 
names(expertise)[names(expertise) == "undefined_Tracking_your_internet_habits_to_send_advertisements_"] <- "Kphish6" 
names(expertise)[names(expertise) == "undefined_I_Do_not_Know"] <- "Kphish7" 

names(expertise)[names(expertise) == "undefined_The_certificate_provides_encryption"] <- "Kcert1"
names(expertise)[names(expertise) == "undefined_The_certificate_protects_information"] <- "Kcert2"
names(expertise)[names(expertise) == "undefined_The_certificate_shows_the_website_is_registered_and_valid"] <- "Kcert3"
names(expertise)[names(expertise) == "undefined_The_certificate_actively_is_secure_and_safe_against_malicious_stuff_including_hackers"] <- "Kcert4"
names(expertise)[names(expertise) == "undefined_The_website_is_trustworthy_and_has_proper_privacy_protection_and_is_accountable_for_information_use"] <- "Kcert5"

names(expertise)[names(expertise) == "SQL_injection_is_a_technique_to"] <- "Ksql"

names(expertise)[names(expertise) == "The_difference_between_a_passive_and_reactive_Intrusion_Detection_System_is"] <- "Kids"

names(expertise)[names(expertise) == "Without_any_other_changes_in_the_default_settings_of_a_web_server_what_can_be_the_motivation_to_close_port_80"] <- "K80"

names(expertise)[names(expertise) == "How_many_computer_programming_languages_do_you_know_Not_including_HTML"] <- "Elang"

names(expertise)[names(expertise) == "How_many_years_of_working_experience_do_you_have_in_network_operation_and_security_area"] <- "Eyears"

names(expertise)[names(expertise) == "On_average_how_many_times_do_you_have_to_deal_with_computer_security_related_problems"] <- "Etimes"

names(expertise)[names(expertise) == "undefined_Firewall"] <- "Etech1"
names(expertise)[names(expertise) == "undefined_Antivirus"] <- "Etech2"
names(expertise)[names(expertise) == "undefined_Intrusion_Detection_System_IDS"] <- "Etech3"
names(expertise)[names(expertise) == "undefined_Secure_Shell_SSH"] <- "Etech4"
names(expertise)[names(expertise) == "undefined_Pretty_Good_Privacy_PGP"] <- "Etech5"
names(expertise)[names(expertise) == "undefined_Access_control_AC"] <- "Etech6"

names(expertise)[names(expertise) == "undefined_Designed_a_website"] <- "Eexp1"
names(expertise)[names(expertise) == "undefined_Registered_a_domain_name"] <- "Eexp2"
names(expertise)[names(expertise) == "undefined_Used_SSH"] <- "Eexp3"
names(expertise)[names(expertise) == "undefined_Configured_a_firewall"] <- "Eexp4"
names(expertise)[names(expertise) == "undefined_Created_a_database"] <- "Eexp5"
names(expertise)[names(expertise) == "undefined_Installed_a_computer_program"] <- "Eexp6"
names(expertise)[names(expertise) == "undefined_Written_a_computer_program"] <- "Eexp7"

names(expertise)[names(expertise) == "undefined_https"] <- "Kweb1"
names(expertise)[names(expertise) == "undefined_lock_icon_on_the_page"] <- "Kweb2"
names(expertise)[names(expertise) == "undefined_certificate"] <- "Kweb3"
names(expertise)[names(expertise) == "undefined_website_privacy_statements"] <- "Kweb4"
names(expertise)[names(expertise) == "undefined_type_of_website"] <- "Kweb5"
names(expertise)[names(expertise) == "undefined_professionallooking_website"] <- "Kweb6"
names(expertise)[names(expertise) == "undefined_Other"] <- "Kweb7"

names(expertise)[names(expertise) == "What_is_the_main_definition_of_IoT"] <- "Kiot"

names(expertise)[names(expertise) == "What_is_Access_Control_List"] <- "Kac"

#reprogram values
expertise$Kphish1 <- ifelse(expertise$Kphish1 %in% c('checked'),1,0)
expertise$Kphish2 <- ifelse(expertise$Kphish2 %in% c('checked'),1,0)
expertise$Kphish3 <- ifelse(expertise$Kphish3 %in% c('unchecked'),1,0)
expertise$Kphish4 <- ifelse(expertise$Kphish4 %in% c('unchecked'),1,0)
expertise$Kphish5 <- ifelse(expertise$Kphish5 %in% c('unchecked'),1,0)
expertise$Kphish6 <- ifelse(expertise$Kphish6 %in% c('unchecked'),1,0)
expertise$Kphish7 <- ifelse(expertise$Kphish7 %in% c('unchecked'),1,0)
Kphishsum <- expertise$Kphish1 + expertise$Kphish2 + expertise$Kphish3 + expertise$Kphish4 + expertise$Kphish5 + expertise$Kphish6 + expertise$Kphish7
Kphish <- ifelse(Kphishsum>=5,1,0) #median

expertise$Kcert1 <- ifelse(expertise$Kcert1 %in% c('checked'),1,0)
expertise$Kcert2 <- ifelse(expertise$Kcert2 %in% c('checked'),1,0)
expertise$Kcert3 <- ifelse(expertise$Kcert3 %in% c('unchecked'),1,0)
expertise$Kcert4 <- ifelse(expertise$Kcert4 %in% c('unchecked'),1,0)
expertise$Kcert5 <- ifelse(expertise$Kcert5 %in% c('unchecked'),1,0)
Kcertsum <- expertise$Kcert1 + expertise$Kcert2 + expertise$Kcert3 + expertise$Kcert4 + expertise$Kcert5
Kcert <- ifelse(Kcertsum>=3,1,0) #median

expertise$Ksql <- ifelse(expertise$Ksql %in% c('Inject_a_malicious_statement_to_the_database_through_a_website'),1,0) 

expertise$Kids <- ifelse(expertise$Kids %in% c('Reactive_IDS_can_reprogram_the_Firewall_and_passive_IDS_does_not'),1,0) 

expertise$K80 <- ifelse(expertise$K80 %in% c('Block_Hypertext_Transfer_Protocol_daemon'),1,0) 

expertise$Kweb1 <- ifelse(expertise$Kweb1 %in% c('checked'),1,0)
expertise$Kweb2 <- ifelse(expertise$Kweb2 %in% c('checked'),1,0)
expertise$Kweb3 <- ifelse(expertise$Kweb3 %in% c('checked'),1,0)
expertise$Kweb4 <- ifelse(expertise$Kweb4 %in% c('unchecked'),1,0)
expertise$Kweb5 <- ifelse(expertise$Kweb5 %in% c('unchecked'),1,0)
expertise$Kweb6 <- ifelse(expertise$Kweb6 %in% c('checked'),1,0)
expertise$Kweb7 <- ifelse(expertise$Kweb7 %in% c('checked'),1,0)
Kwebsum <- expertise$Kweb1 + expertise$Kweb2 + expertise$Kweb3 + expertise$Kweb4 + expertise$Kweb5 + expertise$Kweb6 + expertise$Kweb7
Kweb <- ifelse(Kwebsum>=5,1,0) #median

expertise$Kiot <- ifelse(expertise$Kiot %in% c('The_Internet_of_Things_IoT_describes_the_network_of_objects_that_are_connected_and_exchange_data_with_other_devices_and_systems_over_the_internet'),1,0) 

expertise$Kac <- ifelse(expertise$Kac %in% c('list_of_permissions_associated_with_an_object'),1,0) 

expertise <- cbind(expertise, Kphish, Kcert, Kweb)

totalknowledge <- expertise$Kphish + expertise$Kcert + expertise$Ksql + expertise$Kids + expertise$K80 + expertise$Kweb + expertise$Kiot + expertise$Kac 
expertise <- cbind(expertise, totalknowledge)
#lm
df <- cbind(expertise$Kphish, expertise$Kcert, expertise$Ksql, expertise$Kids, expertise$K80, expertise$Kweb, expertise$Kac) #Kiot is dropped
df_headings <- c('Kphish', 'Kcert', 'Ksql', 'Kids', 'K80', 'Kweb', 'Kac')
#names(df) <- df_headings
c <- cov(df)
factors <- fa(c, nfactors = 1, rotate = "oblimin", fm="minres")
print(factors)
print(factors$loadings, cutoff = 0.3)
fa.diagram(factors)
fa.parallel(c, fm = "minres", fa = "fa")
totalknowledge_fa <- -0.5*expertise$Kcert + 0.6*expertise$Ksql + 0.6*expertise$Kids + 0.7*expertise$K80
#names(combined)[names(combined) == "undefined_IPv6_800_80"] <- "u3"
#factors
#f1 = .7(taught course) .7 (attend conf)+.6(security job)+.6 (IT degree)
#f2 = .8 designedWebsite + .7 RegisteredDomain + .4 CreateDatabase + .4 WrittenProgram
#f3 = 1 InstalledProgram - .6 NoneOfAbove
#f4 = .6 Used_SSH + .5 ConfFirewall
#total security = f1 + f4
#total computer = f2+f3
#F1 <- 0.6*plain$How_many_years_of_working_experience_do_you_have_in_network_operation_and_security_area

df2 <- cbind(expertise$Eexp, expertise$Etech, expertise$Eyears, expertise$Etimes, expertise$Elang)
df2_headings <- c('Eexp', 'Etech', 'Eyears', 'Etimes', 'Elang')
c2 <- cov(df2)
library("psych")
factors2 <- fa(c2, nfactors = 1, rotate = "oblimin", fm="minres")
print(factors2)
print(factors2$loadings, cutoff = 0.3)
fa.diagram(factors2)
fa.parallel(c, fm = "minres", fa = "fa")
total_computer_fa <- 0.5*expertise$Eyears + 0.4*expertise$Elang + 0.4*expertise$Etimes
#experience 
expertise$Eexp1 <- ifelse(expertise$Eexp1 %in% c('checked'),1,0)
expertise$Eexp2 <- ifelse(expertise$Eexp2 %in% c('checked'),1,0)
expertise$Eexp3 <- ifelse(expertise$Eexp3 %in% c('checked'),1,0)
expertise$Eexp4 <- ifelse(expertise$Eexp4 %in% c('checked'),1,0)
expertise$Eexp5 <- ifelse(expertise$Eexp5 %in% c('checked'),1,0)
expertise$Eexp6 <- ifelse(expertise$Eexp6 %in% c('checked'),1,0)
expertise$Eexp7 <- ifelse(expertise$Eexp7 %in% c('checked'),1,0)
totcomp <- expertise$Eexp1 + expertise$Eexp2 + expertise$Eexp3 + expertise$Eexp4 + expertise$Eexp5 + expertise$Eexp6 + expertise$Eexp7
Eexp <- totcomp
none <- ifelse(totcomp==0,1,0)
F2 <- 0.8*expertise$Eexp1 + 0.7*expertise$Eexp2 + 0.4*expertise$Eexp5 + 0.4*expertise$Eexp7
F3 <- 1*expertise$Eexp6 - 0.6*none #sorted
#F4 <- 0.6*plain$undefined_Used_SSH + 0.5*plain$undefined_Configured_a_firewall
#total_security <- F1 + F4 
total_computer <- F2 + F3
expertise <- cbind(expertise, Eexp)
expertise$Eexp <- (expertise$Eexp - min(expertise$Eexp))/(max(expertise$Eexp) - min(expertise$Eexp))

expertise$Etech1 <- ifelse(expertise$Etech1 %in% c('checked'),1,0)
expertise$Etech2 <- ifelse(expertise$Etech2 %in% c('checked'),1,0)
expertise$Etech3 <- ifelse(expertise$Etech3 %in% c('checked'),1,0)
expertise$Etech4 <- ifelse(expertise$Etech4 %in% c('checked'),1,0)
expertise$Etech5 <- ifelse(expertise$Etech5 %in% c('checked'),1,0)
expertise$Etech6 <- ifelse(expertise$Etech6 %in% c('checked'),1,0)
tottech <- expertise$Etech1 + expertise$Etech2 + expertise$Etech3 + expertise$Etech4 + expertise$Etech5 + expertise$Etech6
Etech <- ifelse(tottech>=2,1,0)
expertise <- cbind(expertise, Etech, total_computer)
expertise$Etech <- (expertise$Etech - min(expertise$Etech))/(max(expertise$Etech) - min(expertise$Etech))

expertise$Elang <- ifelse(expertise$Elang==15, 2, ifelse(expertise$Elang==510, 3, ifelse(expertise$Elang==10, 4, expertise$Elang)))
expertise$Elang <- (expertise$Elang - min(expertise$Elang))/(max(expertise$Elang) - min(expertise$Elang))

expertise$Eyears <-  as.factor(expertise$Eyears)
expertise$Eyears <- ifelse(expertise$Eyears %in% c('None'), 1, ifelse(expertise$Eyears %in% c('A_few_months_less_than_a_year'), 2, ifelse(expertise$Eyears %in% c('15_years'), 3, 4)))
expertise$Eyears <- (expertise$Eyears - min(expertise$Eyears))/(max(expertise$Eyears) - min(expertise$Eyears))

expertise$Etimes <-  as.factor(expertise$Etimes)
expertise$Etimes <- ifelse(expertise$Etimes %in% c('Once_every_year_or_less'), 1, ifelse(expertise$Etimes %in% c('Once_every_month'), 2, ifelse(expertise$Etimes %in% c('Once_every_week'), 3, ifelse(expertise$Etimes %in% c('Once_every_day'), 4, 5))))
expertise$Etimes <- (expertise$Etimes - min(expertise$Etimes))/(max(expertise$Etimes) - min(expertise$Etimes))

expertise <- cbind(expertise, totalknowledge_fa)
expertise <- cbind(expertise, total_computer_fa)
copy_expertise <- cbind(expertise, combined$acc)
categoryl <- rep("low",13)
categoryh <- rep("high",13)
category <- c(categoryl, categoryh)

know_mud <- copy_expertise[1:26,]
know_mud <- know_mud[order(know_mud$totalknowledge_fa),]
know_mud <- cbind(know_mud, category)
know_plain <- copy_expertise[27:52,]
know_plain <- know_plain[order(know_plain$totalknowledge_fa),]
know_plain <- cbind(know_plain, category)

comp_mud <- copy_expertise[1:26,]
comp_mud <- comp_mud[order(comp_mud$total_computer_fa),]
comp_mud <- cbind(comp_mud, category)
comp_plain <- copy_expertise[27:52,]
comp_plain <- comp_plain[order(comp_plain$total_computer_fa),]
comp_plain <- cbind(comp_plain, category)

know_mud_lh <- wilcox.test(know_mud$totalknowledge_fa[1:13], know_mud$totalknowledge_fa[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)
know_plain_lh <- wilcox.test(know_plain$totalknowledge_fa[1:13], know_plain$totalknowledge_fa[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)

comp_mud_lh <- wilcox.test(comp_mud$total_computer_fa[1:13], comp_mud$total_computer_fa[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)
comp_plain_lh <- wilcox.test(comp_plain$total_computer_fa[1:13], comp_plain$total_computer_fa[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)

#plot stuff
ggboxplot(know_mud, x = "category", y = "`combined$acc`", color = "category", palette = c("#00AFBB", "#E7B800"), order = c("low", "high"), ylab = "accuracy", xlab = "category", title = "Mud - Knowledge") 
ggboxplot(know_plain, x = "category", y = "`combined$acc`", color = "category", palette = c("#00AFBB", "#E7B800"), order = c("low", "high"), ylab = "accuracy", xlab = "category", title = "Plain - Knowledge")

ggboxplot(comp_mud, x = "category", y = "`combined$acc`", color = "category", palette = c("#00AFBB", "#E7B800"), order = c("low", "high"), ylab = "accuracy", xlab = "category", title = "Mud - Comp Exp")
ggboxplot(comp_plain, x = "category", y = "`combined$acc`", color = "category", palette = c("#00AFBB", "#E7B800"), order = c("low", "high"), ylab = "accuracy", xlab = "category", title = "Plain - Comp Exp")

#know_plain_acc_lh <- wilcox.test(know_plain$`combined$acc`[1:13], know_plain$`combined$acc`[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)
#know_mud_acc_lh <- wilcox.test(know_mud$`combined$acc`[1:13], know_mud$`combined$acc`[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)

#comp_plain_acc_lh <- wilcox.test(comp_plain$`combined$acc`[1:13], comp_plain$`combined$acc`[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)
#comp_mud_acc_lh <- wilcox.test(comp_mud$`combined$acc`[1:13], comp_mud$`combined$acc`[14:26], conf.level = 0.95, alternative = "two.sided", paired = TRUE)

#ordinal logistic regression
m <- glm(know_mud$category ~ know_mud$'combined$acc', family = "binomial")
n <- glm(know_plain$category ~ know_plain$'combined$acc', family = "binomial")
o <- glm(comp_plain$category ~ comp_plain$'combined$acc', family = "binomial")
p <- glm(comp_mud$category ~ comp_mud$'combined$acc', family = "binomial")

summary(glm(know_mud$'combined$acc' ~ know_mud$totalknowledge_fa))
summary(glm(know_plain$'combined$acc' ~ know_plain$totalknowledge_fa)) #significant
summary(glm(comp_mud$'combined$acc' ~ comp_mud$total_computer_fa))
summary(glm(comp_plain$'combined$acc' ~ comp_plain$total_computer_fa))
# know_mud$Kac, know_mud$K80, know_mud$Kweb, know_mud$Ksql, know_mud$Kids

plot(know_mud$'combined$acc' ~ know_mud$totalknowledge_fa, xlab = "TotalKnowledge", ylab = "Accuracy (mudviz)")
abline(glm(know_mud$'combined$acc' ~ know_mud$totalknowledge_fa))

plot(know_plain$'combined$acc' ~ know_plain$totalknowledge_fa, xlab = "TotalKnowledge", ylab = "Accuracy (plain)")
abline(glm(know_plain$'combined$acc' ~ know_plain$totalknowledge_fa))

plot(comp_mud$'combined$acc' ~ comp_mud$total_computer_fa, xlab = "TotalExperience", ylab = "Accuracy (mudviz)")
abline(glm(comp_mud$'combined$acc' ~ comp_mud$total_computer_fa))

plot(comp_plain$'combined$acc' ~ comp_plain$total_computer_fa, xlab = "TotalExperience", ylab = "Accuracy (plain)")
abline(glm(comp_plain$'combined$acc' ~ comp_plain$total_computer_fa))
# [1] "Header"                                                                                                                                                              
# [2] "Group_name"                                                                                                                                                          
# [3] "User_name"                                                                                                                                                           
# [4] "How_many_remote_servers_websites_on_the_Internet_can_the_smart_coffee_maker_communicate_with" (2)                                                                        
# [5] "undefined_IP4_800_80" (checked)                                                                                                                                               
# [6] "undefined_IPv6_800_80" (checked)                                                                                                                                               
# [7] "undefined_IPv4_777_888" (unchecked)                                                                                                                                               
# [8] "undefined_IPv6_777_888" (unchecked)                                                                                                                                             
# [9] "undefined_IPv4_Any_source_port_Any_destination_port"  (unchecked)                                                                                                               
# [10] "undefined_IPv6_Any_source_port_Any_destination_port" (unchecked)                                                                                                                
# [11] "Which_local_device_can_the_coffee_maker_communicate_with"  (Motion)                                                                                                          
# [12] "When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_source_port_numbers_should_it_use" (777_123)                                                  
# [13] "When_the_smart_coffee_maker_communicates_with_the_motion_sensor_locally_which_destination_port_numbers_should_it_use" (888_225)                                                
# [14] "When_the_motion_sensor_communicates_with_the_coffee_maker_locally_over_UDP_which_internet_layer_protocol_source_port_number_and_destination_port_number_would_it_use" (IPv4_123_50000)
# [15] "Can_the_smart_fridge_communicate_with_any_other_local_devices_beside_itself" (No)                                                                                         
# [16] "How_many_local_devices_and_remote_servers_can_the_Vase_device_communicate_with" (0_local_device__3_remote_servers)                                                                                     
# [17] "undefined_www_vaseupdater_com"    (unchecked)                                                                                                                                     
# [18] "undefined_www_vasefileserver_com" (unchecked)                                                                                                                                     
# [19] "undefined_www_vasedb_com"         (unchecked)                                                                                                                                     
# [20] "undefined_www_vasefirmware_com"  (checked)                                                                                                                                    
# [21] "undefined_www_vaselinks_com"     (checked)                                                                                                                                    
# [22] "undefined_www_vasedatabase_com"  (checked)                                                                                                                                    
# [23] "undefined_TCP_IPv4_Any_Source_Port_443"  (checked)                                                                                                                            
# [24] "undefined_TCP_IPv6_Any_Source_Port_443"  (checked)                                                                                                                           
# [25] "undefined_UDP_IPv4_Any_Source_Port_Any_Destination_Port"  (unchecked)                                                                                                             
# [26] "undefined_UDP_IPv6_Any_Source_Port_Any_Destination_Port"  (unchecked)                                                                                                             
# [27] "ResponseTime"                                                                                                                                                        
# [28] "participant"                                                                                                                                                         
# [29] "valid_participant"                                                                                                                                                   
# [30] "What_is_your_age"                                                                                                                                                    
# [31] "Which_gender_do_you_most_identify_with"                                                                                                                              
# [32] "What_is_the_highest_degree_or_level_of_school_you_have_completed_If_you_are_currently_enrolled_in_school_please_indicate_the_highest_degree_you_have_received_"      
# [33] "What_is_your_current_employment_status"                                                                                                                              
# [34] "What_is_your_annual_income"                                                                                                                                          
# [35] "ResponseTime.1"                                                                                                                                                      
# [36] "participant.1"                                                                                                                                                       
# [37] "valid_participant.1"                                                                                                                                                 
# [38] "undefined_Pretending_to_be_someone_or_a_company_to_steal_users_information"                                                                                          
# [39] "undefined_Making_a_fake_website_that_looks_legitimate_to_steal_user_information"                                                                                     
# [40] "undefined_Sending_spam_emails_Defrauding_someone_online"                                                                                                             
# [41] "undefined_Other_methods_for_stealing_information"                                                                                                                    
# [42] "undefined_Hacking_someones_computer"                                                                                                                                 
# [43] "undefined_Tracking_your_internet_habits_to_send_advertisements_"                                                                                                     
# [44] "undefined_I_Do_not_Know"                                                                                                                                             
# [45] "undefined_The_certificate_provides_encryption"                                                                                                                       
# [46] "undefined_The_certificate_protects_information"                                                                                                                      
# [47] "undefined_The_certificate_shows_the_website_is_registered_and_valid"                                                                                                 
# [48] "undefined_The_certificate_actively_is_secure_and_safe_against_malicious_stuff_including_hackers"                                                                     
# [49] "undefined_The_website_is_trustworthy_and_has_proper_privacy_protection_and_is_accountable_for_information_use"                                                       
# [50] "SQL_injection_is_a_technique_to"                                                                                                                                     
# [51] "The_difference_between_a_passive_and_reactive_Intrusion_Detection_System_is"                                                                                         
# [52] "Without_any_other_changes_in_the_default_settings_of_a_web_server_what_can_be_the_motivation_to_close_port_80"                                                       
# [53] "How_many_computer_programming_languages_do_you_know_Not_including_HTML"                                                                                              
# [54] "How_many_years_of_working_experience_do_you_have_in_network_operation_and_security_area"                                                                             
# [55] "On_average_how_many_times_do_you_have_to_deal_with_computer_security_related_problems"                                                                               
# [56] "undefined_Firewall"                                                                                                                                                  
# [57] "undefined_Antivirus"                                                                                                                                                 
# [58] "undefined_Intrusion_Detection_System_IDS"                                                                                                                            
# [59] "undefined_Secure_Shell_SSH"                                                                                                                                          
# [60] "undefined_Pretty_Good_Privacy_PGP"                                                                                                                                   
# [61] "undefined_Access_control_AC"                                                                                                                                         
# [62] "undefined_Designed_a_website"                                                                                                                                        
# [63] "undefined_Registered_a_domain_name"                                                                                                                                  
# [64] "undefined_Used_SSH"                                                                                                                                                  
# [65] "undefined_Configured_a_firewall"                                                                                                                                     
# [66] "undefined_Created_a_database"          (F2)                                                                                                                             
# [67] "undefined_Installed_a_computer_program"                                                                                                                              
# [68] "undefined_Written_a_computer_program"  (F2)                                                                                                                              
# [69] "undefined_https"                                                                                                                                                     
# [70] "undefined_lock_icon_on_the_page"                                                                                                                                     
# [71] "undefined_certificate"                                                                                                                                               
# [72] "undefined_website_privacy_statements"                                                                                                                                
# [73] "undefined_type_of_website"                                                                                                                                           
# [74] "undefined_professionallooking_website"                                                                                                                               
# [75] "undefined_Other"                                                                                                                                                     
# [76] "What_is_the_main_definition_of_IoT"                                                                                                                                  
# [77] "What_is_Access_Control_List"                                                                                                                                         
# [78] "ResponseTime.2"                                                                                                                                                      
# [79] "participant.2"                                                                                                                                                       
# [80] "valid_participant.2"                                                                                                                                                 
# [81] "I_think_that_I_would_like_to_perform_this_analysis_frequently"                                                                                                       
# [82] "I_found_the_analysis_unnecessarily_complex"                                                                                                                          
# [83] "I_thought_the_analysis_was_easy"                                                                                                                                     
# [84] "I_think_that_I_would_need_the_support_of_a_technical_person_to_be_able_to_perform_the_analysis"                                                                      
# [85] "I_found_the_various_components_in_this_analysis_were_well_integrated"                                                                                                
# [86] "I_thought_there_was_too_much_inconsistency_in_this_analysis"                                                                                                         
# [87] "I_would_imagine_that_most_people_would_learn_to_use_this_analysis_very_quickly"                                                                                      
# [88] "I_found_this_analysis_very_cumbersome_to_perform"                                                                                                                    
# [89] "I_felt_very_confident_performing_the_analysis"                                                                                                                       
# [90] "I_needed_to_learn_a_lot_of_things_before_I_could_get_going_with_the_analysis"                                                                                        
# [91] "ResponseTime.3"                                                                                                                                                      
# [92] "workerId"                                                                                                                                                            
# [93] "assignmentId"                                                                                                                                                        
# [94] "valid_participant.3"  