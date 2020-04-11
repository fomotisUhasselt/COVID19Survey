load(file = 'Data/surveys/preloaded.RData')


ggblue <- "#56B4E9";#gg_co
ggorange <- gg_color_hue(2)[1];
ggred <- scales::hue_pal()(1)
ggpurple <- "#796BD6"
gggreen <- "#A3D596"


variable1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, "language", "age", "gender", "postal_code", "education", "0", "0", "0", "0", "number_people_talk", "date_last_fysical_contact", 
              "employment", "status_usual_home_work", "freq_usual_home_work", "type_work", "", "freq_home_work_exceed_0", "freq_home_work_last_week", "home_work_today", "reason_not_home_work_today", 
              "", "childcare_today", "", "has_flu_symptom_last_week", "duration_flu_symptom", "flu_symptom_over", "date_flue_symptom_over", "housemember_flu_symptom_last_week",
              "change_behaviour_household", "change_behaviour_work", "change_behaviour_public", "disease", "freq_nervous_last_two_week", "freq_afraid_last_two_week",
              "age_class", "", "niscode", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 'diploma')
variable1[48] <- 'province'

variable2 <- c(0, 'start', 0, 0, 'ip', 0, 'duration_sec', 0, 0, 0, 0, 0, 0, 0, 'latitude', 'longitude', 'channel', 'languague', 'answer_1703', 'age', 'gender', 'postal_code', 'education', 0, 0, 0, 0, 0, 'number_people_talk_outside_household', 'date_last_fysical_contact_outside_houshold', 'number_people_talk', 'date_last_fysical_contact',
               'employment', 'status_usual_home_work', 'freq_usual_home_work', 'reason_no_homework_precovid', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 'postal_code_work', 'work_wednesday', 'work_thursday', 'work_friday', 'work_saturday', 'work_sunday', 'work_monday', 'work_tuesday', 'location_work_wednesday', 'location_work_thursday', 'location_work_friday', 'location_work_saturday', 'location_work_sunday', 'location_work_monday', 'location_work_tuesday', 'reason_not_home_work_today', 0,
               0, 'reason_not_work_today', 'childcare_today', 0, 0, 0, 0, 0, 0, 0, 0, 'fast_fever', 'high_fever', 'sore_troat', 'short_breath', 'dry_cough', 'deep_cough', 'chest_pain', 'runny_nose', 'muscle_ache', 'fatiguee', 'shivery', 'nausea', 'sore_eyes', '', 0, 0,
               "start_symptom", "still_symptom", "end_symptom", "contact_healthcare", "method_contact_healthcare", "info_phone_healthcare", "info_examination_healthcare", rep('', 56),
               'personal_change_behaviour_household', 'personal_change_behaviour_work', 'personal_change_behaviour_public', 'houshold_change_behaviour_household', 'houshold_change_behaviour_work', 'houshold_change_behaviour_public', 'disease_hart', 'disease_lung', 'disease_kidney', 'disease_diabetes', 'disease_high_bloodpressure', 'disease_immune', 'disease_cancer', '', 'concentrate_last_week', 'short_sleep_last_week', 'useful_last_week', 
               'decision_last_week', 'pressure_last_week', 'manage_difficulties_last_week', 'pleasure_last_week', 'face_problems_last_week', 'depressed_last_week', 'lost_selfconfidence_last_week', 'useless_last_week', 'happy_last_week', 'optimistic_last_week', 'freq_anxious_last_two_weeks', 'freq_catastrophic_feeling_last_two_weeks', 'tension_70plus', 'tension_adult', 'tension_child_12plus', 'tension_minor', 'tension_family', 'walking_last_week',
               'biking_last_week', 'exercise_inside_last_week', 'games_last_week', 'talking_last_week_outside', 'visual_communication_last_week', 'non_visual_communciation_last_week', '', rep('', 16), 'diploma')
variable2[211] <- 'province'

variable3 <- c(0, 'start', 0, 0, 'ip', 0, 'duration_sec', 0, 0, 0, 0, 0, 0, 0, 'latitude', 'longitude', 'channel', 'languague', 'answer_1703', 'answer_2403', '', 'age', 'gender', 'postal_code', 'in_belgium', 'education', 0, 0, 0, 0, 0, 'number_people_talk_outside_household', 'date_last_fysical_contact_outside_houshold', 'number_people_talk', 'date_last_fysical_contact',
              'employment_precovid',  'employment_change', rep('', 19), 'status_usual_home_work', 'freq_usual_home_work', 'reason_no_homework_precovid', 0, 0, 0,
               'postal_code_work', 'work_wednesday', 'work_thursday', 'work_friday', 'work_saturday', 'work_sunday', 'work_monday', 'work_tuesday', 'location_work_wednesday', 'location_work_thursday', 'location_work_friday', 'location_work_saturday', 'location_work_sunday', 'location_work_monday', 'location_work_tuesday', 'reason_not_home_work_today', 0,
               0, 'reason_not_work_today', 'childcare_today', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'fast_fever', 'high_fever', 'sore_troat', 'short_breath', 'dry_cough', 'deep_cough', 'chest_pain', 'runny_nose', 'muscle_ache', 'fatiguee', 'shivery', 'nausea', 'sore_eyes', '', 0, 0,
               "start_symptom", "still_symptom", "end_symptom", "contact_healthcare", "method_contact_healthcare", "info_phone_healthcare", "info_examination_healthcare", 'disease_hart', 'disease_lung', 'disease_kidney', 'disease_diabetes', 'disease_high_bloodpressure', 'disease_immune', 'disease_cancer', '', rep('', 28),
               'personal_change_behaviour_household', 'personal_change_behaviour_work', 'personal_change_behaviour_public', 'houshold_change_behaviour_household', 'houshold_change_behaviour_work', 'houshold_change_behaviour_public', 'concentrate_last_week', 'short_sleep_last_week', 'useful_last_week', 
               'decision_last_week', 'pressure_last_week', 'manage_difficulties_last_week', 'pleasure_last_week', 'face_problems_last_week', 'depressed_last_week', 'lost_selfconfidence_last_week', 'useless_last_week', 'happy_last_week', 'optimistic_last_week', 'freq_anxious_last_two_weeks', 'tension_70plus', 'tension_adult', 'tension_child_12plus', 'tension_minor', 'tension_family', 'walking_last_week',
               'biking_last_week', 'exercise_inside_last_week', 'games_last_week', 'talking_last_week_outside', 'visual_communication_last_week', 'non_visual_communciation_last_week', '', 'use_app', 'reason_not_use_app_smartphone', 'reason_not_use_app_privacy', '', 'reason_not_use_app_no_confidence', 'reason_not_use_app_not_understand', '', '', rep('', 21), 'diploma')
variable3[198] <- 'province'

variable4 <- c(0, 'start', 0, 0, 'ip', 0, 'duration_sec', 0, 0, 0, 0, 0, 0, 0, 'latitude', 'longitude', 'channel', 'languague', 'answer_1703', 'answer_2403', 'answer_3103', '', 'age', 'gender', 'postal_code', 'in_belgium', 'education', 0, 0, 0, 0, 0, 'number_people_talk_outside_household', 'date_last_fysical_contact_outside_houshold', 'number_people_talk', 'date_last_fysical_contact',
               'employment_precovid',  'employment_change', rep('', 19), 'status_usual_home_work', 'freq_usual_home_work', 'reason_no_homework_precovid', 0, 0, 0,
               'postal_code_work', 'work_wednesday', 'work_thursday', 'work_friday', 'work_saturday', 'work_sunday', 'work_monday', 'work_tuesday', 'location_work_wednesday', 'location_work_thursday', 'location_work_friday', 'location_work_saturday', 'location_work_sunday', 'location_work_monday', 'location_work_tuesday', 'reason_not_home_work_today', 0,
               0, 'reason_not_work_today', 'childcare_today', 0, 0, 0, 0, 0, 0, 0, 0, 'fast_fever', 'high_fever', 'sore_troat', 'short_breath', 'dry_cough', 'deep_cough', 'chest_pain', 'runny_nose', 'muscle_ache', 'fatiguee', 'shivery', 'nausea', 'sore_eyes', '', 0, 0,
               "start_symptom", "still_symptom", "end_symptom", "contact_healthcare", "method_contact_healthcare", "info_phone_healthcare", "info_examination_healthcare", 'disease_hart', 'disease_lung', 'disease_kidney', 'disease_diabetes', 'disease_high_bloodpressure', 'disease_immune', 'disease_cancer', '', 'avoid_doctor_fear', 'avoid_doctor_busy', 'avoid_doctor_unavailable', '', 'treatment', '',  rep('', 28),
               'know_covid_patient_hospital', 'know_covid_patient_nursing_home', 'know_covid_patient_deceased', 'know_covid_patient_home', '', '', '',
               'personal_change_behaviour_household', 'personal_change_behaviour_work', 'personal_change_behaviour_public', 'houshold_change_behaviour_household', 'houshold_change_behaviour_work', 'houshold_change_behaviour_public', 'concentrate_last_week', 'short_sleep_last_week', 'useful_last_week', 
               'decision_last_week', 'pressure_last_week', 'manage_difficulties_last_week', 'pleasure_last_week', 'face_problems_last_week', 'depressed_last_week', 'lost_selfconfidence_last_week', 'useless_last_week', 'happy_last_week', 'optimistic_last_week', 'freq_anxious_last_two_weeks', 'tension_70plus', 'tension_adult', 'tension_child_12plus', 'tension_minor', 'tension_family', 'trust_others', 'risk_helping_others', 'importance_equality', 'trust_federal_government', 
               'trust_regional_government', 'trust_municipality', 'trust_europe', 'trust_scientists', 'trust_news_government', 'trust_news_experts', 'trust_news_journalist', 'trust_news_social_media', 'trust_news_healthcare_workers', 'trust_news_family', 'opinion_measures_government', 'opinion_risk_infection', 'opinion_disease_serious', 'opinion_measures_too_severe',  'walking_last_week',
               'biking_last_week', 'exercise_inside_last_week', 'games_last_week', 'talking_last_week_outside', 'visual_communication_last_week', 'non_visual_communciation_last_week', '', 'use_app', 'reason_not_use_app_smartphone', 'reason_not_use_app_privacy', '', 'reason_not_use_app_no_confidence', 'reason_not_use_app_not_understand', '', '', rep('', 13), 'diploma') 
variable4[219] <- 'province'

attach_variable_names <- function(data, variable_name) {
  select <- !(variable_name %in% c('', '0'))
  data <- data[, select]
  variable_name <- variable_name[select]
  
  colnames(data) <- variable_name
  
  return(data)
}

survey1 <- attach_variable_names(survey_list[[1]], variable1)
survey2 <- attach_variable_names(survey_list[[2]], variable2)
survey3 <- attach_variable_names(survey_list[[3]], variable3)
survey4 <- attach_variable_names(survey_list[[4]], variable4)

survey1$survey = 1
survey2$survey = 2
survey3$survey = 3
survey4$survey = 4

table(survey1$reason_not_home_work_today)
table(survey1$home_work_today)

survey1$home_work_today[survey1$reason_not_home_work_today == "I'm at home but not working"] <- "Not applicable"
survey1$reason_not_home_work_today[survey1$reason_not_home_work_today == "I'm at home but not working"] <- "Missing"

prepare_work <- function(data, survey_number) {
  home <- rowSums(data %>% select(starts_with('location_work_')) == "Thuis")
  company_protected <- rowSums(data %>% select(starts_with('location_work_')) == "Niet thuis omringd door andere mensen of collega's maar met voldoende voorzorgsmaatregelen")
  company_unprotected <- rowSums(data %>% select(starts_with('location_work_')) == "Niet thuis omringd door andere mensen of collega's en in mijn ogen zonder voldoende voorzorgsmaatregelen")
  
  count <- rowSums(data %>% select(starts_with('work_')) == "Ja")
  
  data <- data %>%
    mutate(freq_work_last_week = count,
           freq_work_home_last_week = home,
           freq_work_company_protected_last_week = company_protected,
           freq_work_company_unprotected_last_week = company_unprotected,
           pct_work_home_last_week = home / count,
           pct_work_company_protected_last_week = company_protected / count,
           pct_work_company_unprotected_last_week = company_unprotected / count)
  
  aggregated <- summarize(data,
                           survey = survey_number,
                           variable = 'work condition', 
                           protected = mean(pct_work_company_protected_last_week, na.rm = TRUE),
                           unprotected = mean(pct_work_company_unprotected_last_week, na.rm = TRUE),
                           home = mean(pct_work_home_last_week, na.rm = TRUE)) %>%
    pivot_longer(cols = -c(1, 2), names_to = 'label', values_to = 'value')
  aggregated$order = 1:nrow(aggregated)
  
  return(aggregated)
}

prepare_age <- function(data, survey_number) {
  age_group <- cut(data$age, 
            c(-1, 17, 29, 39, 49, 59, 69, 79, Inf),
            labels = c('0-17', '18-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+'))
  
  aggregated <- data.frame(survey = survey_number,
                           variable = 'age',
                           label = names(table(age_group)),
                           order = 1:8,
                           value = as.numeric(prop.table(table(age_group))))
  
  return(aggregated)
}

prepare_gender <- function(data, survey_number) {
  as.numeric(prop.table(table(data$gender)[1:3]))
  
  aggregated <- data.frame(survey = survey_number,
                           variable = 'gender',
                           label = c('Male', 'Female', 'Other'),
                           order = 1:3,
                           value = as.numeric(prop.table(table(data$gender)[1:3])))
  
  return(aggregated)
}

prepare_change_behaviour <- function(data, 
                                     survey_number,
                                     household = "houshold_change_behaviour_household", 
                                     work = "houshold_change_behaviour_work",
                                     public = "houshold_change_behaviour_public",
                                     personal_household = "personal_change_behaviour_household", 
                                     personal_work = "personal_change_behaviour_work",
                                     personal_public = "personal_change_behaviour_public") {
  
  aggregated <- rbind(
    data.frame(survey = survey_number,
               variable = 'change_behaviour_household',
               label = 0:10,
               order = 0:10,
               value = as.numeric(prop.table(table(data[[household]])))),
    data.frame(survey = survey_number,
               variable = 'change_behaviour_work',
               label = 0:10,
               order = 0:10,
               value = as.numeric(prop.table(table(data[[work]])))),
    data.frame(survey = survey_number,
               variable = 'change_behaviour_public',
               label = 0:10,
               order = 0:10,
               value = as.numeric(prop.table(table(data[[public]])))),
    data.frame(survey = survey_number,
               variable = 'change_behaviour',
               label = c('household', 'work', 'public places'),
               order = 1:3,
               value = c(mean(data[[household]], na.rm = TRUE),
                         mean(data[[work]], na.rm = TRUE),
                         mean(data[[public]], na.rm = TRUE))),
    data.frame(survey = survey_number,
               variable = 'personal_change_behaviour_household',
               label = 0:10,
               order = 0:10,
               value = as.numeric(prop.table(table(data[[personal_household]])))),
    data.frame(survey = survey_number,
               variable = 'personal_change_behaviour_work',
               label = 0:10,
               order = 0:10,
               value = as.numeric(prop.table(table(data[[personal_work]])))),
    data.frame(survey = survey_number,
               variable = 'personal_change_behaviour_public',
               label = 0:10,
               order = 0:10,
               value = as.numeric(prop.table(table(data[[personal_public]])))),
    data.frame(survey = survey_number,
               variable = 'personal_change_behaviour',
               label = c('household', 'work', 'public places'),
               order = 1:3,
               value = c(mean(data[[personal_household]], na.rm = TRUE),
                         mean(data[[personal_work]], na.rm = TRUE),
                         mean(data[[personal_public]], na.rm = TRUE))))
  
  return(aggregated)
}

prepare_contact <- function(data, survey_number) {
  var <- data$date_last_fysical_contact_outside_houshold
  var <- var[var != 'Missing']
  
  today <- sum(str_detect(var, 'dinsdag')) / length(var)
  yesterday <- sum(str_detect(var, 'maandag')) / length(var)
  one_week_plus <- sum(str_detect(var, 'niemand')) / length(var)
  two_to_six <- 1 - today - yesterday - one_week_plus
  
  aggregated <- data.frame(survey = survey_number,
                           variable = 'last_fysical_contact',
                           label = c('today', 'yesterday', '2-6 days ago', '7+ days ago'),
                           order = 1:4,
                           value = c(today, yesterday, two_to_six, one_week_plus))
  
  return(aggregated)
}

prepare_talk <- function(data, survey_number) {
  var <- data$number_people_talk_outside_household
  var <- var[var != 'Missing']
  
  none <- sum(var == 'Geen') / length(var)
  one <- sum(var == '1') / length(var)
  two_to_five <- sum(var %in% 2:5) / length(var)
  six_to_twenty <- sum(var %in% 6:20) / length(var)
  twenty_plus <- sum(var == "Meer dan 20") / length(var)
  
  aggregated <- data.frame(survey = survey_number,
                           variable = 'number_people_talk',
                           label = c('None', 'one', '2-5', '6-20', '20+'),
                           order = 1:5,
                           value = c(none, one, two_to_five, six_to_twenty, twenty_plus))
  
  return(aggregated)
}

prepare_psycho <- function(data, survey_number) {
  sleep <- data$short_sleep_last_week
  sleep <- sleep[sleep != 'Missing']
  
  optimistic <- data$optimistic_last_week
  optimistic <- optimistic[optimistic != 'Missing']
  
  concentrate <- data$concentrate_last_week
  concentrate <- concentrate[concentrate != 'Missing']
  
  aggregated <- data.frame(survey = survey_number,
                           variable = c(rep('sleep', 4), rep('optimistic', 4), rep('concentrate', 4)),
                           label = c('Better', 'Same', 'Somewhat worse', 'Much worse',
                                     'More', 'Same', 'A bit less','Much less',
                                     'Better', 'Same', 'Somewhat worse', 'Much worse'),
                           order = rep(1:4, 3),
                           value = c(as.numeric(prop.table(table(sleep)))[1:4],
                                     as.numeric(prop.table(table(optimistic)))[1:4],
                                     as.numeric(prop.table(table(concentrate)))[1:4]))
  
  return(aggregated)
}

prepare_tension <- function(data, survey_number) {
  tension <- data %>% 
    select(starts_with('tension')) %>%
    map_df(function(x){ 
      x <- as.numeric(x)
      x[x == 5] <- 0 # missing = 0
      return(x)
      }) %>%
    mutate(tension = pmax(tension_70plus, tension_adult, tension_child_12plus, tension_minor, tension_family)) %>%
    filter(tension != 0) %>%
    pull(tension)
  
  aggregated <- data.frame(survey = survey_number,
                           variable = 'tension',
                           label = c('None', 'Same', 'Somewhat more', 'Much more'),
                           order = 1:4,
                           value = c(as.numeric(prop.table(table(tension)))[1:4]))
  
  return(aggregated)
}

prepare_response_count <- function(data, survey_number) {
  var <- data$province
  var <- var[var != 'Missing']
  total_count <- nrow(data)
  freq <- as.numeric(prop.table(table(var))[1:11])
  
  aggregated <- rbind(data.frame(survey = survey_number,
                           variable = 'response_freq',
                           label = c('Antwerpen', 'Vlaams-Brabant', 'West-Vlaanderen', 'Oost-Vlaanderen', 'Henegouwen',
                                     'Luik', 'Limburg', 'Luxemburg', 'Namen', 'Waals-Brabant', 'Brussel'),
                           order = 1:11,
                           value = freq),
                      data.frame(survey = survey_number,
                                 variable = 'response_count',
                                 label = 'response_count',
                                 order = survey_number,
                                 value = total_count))
  
  return(aggregated)
}

aggregated1 <- rbind(prepare_age(survey1, 1),
                     prepare_gender(survey1, 1),
                     prepare_change_behaviour(survey1, 1, 
                                              "change_behaviour_household", 
                                              "change_behaviour_work",
                                              "change_behaviour_public",
                                              "change_behaviour_household", 
                                              "change_behaviour_work",
                                              "change_behaviour_public"),
                     prepare_response_count(survey1, 1) )

aggregated2 <- rbind(prepare_age(survey2, 2),
                     prepare_work(survey2, 2),
                     prepare_gender(survey2, 2),
                     prepare_change_behaviour(survey2, 2),
                     prepare_contact(survey2, 2),
                     prepare_talk(survey2, 2),
                     prepare_psycho(survey2, 2), 
                     prepare_tension(survey2, 2),
                     prepare_response_count(survey2, 2))

aggregated3 <- rbind(prepare_age(survey3, 3),
                     prepare_work(survey3, 3),
                     prepare_gender(survey3, 3),
                     prepare_change_behaviour(survey3, 3),
                     prepare_contact(survey3, 3),
                     prepare_talk(survey3, 3),
                     prepare_psycho(survey3, 3), 
                     prepare_tension(survey3, 3),
                     prepare_response_count(survey3, 3))

aggregated4 <- rbind(prepare_age(survey4, 4),
                     prepare_work(survey4, 4),
                     prepare_gender(survey4, 4),
                     prepare_change_behaviour(survey4, 4),
                     prepare_contact(survey4, 4),
                     prepare_talk(survey4, 4),
                     prepare_psycho(survey4, 4), 
                     prepare_tension(survey4, 4),
                     prepare_response_count(survey4, 4))

aggregated <- rbind(aggregated1, aggregated2, aggregated3, aggregated4)

ggplot(aggregated %>% filter(variable == 'tension')) +
  theme_bw() +
  geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('tension')

ggplot(aggregated %>% filter(variable == 'concentrate')) +
  theme_bw() +
  geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('concentration')

ggplot(aggregated %>% filter(variable == 'optimistic')) +
  theme_bw() +
  geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('optimistic')

ggplot(aggregated %>% filter(variable == 'sleep')) +
  theme_bw() +
  geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('sleep quality')


ggplot(aggregated %>% filter(variable == 'number_people_talk')) +
  theme_bw() +
  geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('number of people talked to (not virtual)')

ggplot(aggregated %>% filter(variable == 'last_fysical_contact')) +
  theme_bw() +
  geom_bar(aes(x = reorder(label, order), y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('last fysical contact (handshake or kiss)')


ggplot(aggregated %>% filter(variable == 'change_behaviour_public',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('change behaviour in public locations')

ggplot(aggregated %>% filter(variable == 'change_behaviour_work',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('change behaviour at work')

ggplot(aggregated %>% filter(variable == 'change_behaviour_household',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('change behaviour in household')

ggplot(aggregated %>% filter(variable == 'change_behaviour',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('average response change behaviour')

ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_public',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('personal change behaviour in public locations')

ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_work',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('personal change behaviour at work')

ggplot(aggregated %>% filter(variable == 'personal_change_behaviour_household',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('personal change behaviour in household')

ggplot(aggregated %>% filter(variable == 'personal_change_behaviour',
                             survey != 1)) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggorange, gggreen, ggpurple), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('average response personal change behaviour')

ggplot(aggregated %>% filter(variable == 'age')) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple), label = c('March 17', 'March 24', 'March 31', 'April 7')) +
  ggtitle('participants age')

ggplot(aggregated %>% filter(variable == 'gender')) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = 'dodge2') +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggblue, ggorange, gggreen, ggpurple), label = c('March 17', 'March 24', 'March 31', 'April 7')) +
  ggtitle('participants gender')


ggplot(aggregated %>% filter(variable == "work condition")) +
  theme_bw() +
  geom_bar(aes(x = label, y = value, fill = factor(survey)), stat = 'identity', position = "dodge2") +
  ylab('percentage') + xlab('') +
  scale_fill_manual('', values = c(ggblue, ggorange, gggreen), label = c('March 24', 'March 31', 'April 7')) +
  ggtitle('work conditions')



Reduce(intersect, list(colnames(survey1), colnames(survey2), colnames(survey3), colnames(survey4)))
Reduce(intersect, list(colnames(survey2), colnames(survey3), colnames(survey4)))

## spatial

spatial_symptom_2 <- read.table('spatialPredictions/preds_R2_S1.txt', header = T)[,1]
spatial_symptom_3 <- read.table('spatialPredictions/preds_R3_S1.txt', header = T)[,1]

require(rgdal)
require(sf)
map <- read_sf("shapeFiles/Belgium_NIS (1).shp", quiet = TRUE)
map$symptom2 <- spatial_symptom_2
map$symptom3 <- spatial_symptom_3

belgium_shp <- read_sf("shapeFiles/BE_SB_TF_PD_STATDIS_2014.shp", quiet = TRUE)
belgium_shp <- belgium_shp %>%
  group_by(Nis_012011) %>% 
  slice(1) %>%
  ungroup() %>%
  select(Nis_012011, Gemeente, Commune, Prov_nl, Prov_fr) %>%
  data.frame

map$gemeente = belgium_shp$Gemeente
map$commune = belgium_shp$Commune
map$prov_nl = belgium_shp$Prov_nl
map$prov_fr = belgium_shp$Prov_fr

map$prov_fr[is.na(map$prov_fr)] <- 'Bruxelles'
map$prov_nl[is.na(map$prov_nl)] <- 'Brussel'

st_crs(map) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
map <- st_transform(map, '+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs')


require(RColorBrewer)
require(classInt)
require(leaflet)
nclr <- 5       #### number of colors to be used
plotclr <- brewer.pal(nclr,"YlOrRd")

breaks <- c(round(min(map$predict_symptom_2),4)-0.0001,
            round(as.numeric(quantile(map$predict_symptom_2, probs = seq(0, 1, 0.125))),4)[2:8],
            round(max(map$predict_symptom_2),4)+0.0001)
class <- classIntervals(map$predict_symptom_2, nclr, style = "fixed",fixedBreaks = breaks)
color_bins <- class$brks
color_pal <- colorBin( 
  plotclr, domain = map$Gemeente, 
  bins = color_bins
)

color_pal <- colorNumeric( 
  plotclr, 
  domain = map$predict_symptom_2, 
  palette = 'RdYlGn'
)

color_pal <- colorNumeric(
  palette = colorRampPalette(c('yellow', 'red'))(8), 
  domain = seq(0.1, 0.2, by =0.05))


leaflet(data = map) %>% 
  addLegend("bottomleft", pal = color_pal, 
            values = ~ predict_symptom_2, 
            title = "<small>Predicted probability of having at least 1 
            COVID-19 symptom <br /> (mean, CAR convolution model, 
            <br /> corrected for age and gender)</small>") %>% 
  
  addPolygons(
    data = map,
    color = "#444444",
    weight = 2,
    #stroke = FALSE,
    smoothFactor = 0.5, 
    opacity = 1.0,
    fillOpacity = 0.5,
    fillColor = ~color_pal(map$predict_symptom_2),
    highlightOptions = highlightOptions(color = "white", weight = 1,
                                        bringToFront = TRUE),
    label = sprintf("<strong>%s (Prob = %g )</strong>", 
                    map$gemeente, 
                    map$predict_symptom_2) %>% lapply(htmltools::HTML)
  )

save(aggregated, file = "publicApp/data/aggregated_data.RData")
st_write(map, "publicApp/data", "shapefile_predict", driver="ESRI Shapefile")


res <- read_sf("publicApp/data/shapefile_predict.shp", quiet = TRUE)
