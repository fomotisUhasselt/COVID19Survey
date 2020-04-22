require(sf)

translation_en <- data.frame(ex = c(''), en = c(''));
translation_nl <- data.frame(ex = c(''), nl = c(''));

add_translation <- function(txt_ex, txt_en, txt_nl) {
  translation_en <<- rbind(translation_en, data.frame(ex = txt_ex, en = txt_en))
  translation_nl <<- rbind(translation_nl, data.frame(ex = txt_ex, nl = txt_nl))
}

add_translation('Belgium', 'Belgium', 'België')
add_translation(c('Antwerpen', 'Brussel', 'Henegouwen', 'Limburg', 'Luik', 'Luxemburg', 'Namen', 'Oost-Vlaanderen', 'Vlaams-Brabant', 'Waals-Brabant', 'West-Vlaanderen', 'Arrondissemment Brussel'),
                c('Antwerp', 'Brussels', 'Hainaut', 'Limburg', 'Liège', 'Luxembourg', 'Namur', 'East Flanders', 'Flemish Brabant', 'Walloon Brabant', 'West Flanders', 'Brussels'),
                c('Antwerpen', 'Brussel', 'Henegouwen', 'Limburg', 'Luik', 'Luxemburg', 'Namen', 'Oost-Vlaanderen', 'Vlaams-Brabant', 'Waals-Brabant', 'West-Vlaanderen', 'Brussel'))
add_translation(c('April 21', 'April 14', 'April 7', 'March 31', 'March 24', 'March 17'),
                c('April 21', 'April 14', 'April 7', 'March 31', 'March 24', 'March 17'),
                c('21 april', '14 april', '7 april', '31 maart', '24 maart', '17 maart'))

add_translation('Distribution participants by province',
                'Distribution participants by province',
                'Verdeling deelnemers per provincie')

add_translation(c('Probability of having symptoms',
                  'percentage'),
                c('Proportion of citizens having at least one COVID-19 symptom',
                  'percentage'),
                c('Proportie inwoners met minstens één COVID-19 symptoom',
                  'percentage'))

map <- read_sf('shapefiles/predict_nis/shapefile_predict_nis.shp', quiet = TRUE)

add_translation(map$gemeente,
                map$gemeente,
                map$gemeente)

add_translation(c('response', 'age', 'gender', 'work', 'behaviour work', 'behaviour public', 'behaviour household', 'conversation', 'fysical contact', 'sleep', 'concentrate', 'optimistic', 'tension'),
                c('participants', 'age', 'gender', 'work', 'behaviour work', 'behaviour public', 'behaviour household', 'conversation', 'fysical contact', 'sleep', 'concentrate', 'optimistic', 'tension'),
                c('deelnemers', 'leeftijd', 'geslacht', 'werk', 'gedrag werk', 'gedrag publieke plaatsen', 'gedrag gezin', 'conversatie', 'fysiek contact', 'slaap', 'concentratie', 'optimisme', 'spanning'))

add_translation(c('survey_response', 'responses', 'distribution_gender', 'Female', 'Male', 'Other', 'distribution_age'),
                c('Number of participants per survey (x1000)', 'participants', 'Distribution of participants per gender', 'Female', 'Male', 'Other', 'Distribution of participants by age group'),
                c('Aantal deelnemers per enquête (x1000)', 'deelnemers', 'Verdeling deelnemers per geslacht', 'Vrouw', 'Man', 'Andere', 'Verdeling deelnemers per leeftijdscategorie'))

add_translation(c('Current working conditions', 'protected', 'unprotected', 'homework', 'Change in behaviour at work', 'Change in behaviour at public places', 'Change in behaviour at home', 'Number of people spoken today (not virtual)', 'None', 'one', '2-5', '6-20', '20+', 'last fysical contact (handshake or kiss) outside household', 'today', 'yesterday', '2-6 days ago', '7+ days ago'),
                c('In which conditions have you worked over the past days?', 'Protected', 'Unprotected', 'Homework', 'To what extent have you adapted your behaviour with regard to work', 'To what extent have you adapted your behaviour at public places', 'To what extent have you adapted your behaviour with the people you live with (1; no ad', 'Number of people spoken today (not virtual)', 'None', 'One', '2-5', '6-20', '20+', 'last fysical contact (handshake or kiss) outside household\nComputed based on information collected between March 17 and April 14', 'Today', 'Yesterday', '2-6 days ago', '7+ days ago'),
                c('In welke omstandigheden heb je de afgelopen dagen gewerkt?', 'Beschermd', 'Onbeschermd', 'Thuiswerk', 'In welke mate heb jijzelf jouw gedrag aangepast tegenover werk? (1: niet, 10: zeer sterk)', 'In welke mate heb jijzelf jouw gedrag aangepast op openbare plaatsen? (1: niet, 10: zeer sterk)', 'In welke mate heb jijzelf jouw gedrag aangepast tegenover huisgenoten? (1: niet, 10: zeer sterk)', 'Hoeveel mensen, afgezien van huisgenoten, heb je gisteren gesproken in het echt?', 'Geen', 'Eén', '2-5', '6-20', '20+', 'Laatste fysiek contact (hand of kus) buiten het gezin\nBerekend op basis van informatie verzameld tussen 17 maart en 21 april', 'Vandaag', 'Gisteren', '2-6 dagen geleden', '7+ dagen geleden'))

add_translation(c('Sleep quality last week', 'Better', 'Same', 'Somewhat worse', 'Much worse', 'Concentration last week', 'Optimism last week', 'More', 'A bit less', 'Much less', 'Tension last week', 'None', 'Same', 'Somewhat more', 'Much more'),
                c('How was your sleep quality over the past week?', 'Better', 'Same', 'Somewhat worse', 'Much worse', 'How well could you concentrate on your activities over the past week?', 'How optimistic were you over the past week?', 'More', 'A bit less', 'Much less', 'Have you experienced tensions within your family over the past week?', 'None', 'Same', 'A bit more', 'Much more'),
                c('Wat was de kwaliteit van je slaap afgelopen week?', 'Beter', 'Zelfde', 'Beetje slechter', 'Veel slechter', 'Hoe kon je je concentreren afgelopen week?', 'Hoe optimistisch was je afgelopen week?', 'Meer', 'Een beetje minder', 'Veel minder', 'Heb je familiale spanningen ervaren afgelopen week?', 'Nee', 'Evenveel als normaal', 'Een beetje meer', 'Veel meer'))

add_translation(c('education', 'symptom', 'childcare', 'home', 'school/childcare', 'friends/acquaintances', 'aunts/uncles', 'grandparents', 'other', 'arrangement childcare', 'Primary education', 'Secondary education', 'Higher education', 'PhD', 'Distribution of participants by level of education'),
                c('education', 'Symptom', 'childcare', 'Home', 'School/childcare', 'Friends/acquaintances', 'Aunts/uncles', 'Grandparents', 'Other', 'How did you arrange childcare today?', 'Primary/BUSO', 'Secondary', 'Higher\nBachelor/Master', 'PhD', 'Distribution of participants by level of education'),
                c('opleiding', 'Symptomen', 'kinderopvang', 'Thuis', 'School/opvang', 'Vrienden/kennissen', 'Oom/tante', 'Grootouders', 'Overige', 'Hoe heb je de opvang van je kinderen vandaag geregeld?', 'Lager/BUSO', 'Secundair', 'Hoger\nBachelor/Master', 'Doctoraat', 'Verdeling deelnemers per opleidingsniveau'))

add_translation(c('Percantage of participants reporting COVID19 symptoms', 'rapidly developing fever', 'high fever', 'sore throat', 'short of breath', 'dry cough', 'chesty cough', 'chestpain', 'running nose', 'muscle strain', 'fatiguee', 'shivers', 'nausea', 'sore eyes'),
                c('Percentage of participants reporting COVID-19 symptoms', 'Rapidly developing fever', 'High fever', 'Sore throat', 'short of breath', 'dry cough', 'chesty cough', 'chestpain', 'running nose', 'muscle strain', 'fatiguee', 'shivers', 'nausea', 'sore eyes'),
                c('Percentage deelnemers die COVID-19 symptomen rapporteren', 'Snel opkomende koorts', 'Hoge koorts', 'Keelpijn', 'Kortademigheid', 'Droge hoest', 'Rochelende hoest', 'Pijn op de borst', 'Lopende neus', 'Spierpijn', 'Vermoeidheid', 'Rillingen', 'Misselijkheid', 'Pijnlijke ogen'))

add_translation(c('fast_fever', 'high_fever', 'sore_throat', 'short_breath', 'dry_cough', 'deep_cough', 'chest_pain', 'runny_nose', 'muscle_ache', 'symptom_fatiguee', 'symptom_shivery', 'symptom_nausea', 'sore_eyes'),
                c('Rapidly developing fever', 'High fever', 'Sore throat', 'short of breath', 'dry cough', 'chesty cough', 'chestpain', 'running nose', 'muscle strain', 'fatiguee', 'shivery', 'nausea', 'sore eyes'),
                c('snel opkomende koorts', 'hoge koorts', 'keelpijn', 'kortademigheid', 'droge hoest', 'rochelende hoest', 'pijn op de borst', 'lopende neus', 'spierpijn', 'vermoeidheid', 'rillingen', 'misselijkheid', 'pijnlijke ogen'))


add_translation('About', 'About', 'Achtergrond')

add_translation(c('Participants', 'Epidemiology', 'Social distancing', 'Mental health', 'Corona Survey'),
                c('Participants', 'Epidemiology', 'Social distancing', 'Mental health', 'The big Corona study'),
                c('Deelnemers', 'Epidemiologie', 'Social distancing', 'Mentale gezondheid', 'De Grote Coronastudie'))

add_translation(c('Measures', 'Date', '0-17 year', '18-65 year', '65+ year', '18-70 year', '70+ year'),
                c('Measures', 'Date', '0-17 year', '18-65 year', '65+ year', '18-70 year', '70+ year'),
                c('Maatregelen', 'Datum', '0-17 jaar', '18-65 jaar', '65+ jaar', '18-70 jaar', '70+ jaar'))

add_translation(c('Percentage people of age', 'reporting increased tension with people of age', 'Increased tension by age group'),
                c('Percentage people of age', 'reporting increased tension with people of age', 'Increased tension by age group (Interpretation: Press ? bottom left)'),
                c('Percentage in leeftijdsgroep', 'die verhoogde spanningen rapporteren met leeftijdsgroep', 'Verhoogde spanning per leeftijdscategorie (Interpretatie: Klik ? links onderaan)'))

add_translation(c('Low', 'High', 'Prevalence', 'Legend: Time dependent', 'Legend: Fixed'),
                c('Low', 'High', 'Prevalence', 'Timevarying legend', 'Fixed legend'),
                c('Laag', 'Hoog', 'Prevalentie', 'Tijdsafhankelijke legende', 'Vaste legende'))

add_translation(c('Diagnosis received by patients with ', ' after docter examination', 'COVID19_unconfirmed', 'COVID19_cold_flu_unconfirmed', 'flu_confirmed', 'COVID19_confirmed', 'other'),
                c('Diagnosis received by patients with ', ' after docter examination', 'COVID-19\nunconfirmed', 'COVID-19 cold flu\nunconfirmed', 'Flu\nconfirmed', 'COVID-19\nconfirmed', 'Other'),
                c('Diagnose bij patiënten met ', ' na consultatie bij een dokter', 'COVID-19\nonbevestigd', 'COVID-19 verkoudheid griep\nonbevestigd', 'Griep\nbevestigd', 'COVID-19\nbevestigd', 'Overig'))

add_translation(c('Plot options', 'Results', "Age group", "Gender", "All", 'Direct statistics'),
                c('Plot options', 'Results', "Age group", "Gender", "All", 'Direct statistics'),
                c('Grafiek instellingen', 'Resultaten', "Leeftijdscategorie", "Geslacht", "Iedereen", 'Directe statistieken'))

add_translation('symptoms', 'symptoms', 'symptomen')

add_translation(c('Percentage reporting symptom', 'also reporting symptom', 'Relation between reported symptoms'), 
                c('Percentage reporting symptom', 'also reporting symptom', 'Relations between symptoms (Interpretation: Press ? bottom left)'),
                c('Percentage deelnemers met symptoom', 'die ook last hebben van', 'Relaties tussen symptomen (Interpretatie: Klik ? links onderaan)'))

add_translation(c('ghq', 'mean precovid', 'Low mental impact', 'High mental impact', 'ghq_title'),
                c('General Health Questionnaire (GHQ)', 'Average score Belgium 2018: 1.7', 'Low mental impact', 'High mental impact', 'Score (GHQ12) indicating reduced mental wellbeing (0: good - 12: severe reduction wellbeing)'),
                c('General Health Questionnaire (GHQ)', 'Gemiddelde score België 2018: 1.7', 'Lage mentale impact', 'Hoge mentale impact', 'Score (GHQ12) voor verminderd mentaal welzijn (0: goed - 12: sterke vermindering welzijn)'))

add_translation(c('Survey', 'Age category', 'Grouping'),
                c('Survey', 'Age category', 'Grouping'),
                c('Enquête', 'Leeftijdscategorie', 'Groepering'))

add_translation(c('Map symptoms', 'Participants coronastudy', 'Map participants', 'Symptom clusters', 'Social distancing', 'Intergenerational tensions', 'Mental health'),
                c('Spatial distribution symptoms', 'Participants big Corona study', 'Spatial distribution participants', 'Relations between symptoms', 'Social distancing', 'Intergenerational tensions', 'Mental health'),
                c('Geografische verdeling symptomen', 'Deelnemers Grote Coronastudie', 'Geografische verdeling deelnemers', 'Relaties tussen symptomen', 'Social distancing', 'Intergenerationele spanningen', 'Mentale gezondheid'))

add_translation(c('information/credits.html', 'info_map_symptoms.html', 'info_survey.html', 'info_map_participants.html', 'info_symptom_bar.html', 'info_symptom_heatmap.html', 'info_mitigation.html', 'info_psychology.html', 'info_tension_heatmap.html', 'info_ghq.html', 'info_financial_situation.html', 'info_topics.html', 'info_avg_contact.html', 'info_last_fysical_contact.html', 'info_radarchart.html'),
                c('information/credits_en.html', 'info_map_symptoms_en.html', 'info_survey_en.html', 'info_map_participants_en.html', 'info_symptom_bar_en.html', 'info_symptom_heatmap_en.html', 'info_mitigation_en.html', 'info_psychology_en.html', 'info_tension_heatmap_en.html', 'info_ghq_en.html', 'info_financial_situation_en.html', 'info_topics_en.html', 'info_avg_contact_en.html', 'info_last_fysical_contact_en.html', 'info_radarchart_en.html'),
                c('information/credits_nl.html', 'info_map_symptoms_nl.html', 'info_survey_nl.html', 'info_map_participants_nl.html', 'info_symptom_bar_nl.html', 'info_symptom_heatmap_nl.html', 'info_mitigation_nl.html', 'info_psychology_nl.html', 'info_tension_heatmap_nl.html', 'info_ghq_nl.html', 'info_financial_situation_nl.html', 'info_topics_nl.html', 'info_avg_contact_nl.html', 'info_last_fysical_contact_nl.html', 'info_radarchart_nl.html'))

add_translation(c('Direct statistics', 'Model predictions'),
                c('Direct statistics', 'Model predictions'),
                c('Directe statistieken', 'Model voorspellingen'))

add_translation(c('How have you arrange childcare today?'),
                c('How have you arranged childcare today?'),
                c('Hoe heb je de opvang van je kinderen vandaag geregeld?'))

add_translation(as.character(0:12), as.character(0:12), as.character(0:12))

add_translation(c('Society', 'avg contacts by province'), 
                c('Society', 'Average number of people spoken yesterday outside of your household'),
                c('Maatschappij', 'Gemiddeld aantal mensen gesproken gisteren buiten het gezin'))

add_translation(c('Very easy', 'Easy', 'Rather easy', 'Rather difficult', 'Difficult', 'Financial situation'),
                c('Very easy', 'Easy', 'Rather easy', 'Rather difficult', 'Difficult', 'Financial situation'),
                c('Zeer gemakkelijk', 'Gemakkelijk', 'Eerder gemakkelijk', 'Eerder moeilijk', 'Moeilijk', 'Financial situation'))

add_translation(c('Zeer gemakkelijk', 'Gemakkelijk', 'Eerder gemakkelijk', 'Eerder moeilijk', 'Moeilijk', 'Financial situation'),
                c('Very easy', 'Easy', 'Rather easy', 'Rather difficult', 'Difficult', 'Financial situation'),
                c('Zeer gemakkelijk', 'Gemakkelijk', 'Eerder gemakkelijk', 'Eerder moeilijk', 'Moeilijk', 'Financial situation'))


add_translation(c('alone', '2-4', '5+'),
                c('Living alone', '2-4', 'More than 5'),
                c('Alleenstaand', '2-4', 'Meer dan 5'))

add_translation(c('Neen', 'Ja, technisch werkloos', 'Ja, werkloos', 'Ja, eigen handelszaak/bedrijf moeten sluiten', 'Ja, nieuwe job'),
                c('No change', 'Technical unemployment', 'Lost job', 'Self employed\nCompany closed', 'New employment'),
                c('Onveranderd', 'Technisch werkloos', 'Werkloos geworden', 'Zelfstandige\nBedrijf gesloten', 'Nieuw werk'))

add_translation(c('Financial situation by household size', 'Financial situation by age group', 'Financial situation by employment evolution'),
                c('Financial situation by household size', 'Financial situation by age group', 'Financial situation by employment evolution'),
                c('Financiële situatie in functie van het aantal andere gezinsleden', 'Financiële situatie in functie van leeftijdscategorie', 'Financiële situatie in functie van de evolutie in tewerkstelling'))

add_translation(c('employment evolution', 'family size', "Financial situation participants"),
                c('employment evolution', 'household members', "Financial situation participants"),
                c('verandering werk', 'gezinsleden', "Financiële situatie deelnemers"))

add_translation(c('word education', 'economics', 'measures', 'family', 'word work', 'negative emotion', 'About what do participants write in the comments?'),
                c('Education', 'Economics', 'Measures', 'Family', 'Work', 'Negative emotions', 'About what do participants write in the comments?'),
                c('Onderwijs', 'Geld', 'Maatregelen', 'Familie', 'Werk', 'Negatieve emoties', 'Waarover schrijven deelnemers in de opmerkingen?'))

add_translation(c('Distribution of participants by number of other family members'),
                c('Distribution of participants by number of other family members'),
                c('Verdeling deelnemers per aantal andere gezinsleden'))

add_translation(c('Last fysical contact', 'Average number of contacts by province', 'comments participants'),
                c('Date last fysical contact', 'Number of contacts by province', 'What do participants tell us?'),
                c('Datum laatste fysieke contact', 'Aantal gesprekken per provincie', 'Waarover schrijven deelnemers in de opmerkingen?'))

add_translation(c('Evolution of symptoms', 'radarchart'),
                c('Symptom evolution (Interpretation: Press ? bottom left)', 'Symptom evolution'),
                c('Evolutie van symptomen (Interpretatie: Klik ? links onderaan)', 'Evolutie van symptomen'))

add_translation(c('Filter', 'Man', 'Vrouw'),
                c('Filter', 'Male', 'Female'),
                c('Filter', 'Man', 'Vrouw'))

add_translation(c('Family size', 'COVID-19 diagnosis', 'Fysical contact', 'Financial situation', 'two persons', '3-5 persons', '5+ persons', 'diagnosis_covid', 'diagnosis_potential_covid', 'diagnosis_other', 'symptoms_no_diagnosis', 'no symptoms', 'no fysical contact', 'fysical contact'),
                c('Family size', 'COVID-19 diagnosis', 'Fysical contact', 'Financial situation', 'Two persons', '3-5 persons', '5+ persons', 'Diagnosis: COVID-19', 'Diagnosis: maybe COVID-19', 'Diagnosis: no COVID-19', 'Symptoms without diagnosis', 'Geen symptomen', 'No fysical contact', 'Fysical contact'),
                c('Gezinsleden', 'COVID-19 diagnose', 'Fysiek contact', 'Financiële situatie', 'Twee personen', '3-5 personen', '5+ personen', 'Diagnose: COVID-19', 'Diagnose: misschien COVID-19', 'Diagnose: geen COVID-19', 'Symptomen zonder diagnose', 'Geen symptomen', 'Geen fysiek contact', 'Fysiek contact'))

write.csv(translation_en, 'translations/translation_en.csv', row.names = FALSE, fileEncoding = 'UTF-8')
write.csv(translation_nl, 'translations/translation_nl.csv', row.names = FALSE, fileEncoding = 'UTF-8')

write.table(translation_nl, 'translations/translation_p_fr.csv', row.names = FALSE, fileEncoding = 'UTF-8', sep=';')
