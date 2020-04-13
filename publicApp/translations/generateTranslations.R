translation_en <- data.frame(ex = c(''), en = c(''));
translation_nl <- data.frame(ex = c(''), nl = c(''));

add_translation <- function(txt_ex, txt_en, txt_nl) {
  translation_en <<- rbind(translation_en, data.frame(ex = txt_ex, en = txt_en))
  translation_nl <<- rbind(translation_nl, data.frame(ex = txt_ex, nl = txt_nl))
}

add_translation('Belgium', 'Belgium', 'België')
add_translation(c('Antwerpen', 'Brussel', 'Henegouwen', 'Limburg', 'Luik', 'Luxemburg', 'Namen', 'Oost-Vlaanderen', 'Vlaams-Brabant', 'Waals-Brabant', 'West-Vlaanderen'),
                c('Antwerp', 'Brussels', 'Hainaut', 'Limburg', 'Liège', 'Luxembourg', 'Namur', 'East Flanders', 'Flemish Brabant', 'Walloon Brabant', 'West Flanders'),
                c('Antwerpen', 'Brussel', 'Henegouwen', 'Limburg', 'Luik', 'Luxemburg', 'Namen', 'Oost-Vlaanderen', 'Vlaams-Brabant', 'Waals-Brabant', 'West-Vlaanderen'))
add_translation(c('April 7', 'March 31', 'March 24', 'March 17'),
                c('April 7', 'March 31', 'March 24', 'March 17'),
                c('7 april', '31 maart', '24 maart', '17 maart'))

add_translation('Distribution participants by province',
                'Distribution participants by province',
                'Verdeling deelnemers per provincie')

add_translation(c('Probability of having symptoms',
                  'percentage'),
                c('Probability of having at least 1 COVID-19 symptom',
                  'percentage'),
                c('Kans op minstens één COVID-19 symptoom',
                  'percentage'))

map <- st_zm(read_sf('shapefiles/predict_pc_simplified/shapefile_predict_pc.shp', quiet = TRUE))

add_translation(map$naam_nl,
                map$naam_nl,
                map$naam_nl)

add_translation(c('response', 'age', 'gender', 'work', 'behaviour work', 'behaviour public', 'behaviour household', 'conversation', 'fysical contact', 'sleep', 'concentrate', 'optimistic', 'tension'),
                c('participants', 'age', 'gender', 'work', 'behaviour work', 'behaviour public', 'behaviour household', 'conversation', 'fysical contact', 'sleep', 'concentrate', 'optimistic', 'tension'),
                c('deelnemers', 'leeftijd', 'geslacht', 'werk', 'gedrag werk', 'gedrag publieke plaatsen', 'gedrag gezin', 'conversatie', 'fysiek contact', 'slaap', 'concentratie', 'optimisme', 'spanning'))

add_translation(c('survey_response', 'responses', 'distribution_gender', 'Female', 'Male', 'Other', 'distribution_age'),
                c('Number of participants per survey (x1000)', 'participants', 'Distribution of participants per gender', 'Female', 'Male', 'Other', 'Distribution of participants by age group'),
                c('Aantal deelnemers per enquête (x1000)', 'deelnemers', 'Verdeling deelnemers per geslacht', 'Vrouw', 'Man', 'Overig', 'Verdeling deelnemers per leeftijdscategorie'))

add_translation(c('Current working conditions', 'protected', 'unprotected', 'homework', 'Change in behaviour at work', 'Change in behaviour at public places', 'Change in behaviour at home', 'Number of people spoken today (not virtual)', 'None', 'one', '2-5', '6-20', '20+', 'last fysical contact (handshake or kiss)', 'today', 'yesterday', '2-6 days ago', '7+ days ago'),
                c('Current working conditions', 'Protected', 'Unprotected', 'Homework', 'Change in behaviour at work', 'Change in behaviour at public places', 'Change in behaviour at home', 'Number of people spoken today (not virtual)', 'None', 'One', '2-5', '6-20', '20+', 'last fysical contact (handshake or kiss)', 'Today', 'Yesterday', '2-6 days ago', '7+ days ago'),
                c('Huidige werk omstandigheden', 'Beschermd', 'Onbeschermd', 'Thuiswerk', 'Verandering in gedrag op het werk', 'Verandering in gedrag op openbare plaatsen', 'Verandering in gedrag binnen het gezin', 'Aantal mensen gesproken vandaag (niet virtueel)', 'Geen', 'Eén', '2-5', '6-20', '20+', 'Laatste fysieke contact (hand of kus)', 'Vandaag', 'Gisteren', '2-6 dagen geleden', '7+ dagen geleden'))

add_translation(c('Sleep quality last week', 'Better', 'Same', 'Somewhat worse', 'Much worse', 'Concentration last week', 'Optimism last week', 'More', 'A bit less', 'Much less', 'Tension last week', 'None', 'Same', 'Somewhat more', 'Much more'),
                c('How was your sleep quality last week', 'Better', 'Same', 'Somewhat worse', 'Much worse', 'Concentration last week', 'Optimism last week', 'More', 'A bit less', 'Much less', 'Tension last week', 'None', 'Same', 'A bit more', 'Much more'),
                c('Wat was de kwaliteit van je slaap afgelopen week?', 'Beter', 'Zelfde', 'Beetje slechter', 'Veel slechter', 'Hoe kon je je concentreren afgelopen week?', 'Hoe optimistisch was je afgelopen week?', 'Meer', 'Een beetje minder', 'Veel minder', 'Heb je spanningen ervaren afgelopen week?', 'Nee', 'Evenveel als normaal', 'Een beetje meer', 'Veel meer'))

add_translation(c('education', 'symptom', 'childcare', 'home', 'school/childcare', 'friends/acquaintances', 'aunts/uncles', 'grandparents', 'other', 'arrangement childcare', 'Primary education', 'Secondary education', 'Higher education', 'PhD', 'Distribution of participants by level of education'),
                c('Education', 'Symptom', 'Childcare', 'Home', 'School/childcare', 'Friends/acquaintances', 'Aunts/uncles', 'Grandparents', 'Other', 'How did you arrange childcare today?', 'Primary', 'Secondary', 'Higher', 'PhD', 'Distribution of participants by level of education'),
                c('Opleiding', 'Symptomen', 'Kinderopvang', 'Thuis', 'School/opvang', 'Vrienden/kennissen', 'Oom/tante', 'Grootouders', 'Overige', 'Hoe heb je de opvang van je kinderen vandaag geregeld?', 'Lager', 'Secundair', 'Hoger', 'PhD', 'Verdeling deelnemers per opleidingsniveau'))

add_translation(c('Percantage of participants reporting COVID19 symptoms', 'rapidly developing fever', 'high fever', 'sore throat', 'short of breath', 'dry cough', 'chesty cough', 'chestpain', 'running nose', 'muscle strain', 'fatiguee', 'shivers', 'nausea', 'sore eyes'),
                c('Percantage of participants reporting COVID19 symptoms', 'Rapidly developing fever', 'High fever', 'Sore throat', 'short of breath', 'dry cough', 'chesty cough', 'chestpain', 'running nose', 'muscle strain', 'fatiguee', 'shivers', 'nausea', 'sore eyes'),
                c('Percantage deelnemers die COVID19 symptomen rapporteren', 'Snel opkomende koorts', 'Hoge koorts', 'Keelpijn', 'Kortademigheid', 'Droge hoest', 'Rochelende hoest', 'Pijn op de borst', 'Lopende neus', 'Spierpijn', 'Vermoeidheid', 'Rillingen', 'Misselijkheid', 'Pijnlijke ogen'))

add_translation('About', 'About', 'Achtergrond')

write.csv(translation_en, 'translations/translation_en.csv', row.names = FALSE)
write.csv(translation_nl, 'translations/translation_nl.csv', row.names = FALSE)

