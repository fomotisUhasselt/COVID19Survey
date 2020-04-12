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

write.csv(translation_en, 'translations/translation_en.csv', row.names = FALSE)
write.csv(translation_nl, 'translations/translation_nl.csv', row.names = FALSE)
