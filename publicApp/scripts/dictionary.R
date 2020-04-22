dictionary <- merge(
  read.csv('translations/translation_nl.csv', fileEncoding = 'UTF-8'),
  read.csv('translations/translation_en.csv', fileEncoding = 'UTF-8'),
  by = 'ex')

translate <- function(code) {
  index <- match(code, dictionary[, 1])
  return(as.character(dictionary[index, active_language]))
}

set_language <- function(language_code) {
  active_language <<- match(language_code, colnames(dictionary))
}

