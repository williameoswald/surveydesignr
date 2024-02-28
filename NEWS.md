# surveydesignr 0.1.1

* Fixed name/value ODK/SurveyCTO compatibility in choices sheet and used janitor to remove "::" throughout.

# surveydesignr 0.1.2

* Added "get_quest" function to "export_quest" to simplify approach and avoid repetition.
* Added display of "SELECT ONE" or "SELECT MULTIPLE" to "export_quest" to display choice type.
* Retained "description" column in "export_quest" output for more flexible selection of content using tibble (i.e. flex=F) and improved legibility.
* Default now primary=NULL for "export_quest".
