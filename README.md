# surveydesignr

Package contains tools to support design and use of questionnaires programmed in [XLSForm](https://xlsform.org/en/).

## Download and install package
Download the library from Github:
```{r}
devtools::install_github("williameoswald/surveydesignr")
```

Load libraries:
```{r}
library("surveydesignr")
library("tidyverse")
library("janitor")
library("readxl")
library("flextable")
library("stringr")
```

## export_quest

Function for importing a [XLSForm](https://xlsform.org/en/) and creating a readable dataframe or flextable object that lists variable names, labels in up to two languages, relevancies, constraints, and calculate fields. Output objects can be easily exported to Microsoft Word using Officer, ultimately allowing for questionnaires to be directly programmed in XLSform and then easily shared and reviewed by collaborators or included in protocol or other text documents.

### Your inputs

If XLSform includes multiple languages, you can choose the language to output by specifying the primary argument. For example, specifying "English" as the primary argument means function will use columns containing "label::English", "hint::English", etc., ignoring capitalisation. To output a second language side-by-side, specify the primary argument and a second language as the secondary argument. Please note that if your label column name includes spaces or other punctuation (e.g. "label::English (en)") the function will remove these and replace with underscores ("_"), so you will need to input "English_en".

Setting both primary and secondary to NULL will allow you to output a table for forms without any language specified, i.e. with columns "label","hint", etc.


```{r}
xlsformfile <- "filepath to XLSForm here"

# Specify flex=TRUE (default) to output the table as a formatted flextable object.
quest_table <- export_quest(xlsformfile,primary="English",secondary=NULL)

# Specify flex=FALSE to output the table as a tibble.
quest_table <- export_quest(xlsformfile,primary="English",secondary=NULL,flex=FALSE)
```

### Output table to Word document using Officer
```{r}
library("officer")
library("flextable")
text_style <- fp_text(font.size = 12, font.family = "Calibri", bold = TRUE)
par_style <- fp_par(text.align = "justify")

read_docx() %>% body_remove() %>% 
  body_add_flextable(quest_table) %>%
  print(target = paste0("questionnaire.docx"))

```

## compareXLSForms

Function for comparing two [XLSForm Docs](https://xlsform.org/en/). Currently designed for use with SurveyCTO XLSForm files but should be usable or modifiable for use with ODK forms. 

### Your inputs

Please ensure the "label", "hint", "constraint message" columns are named "label:English", "hint:English", and "constraint message:English". Additional language columns (only one additional language at a time for now) can be examined by specifying them as a third argument according to how the language is named in the form (e.g. "Francais" for label:Francais).

```{r}
form1location <- "filepath to first XLSForm here"
form2location <- "filepath to second XLSForm here"

# Name output of function "full_compare""
full_compare <- compare_survey(form1location,form2location,language)

# Flextable of content comparison
tabulate_comparison(full_compare,1)
```

### compare_survey function tabulates differences between forms for the following checks:
1. Compare content - list items not present in both survey sheets
2. Get row number for each item in form 1 relative to same item's position in form 2
3. Compare type per name
4. Compare calculation fields for calculate items
5. Compare all constraint fields
6. Compare all relevance fields
7. Compare English labels
8. Compare English constraint messages
9. Compare English hints
10. Compare Other language labels
11. Compare Other language constraint messages
12. Compare Other language constraint messages

### Output results from checks
```{r}
tabulate_comparison(1)
tabulate_comparison(2)
tabulate_comparison(3)
tabulate_comparison(4)
tabulate_comparison(5)
tabulate_comparison(6)
tabulate_comparison(7)
tabulate_comparison(8)
tabulate_comparison(9)
tabulate_comparison(10)
tabulate_comparison(11)
tabulate_comparison(12)
```

### To do:
 - Add display of question type (select_one, select_multiple) and constraint message to table
 - Revise comparison function to allow for forms without language specification
 - Add comparison of choices tab
 - Make cleaner output
 - Functionalise repeated actions 

## Contributors

Please contact [me](https://www.linkedin.com/in/william-oswald-17726919/) with any questions or suggestions.

## License

Available for use under a CC BY-NC-SA 4.0 license (https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode).
