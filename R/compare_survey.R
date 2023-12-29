#' Compares survey sheets from two xlsform files
#'
#' This function imports two XLSForm files and currently conducts a number of checks to compare each of the "survey" sheets
#' @param form1 Filepath of first form as text or referring to object with location.
#' @param form2 Filepath of second form as text or referring to object with location.
#' @param language Specify any secondary language of the form. Form can have more than one, but this will only check one at a time. (e.g. "Francais") Entered text must match language specification in the XLSForm columns.
#' @return List of data.frames with results of each "check" in tibbles.
#' @export
#' @examples
#' \dontrun{compare_forms(form1location,form2location,"Francais")}

compare_survey <- function(form1, form2, language){

  # Import forms, flags row numbers, keeps selected variables, and renames any groups or repeats with same name value to make unique using order number
  survey1 <- readxl::read_xlsx(form1, sheet="survey") %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(rownbr = 1:n()) %>%
    dplyr::select(-"required message", -"read only", -contains("media"), -minimum_seconds, -note, -response_note) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(flag_dup_row =
             case_when(
               n()>1 ~ 1L,
               TRUE ~ 0L),
           dup_row_seq = 1:n(),
           name =
             case_when(
               flag_dup_row==1 ~ paste0(name,"_",dup_row_seq),
               TRUE ~ name)) %>%
    dplyr::ungroup()

  survey2 <- readxl::read_xlsx(form2, sheet="survey") %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(rownbr = 1:n()) %>%
    dplyr::select(-"required message", -"read only", -contains("media"), -minimum_seconds, -note, -response_note) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(flag_dup_row =
                    case_when(
                      n()>1 ~ 1L,
                      TRUE ~ 0L),
                  dup_row_seq = 1:n(),
                  name =
                    case_when(
                      flag_dup_row==1 ~ paste0(name,"_",dup_row_seq),
                      TRUE ~ name)) %>%
    dplyr::ungroup()

  # Compare survey sheet
  compare <- dplyr::full_join(survey1, survey2, by="name")

  cat(crayon::bold(crayon::blue(print("Check 1 - Compare content - list items not present in both survey sheets\n"))))
  survey1_items <- survey1 %>% dplyr::select(name) %>% unlist
  survey2_items <- survey2 %>% dplyr::select(name) %>% unlist

  items_different <- dplyr::as_tibble(dplyr::bind_cols(difference = setdiff(survey1_items,survey2_items),
                                         in_survey1 = is.element(setdiff(survey1_items,survey2_items),survey1_items),
                                         in_survey2 = is.element(setdiff(survey1_items,survey2_items),survey2_items)))

  if (length(items_different)>0) {
    print("Not in both forms:")
    print(items_different)
  }
  if (length(items_different)==0) {
    print("No difference between included items")
  }

  cat(crayon::bold(crayon::blue(print("Check 2 - Get row number for each item in form 1 relative to same item's position in form 2\n"))))
  position_different <- compare %>%
    dplyr::filter(!is.na(rownbr.x)) %>%
    dplyr::mutate(position_difference=rownbr.x-rownbr.y) %>%
    dplyr::select(name,rownbr.x,position_difference) %>%
    dplyr::filter(position_difference!=0)

  if (length(position_different)>0) {
    print("Differences between survey 1 item positions and survey 2:")
    print(position_different)
  }
  if (length(position_different)==0) {
    print("No difference in item positions")
  }

  cat(crayon::bold(crayon::blue(print("Check 3 - Compare type per name\n"))))
  types_different <- compare %>%
    dplyr::filter(type.x!=type.y) %>%
    dplyr::select(name,type.x,type.y) %>%
    dplyr::relocate(name, .before = type.x)

  if (nrow(types_different)>0) {
    print("Discrepancies exist in type for fields with same name:")
    print(types_different)
  }
  if (nrow(types_different)==0) {
    print("No discrepancies in type by name")
  }

  cat(crayon::bold(crayon::blue(print("Check 4 - Compare calculation fields for calculate row\n"))))
  calc_different <- full_join(survey1, survey2, by=c("type","name")) %>%
    dplyr::filter(type=="calculate",
           calculation.x!=calculation.y) %>%
    dplyr::select(type,name,calculation.x,calculation.y)

  if (nrow(calc_different)>0) {
    print("Discrepancies exist in calculation fields for specified items")
    print(calc_different)
  }
  if (nrow(calc_different)==0) {
    print("No discrepancies in calculation fields for specified items")
  }

  cat(crayon::bold(crayon::blue(print("Check 5 - Compare all constraint fields\n"))))
  constraint_different <- compare %>%
    dplyr::filter(!is.na(constraint.x),
           !is.na(constraint.y),
           constraint.x!=constraint.y) %>%
    dplyr::select(name,constraint.x,constraint.y)

  if (nrow(constraint_different)>0) {
    print("Discrepancies exist in constraint fields for specified items")
    print(constraint_different)
  }
  if (nrow(constraint_different)==0) {
    print("No discrepancies in constraint fields for specified items")
  }

  cat(crayon::bold(crayon::blue(print("Check 6 - Compare all relevance fields\n"))))
  relevance_different <- compare %>%
    filter(!is.na(relevance.x),
           !is.na(relevance.y),
           relevance.x!=relevance.y) %>%
    select(name,relevance.x,relevance.y)

  if (nrow(relevance_different)>0) {
    print("Discrepancies exist in relevance fields for specified items")
    print(relevance_different)
  }
  if (nrow(relevance_different)==0) {
    print("No discrepancies in relevance fields for specified items")
  }

  cat(crayon::bold(crayon::blue(print("Check 7 - Compare English labels\n"))))
  labels_different <- compare %>%
    dplyr::filter(!is.na(`label:english.x`),
           !is.na(`label:english.y`),
           `label:english.x`!=`label:english.y`) %>%
    dplyr::select(name,`label:english.x`,`label:english.y`)

  if (nrow(labels_different)>0) {
    print("Discrepancies exist in English labels for specified items")
    print(labels_different)
  }
  if (nrow(labels_different)==0) {
    print("No discrepancies in English labels for specified items")
  }

  cat(crayon::bold(crayon::blue(print("Check 8 - Compare English constraint messages\n"))))
  consmess_different <- compare %>%
    dplyr::filter(!is.na(`constraint message:english.x`),
           !is.na(`constraint message:english.y`),
           `constraint message:english.x`!=`constraint message:english.y`) %>%
    dplyr::select(name,`constraint message:english.x`,`constraint message:english.y`)

  if (nrow(consmess_different)>0) {
    print("Discrepancies exist in English constraint messages for specified items")
    print(consmess_different)
  }
  if (nrow(consmess_different)==0) {
    print("No discrepancies in English constraint messages for specified items")
  }

  cat(crayon::bold(crayon::blue(print("Check 9 - Compare English hints\n"))))
  hint_different <- compare %>%
    dplyr::filter(!is.na(`hint:english.x`),
           !is.na(`hint:english.y`),
           `hint:english.x`!=`hint:english.y`) %>%
    dplyr::select(name,`hint:english.x`,`hint:english.y`)

  if (nrow(hint_different)>0) {
    print("Discrepancies exist in English constraint messages for specified items")
    print(hint_different)
  }
  if (nrow(hint_different)==0) {
    print("No discrepancies in English constraint messages for specified items")
  }

  if (!missing(language)) {
    cat(crayon::bold(crayon::blue(print(paste0("Check 10 - Compare ",tolower(language)," language labels\n")))))
    labelx <- sym(paste0("label:",tolower(language),".x"))
    labely <- sym(paste0("label:",tolower(language),".y"))

    otherlabels_different <- compare %>%
      dplyr::filter(!is.na(!!labelx),
             !is.na(!!labely),
             !!labelx!=!!labely) %>%
      dplyr::select(name,!!labelx,!!labely)

    if (nrow(otherlabels_different)>0) {
      print("Discrepancies exist in labels for specified items")
      print(otherlabels_different)
    }
    if (nrow(otherlabels_different)==0) {
      print("No discrepancies in labels for specified items")
    }

    cat(crayon::bold(crayon::blue(print(paste0("Check 11 - Compare ",tolower(language)," constraint messages\n")))))
    constraintx <- sym(paste0("constraint message:",tolower(language),".x"))
    constrainty <- sym(paste0("constraint message:",tolower(language),".y"))

    otherconsmess_different <- compare %>%
      dplyr::filter(!is.na(!!constraintx),
             !is.na(!!constrainty),
             !!constraintx!=!!constrainty) %>%
      dplyr::select(name,!!constraintx,!!constrainty)

    if (nrow(otherconsmess_different)>0) {
      print("Discrepancies exist in constraint messages for specified items")
      print(otherconsmess_different)
    }
    if (nrow(otherconsmess_different)==0) {
      print("No discrepancies in constraint messages for specified items")
    }

    cat(crayon::bold(crayon::blue(print(paste0("Check 12 - Compare ",tolower(language)," hint\n")))))
    hintx <- sym(paste0("hint:",tolower(language),".x"))
    hinty <- sym(paste0("hint:",tolower(language),".y"))

    otherhint_different <- compare %>%
      dplyr::filter(!is.na(!!hintx),
             !is.na(!!hinty),
             !!hintx!=!!hinty) %>%
      dplyr::select(name,!!hintx,!!hinty)

    if (nrow(otherhint_different)>0) {
      print("Discrepancies exist in hints for specified items")
      print(otherhint_different)
    }
    if (nrow(otherhint_different)==0) {
      print("No discrepancies in hints for specified items")
    }
    full_compare <- list(items_different,position_different,types_different,calc_different,constraint_different,relevance_different,labels_different,consmess_different,hint_different,otherlabels_different,otherconsmess_different,otherhint_different)
    names(full_compare) <- c("Check content","Check positions","Check types","Check calculations","Check constraints","Check relevancies","Check labels","Check constraint messages","Check hints","Check other labels","Check other constraint Messages","Check other hints")
  }
  if (missing(language)) {
    full_compare <- list(items_different,position_different,types_different,calc_different,constraint_different,relevance_different,labels_different,consmess_different,hint_different)
    names(full_compare) <- c("Check content","Check positions","Check types","Check calculations","Check constraints","Check relevancies","Check labels","Check constraint messages","Check hints")
  }

  return(full_compare)

}
