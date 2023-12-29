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
    # Rename fields for SurveyCTO/ODK compatibility
    dplyr::rename(any_of(c(relevant="relevance",read_only="read only"))) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(rownbr = 1:dplyr::n()) %>%
    # dplyr::select(-"required message", -"read only", -contains("media"), -minimum_seconds, -note, -response_note) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(flag_dup_row =
                    dplyr::case_when(
                      dplyr::n()>1 ~ 1L,
                      TRUE ~ 0L),
                  dup_row_seq = 1:dplyr::n(),
                  name =
                    dplyr::case_when(
                      flag_dup_row==1 ~ paste0(name,"_",dup_row_seq),
                      TRUE ~ name)) %>%
    dplyr::ungroup()
  
  survey2 <- readxl::read_xlsx(form2, sheet="survey") %>%
    dplyr::rename_all(tolower) %>%
    # Rename fields for SurveyCTO/ODK compatibility
    dplyr::rename(any_of(c(relevant="relevance",read_only="read only"))) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(rownbr = 1:dplyr::n()) %>%
    # dplyr::select(-"required message", -"read only", -contains("media"), -minimum_seconds, -note, -response_note) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(flag_dup_row =
                    dplyr::case_when(
                      dplyr::n()>1 ~ 1L,
                      TRUE ~ 0L),
                  dup_row_seq = 1:dplyr::n(),
                  name =
                    dplyr::case_when(
                      flag_dup_row==1 ~ paste0(name,"_",dup_row_seq),
                      TRUE ~ name)) %>%
    dplyr::ungroup()
  
  print("Check 0 - Compare content")
  compare_cols <- dplyr::union(dplyr::setdiff(names(survey1),names(survey2)),
                               dplyr::setdiff(names(survey2),names(survey1)))
  
  if(length(compare_cols)==0){
    # Merge survey sheets by name field
    compare <- dplyr::full_join(survey1, survey2, by="name")
    
    # Check 1 - Compare content - list items not present in both survey sheets
    # List all values in name field in form 1
    survey1_items <- survey1 %>% dplyr::select(name) %>% unlist
    # List all values in name field in form 2
    survey2_items <- survey2 %>% dplyr::select(name) %>% unlist
    # Build tibble with discrepant values in name fields between forms
    items_different <- dplyr::as_tibble(dplyr::bind_cols(difference = dplyr::setdiff(survey1_items,survey2_items),
                                                         in_survey1 = is.element(dplyr::setdiff(survey1_items,survey2_items),survey1_items),
                                                         in_survey2 = is.element(dplyr::setdiff(survey1_items,survey2_items),survey2_items)))
    
    if (nrow(items_different)>0) {
      print("Not in both forms:")
      print(items_different)
    }
    if (nrow(items_different)==0) {
      print("No difference between included items")
    }
    
    print("Check 2 - Get row number for each item in form 1 relative to same item's position in form 2")
    position_different <- compare %>%
      dplyr::filter(!is.na(rownbr.x)) %>%
      dplyr::mutate(position_difference=rownbr.x-rownbr.y) %>%
      dplyr::select(name,rownbr.x,position_difference) %>%
      dplyr::filter(position_difference!=0)
    
    if (nrow(position_different)>0) {
      print("Differences between survey 1 item positions and survey 2:")
      print(position_different)
    }
    if (nrow(position_different)==0) {
      print("No difference in item positions")
    }
    
    print("Check 3 - Compare type per name")
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
    
    print("Check 4 - Compare calculation fields for calculate row")
    if("calculation" %in% names(survey1)){
      calc_different <- dplyr::full_join(survey1, survey2, by=c("type","name")) %>%
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
    } else{
      print("No calculation fields in specified items")
      calc_different <- tibble()
    }
  
    print("Check 5 - Compare all constraint fields")
    if("constraint" %in% names(survey1)){
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
    } else{
      print("No constraint fields in specified items")
      constraint_different <- tibble()
    }
     
    print("Check 6 - Compare all relevance fields")
    relevance_different <- compare %>%
      dplyr::filter(!is.na(relevant.x),
             !is.na(relevant.y),
             relevant.x!=relevant.y) %>%
      dplyr::select(name,relevant.x,relevant.y)
    
    if (nrow(relevance_different)>0) {
      print("Discrepancies exist in relevance fields for specified items")
      print(relevance_different)
    }
    if (nrow(relevance_different)==0) {
      print("No discrepancies in relevance fields for specified items")
    }
    
    print("Check 7 - Compare English labels")
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
    
    print("Check 8 - Compare English constraint messages")
    if("constraint" %in% names(survey1)){
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
    } else{
      print("No constraint fields in specified items")
      consmess_different <- tibble()
      }
    
    print("Check 9 - Compare English hints")
    if("hint" %in% names(survey1)){
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
    } else{
      print("No hint fields in specified items")
      hint_different <- tibble()
    }
    
    if (!missing(language)) {
      print(paste0("Check 10 - Compare ",tolower(language)," language labels"))
      labelx <- dplyr::sym(paste0("label:",tolower(language),".x"))
      labely <- dplyr::sym(paste0("label:",tolower(language),".y"))
      
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
      
      print(paste0("Check 11 - Compare ",tolower(language)," constraint messages"))
      constraintx <- dplyr::sym(paste0("constraint message:",tolower(language),".x"))
      constrainty <- dplyr::sym(paste0("constraint message:",tolower(language),".y"))
      
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
      
      print(paste0("Check 12 - Compare ",tolower(language)," hint\n"))
      hintx <- dplyr::sym(paste0("hint:",tolower(language),".x"))
      hinty <- dplyr::sym(paste0("hint:",tolower(language),".y"))
      
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
    
  } else if(length(compare_cols)!=0){
    print("Columns do not match between forms. Not present in both forms:")
    print(compare_cols)
  }
}
