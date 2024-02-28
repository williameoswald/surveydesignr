#' Export dataframe or flextable with readable version of xlsform questionnaire
#'
#' This function imports a XLSForm file and exports either a dataframe (tibble) or a flextable object with a readable version of questionnaire.
#' @param xlsformfile Filepath of xlsform as text or referring to object with filepath.
#' @param primary Main language defined in form. Specified as "label:English", etc. Use primary=NULL (default) if no language specified ("label").
#' @param secondary Specify any secondary language of the form. Form can have more than one, but this will only check one at a time. (e.g. "Francais") Entered text must match language specification in the XLSForm columns.
#' @param flex Specify whether flextable object is output (default) or tibble object
#' @return Large flextable or tibble.
#' @importFrom magrittr "%>%"
#' @export
export_quest <- function(xlsformfile,primary=NULL,secondary=NULL,flex=TRUE){

  # Call languages in lower case
  if(!is.null(primary)) {
    primary <- stringr::str_to_lower(primary)
  }
  if(!is.null(secondary)) {
    secondary <- stringr::str_to_lower(secondary)
  }  
  
  # Import xlsform survey sheet
  xlsform <- readxl::read_xlsx(xlsformfile, sheet = "survey") %>%
    # Leave repeat and group in to flag these in text version, "end" rows dropped as blank labels
    # Rename fields for SurveyCTO/ODK compatibility
    dplyr::rename(any_of(c(relevant="relevance",read_only="read only"))) %>%
    # Replace colons with underscore
    janitor::clean_names() %>% 
    dplyr::mutate(choices=dplyr::if_else(stringr::str_detect(type,"select"),
                                              stringr::str_sub(type, start=stringr::str_locate(type," ")[,1]+1, end=-1L),
                                              NA),
                  choice_type=dplyr::if_else(stringr::str_detect(type,"select"),
                                             stringr::str_sub(type, start=stringr::str_locate(type,"_")[,1]+1, end=stringr::str_locate(type," ")[,1]-1),
                                             NA),
                  choice_type=dplyr::if_else(stringr::str_detect(choice_type,"one"),"SELECT ONE","SELECT MULTIPLE"))

  # Specify flextable formatting
  format_table <- function(flextab){
    flextab %>%
      flextable::flextable() %>%
      flextable::align(align="left", part="header") %>%
      flextable::fontsize(size=10, part="all") %>%
      flextable::font(fontname="Calibri", part="all") %>%
      flextable::bold(bold=TRUE, part="header") %>%
      flextable::bold(~ description=="Label:",bold=TRUE, 2) %>%
      flextable::bold(i=NULL,j="description", bold=TRUE, part="body") %>%
      flextable::align(i=NULL,j="description", align="right", part="body") %>%
      flextable::align(i=NULL,j="name", align="left", part="all") %>%
      flextable::set_table_properties(layout="autofit") %>%
      flextable::height(height=0.7, part="body", unit="cm") %>%
      flextable::hline(i = ~is_last_val_in_group == TRUE) %>%
      flextable::delete_columns(c("is_last_val_in_group")) %>%
      flextable::set_caption(paste0(stringr::str_to_title(readxl::read_xlsx(xlsformfile, sheet = "settings") %>%
                                                            dplyr::pull(form_title)),
                                    ", Version: ",
                                    readxl::read_xlsx(xlsformfile, sheet = "settings") %>%
                                      dplyr::select(any_of(c("version"))) %>% 
                                      {
                                        if("version" %in% names(.)) dplyr::pull(.) else .
                                      }),
                             style = "Table Caption")
  }

  # Get questionnaire function - 
  # Creates dataframe with questions, choices, relevance, constraints, and calcs
  # based on specified language, e.g.  NULL or "english"
  get_quest <- function(language){
    
    labeltxt <- paste0("label",dplyr::if_else(is.null(language),"","_"),language)
    hinttxt <- paste0("hint",dplyr::if_else(is.null(language),"","_"),language)
    cmtxt <- paste0("constraint_message",dplyr::if_else(is.null(language),"","_"),language)
    labeltxt_ <- dplyr::ensym(labeltxt)
    
    quest <- xlsform %>%
      dplyr::select(name,any_of(c(matches(labeltxt),matches(hinttxt),matches(cmtxt))),
                    any_of(c("relevant","read_only","calculation","constraint")),
                    choice_type,choices) %>% 
      dplyr::relocate(any_of(matches("constraint")),.after = tidyselect::last_col()) %>% 
      dplyr::relocate(any_of(contains("constraint_message")),.after = any_of(c("constraint"))) %>% 
      tidyr::pivot_longer(cols = -c(name),
                          names_to = "description") %>% 
      dplyr::filter(!is.na(value)) %>% 
      dplyr::left_join(readxl::read_xlsx(xlsformfile, sheet = "choices") %>%
                         # Rename fields for SurveyCTO/ODK compatibility, name used in ODK
                         dplyr::rename(any_of(c(name="value"))) %>%
                         # Replace colons with underscore
                         janitor::clean_names() %>% 
                         dplyr::filter(!is.na(list_name)) %>%
                         dplyr::select(list_name,name,!!labeltxt_) %>%
                         dplyr::rename(label=!!labeltxt_) %>%
                         dplyr::mutate(choice=paste0(as.character(name)," ",label)) %>%
                         dplyr::select(list_name,choice),
                       by=c("value"="list_name"),
                       relationship = "many-to-many") %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(value=dplyr::if_else(description=="choices",
                                         choice,
                                         value),
                    description=dplyr::if_else(is.null(language),
                                               description,
                                               stringr::str_replace(description,paste0("_",language),""))) %>% 
      dplyr::select(-choice) %>%
      dplyr::relocate(name,description,value) 
    
      return(quest)
  }
  
  # No language specified (i.e. language=NULL) or only PRIMARY specified (i.e. language=primary)
  if((is.null(primary) & is.null(secondary))|(!is.null(primary) & is.null(secondary))){
    
    quest <- get_quest(primary)
    
    if(isTRUE(flex)){
      quest <- quest  %>%
        dplyr::group_by(name) %>%
        dplyr::mutate(
          description=dplyr::if_else(description %in%
                                       c("choices"),
                                     "",
                                     paste0(stringr::str_to_title(stringr::str_replace(description,"_"," ")),":")),
          is_last_val_in_group = dplyr::row_number() == max(dplyr::row_number()),
          name=dplyr::if_else(dplyr::row_number()==1,name,NA_character_)) %>%
        format_table %>%
        flextable::set_header_labels("name"="Variable name",
                                     "description"="",
                                     "value"="Question or Calculation")
    }
    
  }

  # Primary and Secondary languages specified
  if(!is.null(primary) & !is.null(secondary)){

    # Create primary language dataframe 
    quest1 <- get_quest(primary)
    
    # Create secondary language dataframe 
    quest2 <- get_quest(secondary)

    tryCatch(
      {
        # Merge primary and secondary dataframes, will fail if numbers of rows differ
        quest <- dplyr::bind_cols(quest1 %>%
                                    dplyr::rename(value1=value),
                           quest2 %>%
                             dplyr::select(value) %>%
                             dplyr::rename(value2=value)) %>%
          dplyr::relocate(name,description,value1,value2)
        
      }, error=function(error_message) {
        message(error_message)
        message(" Are there any missing translations? Check that number of primary and secondary language label rows are equal.")
        NULL
      }
    )

    if(isTRUE(flex)){
      quest <- quest  %>%
        dplyr::group_by(name) %>%
        dplyr::mutate(
          description=dplyr::if_else(description %in%
                                       c("choices"),
                                     "",
                                     paste0(stringr::str_to_title(stringr::str_replace(description,"_"," ")),":")),
          is_last_val_in_group = dplyr::row_number() == max(dplyr::row_number()),
          name=dplyr::if_else(dplyr::row_number()==1,name,NA_character_)) %>%
        format_table %>%
        flextable::set_header_labels("name"="Variable name",
                                     "description"="",
                                     "value1"="Question or Calculation",
                                     "value2"=secondary)
      }

  }

  return(quest)
}
