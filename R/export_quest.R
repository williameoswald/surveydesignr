#' Export dataframe or flextable with readable version of xlsform questionnaire
#'
#' This function imports a XLSForm file and exports either a dataframe (tibble) or a flextable object with a readable version of questionnaire.
#' @param xlsformfile Filepath of xlsform as text or referring to object with filepath.
#' @param primary Main language defined in form. Specified as "label:English", etc.
#' @param secondary Specify any secondary language of the form. Form can have more than one, but this will only check one at a time. (e.g. "Francais") Entered text must match language specification in the XLSForm columns.
#' @param flex Specify whether flextable object is output (default) or tibble object
#' @return Large flextable or tibble.
#' @importFrom magrittr "%>%"
#' @export
export_quest <- function(xlsformfile,primary="English",secondary=NULL,flex=TRUE){

  xlsform <- readxl::read_xlsx(xlsformfile, sheet = "survey") %>%
    # Leave repeat and group in to flag these in text version, "end" rows dropped as blank labels
    # dplyr::filter(!stringr::str_detect(name, "note_"),
    #               !stringr::str_detect(name, "_chk"),
    #               !type %in% c("start","end","today","deviceid")) %>%
    # Rename for SurveyCTO/ODK compatibility
    dplyr::rename(any_of(c(relevant="relevance",read_only="read only"))) %>%
    dplyr::mutate(choices_name=dplyr::if_else(stringr::str_detect(type,"select"),
                                              stringr::str_sub(type, start=stringr::str_locate(type," ")[,1]+1, end=-1L),
                                              NA))

  format_table <- function(flextab){
    flextab %>%
      flextable::flextable() %>%
      flextable::align(align="right", part="body") %>%
      flextable::align(align="left", part="header") %>%
      flextable::fontsize(size=10, part="all") %>%
      flextable::font(fontname="Calibri", part="all") %>%
      flextable::bold(bold=TRUE, part="header") %>%
      flextable::bold(~ description=="label",2) %>%
      flextable::align(~ description=="label",2, align="left") %>%
      flextable::align(i=NULL,j="name", align="left", part="body") %>%
      flextable::set_table_properties(layout="autofit") %>%
      flextable::height(height=0.7, part="body", unit="cm") %>%
      flextable::hline(i = ~is_last_val_in_group == TRUE) %>%
      flextable::delete_columns(c("description","is_last_val_in_group")) %>%
      flextable::set_caption(paste0(stringr::str_to_title(readxl::read_xlsx(xlsformfile, sheet = "settings") %>%
                                                            dplyr::pull(form_title)),
                                    ", Version: ",
                                    readxl::read_xlsx(xlsformfile, sheet = "settings") %>%
                                      dplyr::pull(version)),
                             style = "Table Caption")
  }

  # Primary specified
  if(!is.null(primary) & is.null(secondary)){

    label1txt <- paste0("label:",primary)
    label1txt_ <- dplyr::ensym(label1txt)

    # Create dataframe with questions, choices, relevance, constraints, and calcs
    quest <- xlsform %>%
      dplyr::select(name,contains(primary),
                    any_of(c("relevant","constraint","read_only","calculation")),
                    choices_name) %>%
      dplyr::relocate(relevant,.before = constraint) %>%
      tidyr::pivot_longer(cols = -c(name),
                   names_to = "description") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::left_join(readxl::read_xlsx(xlsformfile, sheet = "choices") %>%
                         dplyr::filter(!is.na(list_name)) %>%
                         dplyr::select(list_name,value,!!label1txt_) %>%
                         dplyr::rename(label=!!label1txt_) %>%
                         dplyr::mutate(choice=paste0(as.character(value)," ",label)) %>%
                         dplyr::select(list_name,choice),
                by=c("value"="list_name"),
                relationship = "many-to-many") %>%
      dplyr::mutate(value=dplyr::if_else(description=="choices_name",
                           choice,
                           value),
             description=stringr::str_replace(description,paste0(":",primary),""),
             value=dplyr::if_else(description %in%
                             c("hint","constraint","relevant",
                               "calculation","read_only"),
                           paste0(stringr::str_to_title(description),": ",value),
                           value)) %>%
      dplyr::select(-choice) %>%
      dplyr::relocate(name,value,description) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(
        is_last_val_in_group = dplyr::row_number() == max(dplyr::row_number()),
        name=dplyr::if_else(dplyr::row_number()==1,name,NA_character_))

    if(isTRUE(flex)){
      quest <- quest %>%
        format_table %>%
        flextable::set_header_labels("name"="Variable name",
                          "value1"="Question or Calculation")
      }

  }

  # Primary and Secondary specified
  if(!is.null(primary) & !is.null(secondary)){

    label1txt <- paste0("label:",primary)
    label1txt_ <- dplyr::ensym(label1txt)
    label2txt <- paste0("label:",secondary)
    label2txt_ <- dplyr::ensym(label2txt)

    # Create dataframe with questions, choices, relevance, constraints, and calcs
    quest1 <- xlsform %>%
      dplyr::select(name,contains(primary),
                    any_of(c("relevant","constraint","read_only","calculation")),
                    choices_name) %>%
      dplyr::relocate(relevant,.before = constraint) %>%
      tidyr::pivot_longer(cols = -c(name),
                   names_to = "description") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::left_join(readxl::read_xlsx(xlsformfile, sheet = "choices") %>%
                         dplyr::filter(!is.na(list_name)) %>%
                         dplyr::select(list_name,value,!!label1txt_) %>%
                         dplyr::rename(label=!!label1txt_) %>%
                         dplyr::mutate(choice=paste0(as.character(value)," ",label)) %>%
                         dplyr::select(list_name,choice),
                by=c("value"="list_name"),
                relationship = "many-to-many") %>%
      dplyr::mutate(value=dplyr::if_else(description=="choices_name",
                           choice,
                           value),
             description=stringr::str_replace(description,paste0(":",primary),""),
             value=dplyr::if_else(description %in%
                             c("hint","constraint","relevant",
                               "calculation","read only"),
                           paste0(stringr::str_to_title(description),": ",value),
                           value)) %>%
      dplyr::select(-choice) %>%
      dplyr::relocate(name,value,description)

    # Create dataframe with questions, choices, relevance, constraints, and calcs
    quest2 <- xlsform %>%
      dplyr::mutate(choices_name=dplyr::if_else(stringr::str_detect(type,"select"),
                                         stringr::str_sub(type, start=stringr::str_locate(type," ")[,1]+1, end=-1L),
                                  NA)) %>%
      dplyr::select(name,contains(secondary),
                    any_of(c("relevant","constraint","read_only","calculation")),
                    choices_name) %>%
      dplyr::relocate(relevant,.before = constraint) %>%
      tidyr::pivot_longer(cols = -c(name),
                   names_to = "description") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::left_join(readxl::read_xlsx(xlsformfile, sheet = "choices") %>%
                         dplyr::filter(!is.na(list_name)) %>%
                         dplyr::select(list_name,value,!!label2txt_) %>%
                         dplyr::rename(label=!!label2txt_) %>%
                         dplyr::mutate(choice=paste0(as.character(value)," ",label)) %>%
                         dplyr::select(list_name,choice),
                by=c("value"="list_name"),
                relationship = "many-to-many") %>%
      dplyr::mutate(value=dplyr::if_else(description=="choices_name",
                           choice,
                           value),
             description=stringr::str_replace(description,paste0(":",secondary),""),
             value=dplyr::if_else(description %in%
                             c("hint","constraint","relevant",
                               "calculation","read_only"),
                           paste0(stringr::str_to_title(description),": ",value),
                           value)) %>%
      dplyr::select(-choice) %>%
      dplyr::relocate(name,value,description)

    tryCatch(
      {
        quest <- dplyr::bind_cols(quest1 %>%
                                    dplyr::rename(value1=value),
                           quest2 %>%
                             dplyr::select(value) %>%
                             dplyr::rename(value2=value)) %>%
          dplyr::relocate(name,value1,value2,description) %>%
          dplyr::group_by(name) %>%
          dplyr::mutate(
            is_last_val_in_group = dplyr::row_number() == max(dplyr::row_number()),
            name=dplyr::if_else(dplyr::row_number()==1,name,NA_character_))
      }, error=function(error_message) {
        message(error_message)
        message(" Are there any missing translations? Check that number of primary and secondary language label rows are equal.")
        NULL
      }
    )

    if(isTRUE(flex)){
      quest <- quest %>%
        format_table %>%
        flextable::set_header_labels("name"="Variable name",
                          "value1"="Question or Calculation",
                          "value2"=secondary)
      }

  }

  # Language not specified (i.e. label,hint,constraint)
  if(is.null(primary) & is.null(secondary)){

    # Create dataframe with questions, choices, relevance, constraints, and calcs
    quest <- xlsform %>%
      dplyr::select(name,label,any_of(c("relevant","constraint",
             "read_only","calculation")),choices_name) %>%
      tidyr::pivot_longer(cols = -c(name),
                   names_to = "description") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::left_join(readxl::read_xlsx(xlsformfile, sheet = "choices") %>%
                         dplyr::filter(!is.na(list_name)) %>%
                         dplyr::mutate(choice=paste0(as.character(value)," ",label)) %>%
                         dplyr::select(list_name,choice),
                by=c("value"="list_name"),
                relationship = "many-to-many") %>%
      dplyr::mutate(value=dplyr::if_else(description=="choices_name",
                           choice,
                           value),
                    value=dplyr::if_else(description %in%
                                           c("hint","constraint","relevance",
                                             "calculation","read only"),
                                         paste0(stringr::str_to_title(description),": ",value),
                                         value)) %>%
      dplyr::select(-choice) %>%
      dplyr::relocate(name,value,description) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(
        is_last_val_in_group = dplyr::row_number() == max(dplyr::row_number()),
        name=dplyr::if_else(dplyr::row_number()==1,name,NA_character_))

    if(isTRUE(flex)){
      quest <- quest %>%
        format_table %>%
        flextable::set_header_labels("name"="Variable name",
                                       "value"="Question or Calculation")
    }

  }
  return(quest)
}
# Figure out relevant versus relevance and compare other column names ODK vs SurveyCTO
