#' Tabulates results from checks output from "compare_survey"
#'
#' This function creates flextable formatted tables from objects stored in full_compare list output by compare_survey.
#' @param results List output by function 'compare_survey'.
#' @param number Number of check to tabulate.
#' @return Flextable object.
#' @export
#' @examples
#' \dontrun{tabulate_comparison(1)}

tabulate_comparison <- function(results,number){
  return(flextable::flextable(results[[number]]) %>%
           flextable::align(align = "right", part = "body") %>%
           flextable::align(align = "center", part = "header") %>%
           flextable::fontsize(size=10, part="all") %>%
           flextable::font(fontname="Calibri", part="all") %>%
           flextable::bold(bold = TRUE, part="header") %>%
           flextable::bold(bold = TRUE, j=1) %>%
           flextable::set_table_properties(layout="autofit"))
}
