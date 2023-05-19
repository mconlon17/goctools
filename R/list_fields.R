#' List Fields in a Table
#'
#' @param table_name One of the Flourish table names. Tables can be referred to by common names, eg, contacts, members, etc, or by database names such as flo_gifts
#' 
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr where
#' @importFrom dplyr across
#' @importFrom dplyr everything
#'
#' @return a flextable showing the fields in the table, the number of times the field is used, whether the field is auto-populated, notes, and actions
#' @export
#'
#' @examples
#' list_fields()
#' list_fields("attendance") # keyword call
#' list_fields("email_addr_bean_rel") # direct call by name

list_fields <- function(table_name = "attendance") {
    
    V1=action=auto=field_name=non_blank=notes=NULL
    
    key <- dict_table_names()$key[which(dict_table_names()$value == table_name)]
    table_key <- ifelse(identical(key, character(0)), table_name, key)

    table <- get_table(table_key) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.na(.), 0, 1))) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ ifelse(. == "" | . == "NA" | is.na(.), 0, 1))) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), sum))

    table_fields <- as.data.frame(t(table)) %>%
        dplyr::mutate(field_name = rownames(t(table))) %>%
        dplyr::rename(non_blank = V1) %>%
        dplyr::left_join(get_field_notes(table_name), by="field_name") %>%
        dplyr::select(field_name, non_blank, auto, action, notes) %>%
        dplyr::arrange(auto, field_name)

    ft <- goc_table(table_fields, paste0(stringr::str_to_title(table_name), " table has ", nrow(table_fields[table_fields$auto == "no", ]),
                                         " data entry fields, ", nrow(table_fields), " total fields"))
    ft
}
