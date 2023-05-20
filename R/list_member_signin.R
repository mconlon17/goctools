#' List Member Sign-In Sheet
#'
#' Active Members are listed along with staff and volunteers who have attended in the last 90 days
#'
#' @param date The date for which sign-in is needed
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr inner_join
#' @importFrom dplyr add_row
#' @importFrom lubridate as_date
#' @importFrom flextable colformat_num
#' @importFrom flextable width
#' @importFrom flextable set_header_labels
#'
#' @return a flextable with the sign-in sheet
#' @export
#'
#' @examples
#' list_member_signin()
#' list_member_signin("2023-05-21")

list_member_signin <- function(date=Sys.Date()) {
    
    Name=contact2_nickname=last_name=first_name=id=c2_groups=NULL
    Since=da_date_of_attendance=contact_2_id=MostRecent=NULL
    Signature=In=Out=In.1=In.1=In.2=Out.2=In.3=Out.3=NULL
    
    contacts <- get_contacts() %>%
        dplyr::mutate(Name = ifelse(!is.na(contact2_nickname) & contact2_nickname != "",
                             paste0(last_name, ", ", first_name, ' "', contact2_nickname, '"'),
                             paste(last_name, first_name, sep = ", "))) %>%
        dplyr::filter(grepl("Member", c2_groups) | grepl("Staff", c2_groups) | grepl("Volunteer", c2_groups)) %>%
        dplyr::select(id, Name)
    
    tab <- get_attendance(days = 90) %>%
        dplyr::mutate(Since = difftime(as.POSIXct(date), as.POSIXct(da_date_of_attendance), units = "days")) %>%
        dplyr::group_by(contact_2_id) %>%
        dplyr::summarise(MostRecent = min(Since)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(contacts, by = c("contact_2_id" = "id")) %>%
        dplyr::mutate(Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
        dplyr::select(Name, Signature, In, Out, In.2, Out.2, In.3, Out.3) %>%
        dplyr::arrange(Name) %>%
        dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
        dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "") %>%
        dplyr::add_row(Name = ".", Signature = "", In = "", Out = "", In.2 = "", Out.2 = "", In.3 = "", Out.3 = "")
    
    ft <- goc_table(tab, paste0("Sign-in sheet for ", lubridate::as_date(date))) %>%
        flextable::colformat_num(big.mark = "") %>%
        flextable::width(j = seq(3, 8), width = 0.8) %>%
        flextable::width(j = 2, width = 2.5) %>%
        flextable::set_header_labels(In.2 = "In", Out.2 = "Out", In.3 = "In", Out.3 = "Out")
    ft
}
