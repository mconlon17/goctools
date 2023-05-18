#' Check Users
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' 
#' @return a flextable showing the users
#' @export
#'
#' @examples
#' check_users()
check_users <- function() {
    
    employee_status=last_name=first_name=NULL
    Name=`User name`=user_name=`Password\nLast Changed`=pwd_last_changed=`Date Account\nCreated`=date_entered=NULL
    `Date Last\nModified`=date_modified=Title=title=`Work Phone`=phone_work=NULL
    
    users <- get_users() %>%
        dplyr::filter(employee_status == "Active") %>%
        dplyr::mutate(Name=paste0(last_name, ifelse(is.na(first_name), "", paste(",",first_name)))) %>%
        dplyr::select(Name, `User name`=user_name, `Password\nLast Changed`=pwd_last_changed, `Date Account\nCreated`=date_entered, 
                      `Date Last\nModified`=date_modified, Title=title, `Work Phone`=phone_work) %>%
        dplyr::arrange(Name)
    
    
    ft <- goc_table(users, "Active Flourish Users")
    ft
}
