#' Membership By the Numbers
#'
#' @param date Compute a dtable of statistics for months.  The 15th month is defined by date
#' @param months Number of months to incude in the resulting table.  One row per month
#' @param output If output="flextable", return a flextable, otherwise return a data.frame
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr tally
#' @importFrom dplyr summarise
#' @importFrom dplyr inner_join
#' @importFrom dplyr full_join
#' @importFrom dplyr n_distinct
#' @importFrom dplyr rename
#' @importFrom dplyr n
#' @importFrom flextable width
#' @importFrom flextable add_header_row
#' @importFrom flextable merge_at
#' @importFrom flextable set_formatter_type
#' @importFrom flextable colformat_num
#' @importFrom flextable align
#' @importFrom lubridate as_date
#' @importFrom lubridate dmonths
#' @importFrom lubridate ddays
#' @importFrom lubridate floor_date
#' @importFrom lubridate %within%
#' @importFrom lubridate %--%
#' @importFrom lubridate wday
#' @importFrom tidyr pivot_wider
#'
#' @return a flextable or data.frame with one for each month, and columns for various statistics of interest to the board of directors
#' @export
#'
#' @examples
#' list_membership_by_the_numbers()
#' list_membership_by_the_numbers("2023-01-01")

list_membership_by_the_numbers <- function(date=Sys.Date(), months=15, output="flextable") {
    
    start_date=end_date=NULL
    ADA=`Amount Raised`=AvgHrs=ContactID=Donors=Employment_IE=NULL
    Employment_SE=Employment_TE=Employment_readiness=`Members with Supports`=NULL
    Supports=NewMembers=UniqueMembers=c2_groups=contact_2_id=NULL
    da_date_of_attendance=da_total_hours=flo_application_date=NULL
    flo_gift_amount=flo_gift_date=flo_support_date=flo_support_type=NULL
    flo_supports_contactscontacts_idb=id=month_start=NULL
    
    start_date <- lubridate::floor_date(lubridate::as_date(date) -lubridate::dmonths(months), "month")
    end_date <- lubridate::ceiling_date(lubridate::as_date(date), "month") -lubridate::ddays(1)
    
    ndays <- as.integer(lubridate::as_date(Sys.Date()) - lubridate::as_date(start_date)) + 1
    
    attendance <- get_attendance(days = ndays) %>%
        dplyr::mutate(da_date_of_attendance = lubridate::as_date(da_date_of_attendance)) %>%
        dplyr::filter(da_date_of_attendance %within% (start_date %--% end_date)) %>%
        dplyr::select(contact_2_id, da_date_of_attendance, da_total_hours)

    contacts <- get_contacts() %>%
        dplyr::filter(grepl("Member", c2_groups)) %>%
        dplyr::mutate(ContactID = id) %>%
        dplyr::select(id, ContactID)

    members <- get_members() %>%
        dplyr::select(id, flo_application_date)

    attendance <- attendance %>%
        dplyr::inner_join(contacts, by = c("contact_2_id" = "id"))

    tab1 <- attendance %>%
        dplyr::mutate(month_start = lubridate::floor_date(da_date_of_attendance, "month")) %>%
        dplyr::filter(da_total_hours < 24) %>%
        dplyr::group_by(month_start) %>%
        dplyr::summarise(UniqueMembers = dplyr::n_distinct(ContactID), AvgHrs = mean(da_total_hours, na.rm = T))

    tab2 <- attendance %>%
        dplyr::filter(lubridate::wday(da_date_of_attendance) > 1 & lubridate::wday(da_date_of_attendance) < 7) %>%
        dplyr::mutate(month_start = lubridate::floor_date(da_date_of_attendance, "month")) %>%
        dplyr::group_by(month_start, da_date_of_attendance) %>%
        dplyr::tally() %>%
        dplyr::group_by(month_start) %>%
        dplyr::summarise(ADA = mean(n))
    
    tab3 <- members %>%
        dplyr::filter(lubridate::as_date(flo_application_date) %within% (start_date %--% end_date)) %>%
        dplyr::mutate(month_start = lubridate::floor_date(as_date(flo_application_date), "month")) %>%
        dplyr::group_by(month_start) %>%
        dplyr::tally() %>%
        dplyr::rename(NewMembers = n)

    supports <- get_supports() %>%
        dplyr::filter(lubridate::as_date(flo_support_date) %within% (start_date %--% end_date)) %>%
        dplyr::select(id, flo_support_type, flo_support_date)

    support.rel <- get_table("flo_supports_contacts_c") %>%
        dplyr::select("flo_supports_contactsflo_supports_ida", "flo_supports_contactscontacts_idb")

    tab4 <- supports %>%
        dplyr::left_join(support.rel, by = c("id" = "flo_supports_contactsflo_supports_ida")) %>%
        dplyr::mutate(month_start = lubridate::floor_date(lubridate::as_date(flo_support_date), "month")) %>%
        dplyr::group_by(month_start) %>%
        dplyr::summarise(Supports = dplyr::n(),
                         `Members with Supports` = n_distinct(flo_supports_contactscontacts_idb, na.rm = T))
    
    tab5 <- supports %>%
        dplyr::left_join(support.rel, by = c("id" = "flo_supports_contactsflo_supports_ida")) %>%
        dplyr::mutate(month_start = lubridate::floor_date(lubridate::as_date(flo_support_date), "month")) %>%
        dplyr::filter(flo_support_type %in% c("Employment_readiness", "Employment_TE", 
                                              "Employment_SE", "Employment_IE")) %>%
        dplyr::group_by(month_start, flo_support_type) %>%
        dplyr::tally() %>%
        tidyr::pivot_wider(names_from=flo_support_type, values_from=n)
    
    tab6 <- get_gifts() %>%
        dplyr::filter(lubridate::as_date(flo_gift_date) %within% (start_date %--% end_date)) %>%
        dplyr::mutate(month_start = lubridate::floor_date(lubridate::as_date(flo_gift_date), "month")) %>%
        dplyr::group_by(month_start) %>%
        dplyr::summarise(Donors=n_distinct(contact_2_id), `Amount Raised`=sum(flo_gift_amount))
        
        

    board.report <- tab1 %>%
        dplyr::full_join(tab2, by="month_start") %>%
        dplyr::full_join(tab3, by="month_start") %>%
        dplyr::full_join(tab4, by="month_start") %>%
        dplyr::full_join(tab5, by="month_start") %>%
        dplyr::full_join(tab6, by="month_start")
    
    board.report <- replace(board.report, is.na(board.report), 0) %>%
        dplyr::select(Month = month_start, `Unique Members` = UniqueMembers, `Average Workday Hours` = AvgHrs, 
               `Average Daily Attendance` = ADA, `New Members` = NewMembers, Supports, `Members with Supports`,
               `Pre-employment Supports`=Employment_readiness, `TE Supports`=Employment_TE,
               `SE Supports`=Employment_SE, `IE Supports`=Employment_IE, Donors, `Amount Raised`)

    if (output == "flextable") {
        
        ft <- goc_table(board.report, "Membership by the Numbers", 
                        caption=paste0("For the period ", start_date, " to ", end_date)) %>%
            flextable::width(j=1, width=1.1) %>%
            flextable::width(j=seq(2,13),width=0.75) %>%
            flextable::add_header_row(colwidths=c(1,4,6,2),values=c("Month", "Attendance", "Supports", "Development")) %>%
            flextable::merge_at(i = seq(1,2), j = 1, part = "header") %>%
            flextable::set_formatter_type(fmt_double = "%.01f") %>%
            flextable::set_formatter_type(fmt_date = "%B, %Y") %>%
            flextable::colformat_num(j=13, prefix="$") %>%
            goc_theme() %>%
            flextable::align(align="center", part="header")
        
    } else {
        
        ft <- as.data.frame(board.report)
        
    }
    
    ft
    
}
