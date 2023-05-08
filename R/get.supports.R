#' Get supports from Flourish
#'
#' @param include.deleted if TRUE, include deleted supports
#' @param with.members if TRUE, return outer join of support data with member data.  WARNING: the result can be a large data.frame
#' @param active.only if TRUE, and with.members=TRUE, the outer join of supports and members include only active members
#' @importFrom dplyr inner_join
#'
#' @return a data frame of supports
#' @export
#'
#' @examples
#' get.supports()
#' get.supports(with.members=TRUE) # return outer join of members and supports (large data.frame)

get.supports <- function(include.deleted = FALSE, with.members=FALSE, active.only=FALSE) {
    
    supports <- get.table("flo_supports", include.deleted)
    
    if (with.members) {
        
        rel <- get.table("flo_supports_contacts_c")
        
        members <- get.members(include.deleted, active.only)
        
        supports <- supports %>%
            dplyr::inner_join(rel, by = c("id"="flo_supports_contactsflo_supports_ida")) %>%
            dplyr::inner_join(members, by=c("flo_supports_contactscontacts_idb"="id"))
    }
    
    supports
}