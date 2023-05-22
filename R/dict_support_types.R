#' dict_support_types
#'
#' @return Dictionary of support types.  key, value, and "narrow" -- a collection of values optimized for narrow plots and lists
#' @export
#'
#' @examples
#' dict_support_types()

dict_support_types <- function() {
    
    vars <- list(
        c("Benefits", "Benefits", "Bene\nfits"),
        c("Culinary", "Culinary", "Culi\nnary"),
        c("Education_in_house", "Education -- in-house", "Edu\nGOC"),
        c("Education_pre_enrollment", "Education -- pre-enrollment", "Edu\npre"),
        c("Education_post_enrollment", "Education -- post-enrollment", "Edu\npost"),
        c("Employment_readiness", "Employment readiness", "Emp\npre"),
        c("Employment_IE", "Employment -- Independent", "Emp\nIE"),
        c("Employment_SE", "Employment -- Supplemental", "Emp\nSE"),
        c("Employment_TE", "Employment -- Transitional", "Emp\nTE"),
        c("Development", "Development", "Dev"),
        c("Garden", "Garden", "Gar\nden"),
        c("Housing", "Housing", "Hou\nsing"),
        c("Janitorial", "Janitorial", "Jani\ntorial"),
        c("Media", "Media", "Me\ndia"),
        c("Mediation", "Mediation", "Medi\nation"),
        c("Office", "Office", "Off\nice"),
        c("Referral", "Referral", "Ref\nerral"),
        c("Social", "Social", "Soc\nial"),
        c("Spiritual", "Spritiual", "Spiri\ntual"),
        c("Transportation_education", "Transportation -- education", "Van\nEdu"),
        c("Transportation_employment", "Transportation -- Employment", "Van\nEmp"),
        c("Transportation_goc", "Transportation -- GOC", "Van\nGOC"),
        c("Transportation_health", "Transportation -- Health", "Van\nHlth"),
        c("Transportation_wellness", "Transportation -- Wellness", "Van\nWell"),
        c("Unknown", "Unknown", "Unk"),
        c("Wellness", "Wellness", "Well\nness"),
        c("Work_ordered_day", "Word Ordered Day", "WOD")
    )
    
   data.frame(key = sapply(vars, function(x) x[1]), value = sapply(vars, function(x) x[2]), narrow = sapply(vars, function(x) x[3]))
}