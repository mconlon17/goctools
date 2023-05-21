#' dict_support_types
#'
#' @return Dictionary of support types.  key, value, and "narrow" -- a collection of values optimized for narrow plots and lists
#' @export
#'
#' @examples
#' dict_support_types()

dict_support_types <- function() {
    
    vars <- list(
        c("Benefits", "Benefits", "Bene fits"),
        c("Culinary", "Culinary", "Culi nary"),
        c("Education_in_house", "Education -- in-house", "Edu GOC"),
        c("Education_pre_enrollment", "Education -- pre-enrollment", "Edu pre"),
        c("Education_post_enrollment", "Education -- post-enrollment", "Edu post"),
        c("Employment_readiness", "Employment readiness", "Emp pre"),
        c("Employment_IE", "Employment -- Independent", "Emp IE"),
        c("Employment_SE", "Employment -- Supplemental", "Emp SE"),
        c("Employment_TE", "Employment -- Transitional", "Emp TE"),
        c("Development", "Development", "Dev"),
        c("Garden", "Garden", "Gar den"),
        c("Housing", "Housing", "Hou sing"),
        c("Janitorial", "Janitorial", "Jani torial"),
        c("Media", "Media", "Me dia"),
        c("Mediation", "Mediation", "Medi ation"),
        c("Office", "Office", "Off ice"),
        c("Referral", "Referral", "Ref erral"),
        c("Social", "Social", "Soc ial"),
        c("Spiritual", "Spritiual", "Spiri tual"),
        c("Transportation_education", "Transportation -- education", "Van Edu"),
        c("Transportation_employment", "Transportation -- Employment", "Van Emp"),
        c("Transportation_goc", "Transportation -- GOC", "Van GOC"),
        c("Transportation_health", "Transportation -- Health", "Van Hlth"),
        c("Transportation_wellness", "Transportation -- Wellness", "Van Well"),
        c("Unknown", "Unknown", "Unk"),
        c("Wellness", "Wellness", "Well ness"),
        c("Work_ordered_day", "Word Ordered Day", "WOD")
    )
    
   data.frame(key = sapply(vars, function(x) x[1]), value = sapply(vars, function(x) x[2]), narrow = sapply(vars, function(x) x[3]))
}