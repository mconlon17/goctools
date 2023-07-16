# goctools 1.1.0

* 97 functions
* `list_gifts` added
* Improvements to `list_supported_employment_billing`. Footnotes describe columns.  Referrals omitted.
* Cosmetic improvements to `list_membership_by_the_numbers` and `plot_membership_by_the_numbers`
* Minor improvements to other functions to support Flourish changes: `list_staff_progress_notes`, `list_staff_time`, `list_supports_by_member_and_type`,
`plot_gifts_by_year`, `plot_supports_by_staff_and_type` 

# goctools 1.0.0

* 96 functions.  This completes the planned collection and revision of all existing GOC R software

# goctools 0.9.0

* 88 functions.  About 7 more to be added to get to Version 1.0.0
* `plot_membership_by_the_numbers` shows 12 plots of GOC data on one page. Displays data from `list_membership_by_the_numbers`

# goctools 0.8.0

* 79 functions.  About 14 to be added in future versions
* `dict_city_county` added to lookup counties for cities
* `dict_support_types` added to translate Flourish dropdown for support types

# goctools 0.7.0

* 67 functions. About 21 to be added in future versions
* New series: `dict_` functions to emulate python dict() objects.  Used to translate from internal database key values to external display values.  Will be built out in future work:
    * `dict_campaign_names` Names of gift campaigns
    * `dict_table_names` Names of Flourish tables, from internal to external.  Formerly `get_table_names`

# goctools 0.6.0

* 58 functions. About 28 to be added in future versions.
* All check functions added -- most functions have a check parameter. Values shown below. 61 checks in all.

    * `check_attendance` attendee, date, time_in, time_out, times, location
    * `check_contacts` phone, email, street, state, nl, full_name, zip, or solicit
    * `check_employment` member, employer, start, support, pay, or hours		
    * `check_outreach` member, date, type, or outcome
    * `check_gifts` donor, restriction, anonymous, amount, campaign, date, method, restricted, or thank_you		
    * `check_progress_notes` staff, member, goal, text
    * `check_duplicate_contacts`	
    * `check_goals` staff, member, text, lapsed_table, and lapsed_list
    * `check_supports` staff, member, date, duration, type, durations
    * `check_duplicate_members`
    * `check_members`	contacts, emergency, benefits, demographics, housing, education, employment, medical, referral, admin, and attendance
    * `check_users`

# goctools 0.5.0

* 51 functions. About 35 to go. Half the "check" functions have been added.  Some functions are being combined, some are being parameterized to return specific results, some are being separated into more than one function.

# goctools 0.4.0

* 31 functions. All previously existing functions in the dashboard are now included.  
* The remainder of the work involves converting Rmd files to functions to simplify code management, simplify dashboard, improve reuse,
and eliminate use of RMarkdown.  All functions are either utilities (helpers, getters, etc), plots --  returning a plot object, or reports --  returning a flextable object. Approximately 60 additional functions will be added.

# goctools 0.3.0

* All helper functions are included

# goctools 0.2.0

* All data getter functions are included

# goctools 0.1.0

* No errors, warnings, notes
* Can be installed and used from GitHub
* Added a `NEWS.md` file to track changes to the package.
