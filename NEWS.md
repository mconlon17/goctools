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
