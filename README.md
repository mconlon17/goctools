# goctools -- an R package for the GOC

goctools is an R package for the [Gainesville Opportunity center](https://goclubhouse.org)

Tools include database access, data cleaning, dashboard widgets, plots, and reports.

Tools are usable from 

1. the command line 
1. the GOC R server in batch jobs controlled by cron 
1. the GOC Dashboard

Having a tools package used by these insures consistency

## Installation
To install:
```
install.packages('mconlon17/goctools')
library(goctools)
```

## Naming Conventions

* `plot_` -- plots
* `list_` -- flextable reports (lists) that tabulate data for presentation.
* `dict_` -- dictionary functions used to translate values from Flourish internal (key) to presentation externation (value)
* `check_` -- functions used to check values for possible errors.  In some cases these are frequency tables, in some cases lists, and in some cases, more sophisticated check.  For example `check_duplicate_contacts` checks all the contacts against each other for duplicate names using Levenshtein distance.
* `get_` -- get data from Flourish.  These functions return data.frames ready for further processing

Two special functions insure consistency of presentation:

* `goc_table` -- creates a flextable from a data.frame using GOC presentation standards
* `goc_plot` -- creates a ggplot from a ggplot applying GOC presentation standards

