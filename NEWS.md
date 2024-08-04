# joyn 0.2.3

* Fix bug where the left join did not work when updating values and a full join was maintained.

# joyn 0.2.2

* Remove unmasking functions as we realized that they are not necessary. 

# joyn 0.2.1 (Not in CRAN)


* Add `anti_join()` function.

* Add `unmask_joyn()` function to unmask `joyn` functions that mask `dplyr` equivalents.

* Add information about duplicated obs in `by` variable when match type is `1` rathern than `m`. 

* improve inefficiencies in deep copies with `m:m` joins

* Replace `m:m` joins from `data.table::merge.data.table` to `collapse::join`. Thanks to @SebKrantz for the suggestion (#58).

* Add information about duplicated obs in `by` variable when match type is `1` rather than `m`.

* Internal: improve storing of joyn messages.

* Improve creation of reporting variable. Now, it is created in [collapse::join] rather than in `joyn` function. In addition, the reporting variable is created as factor to improve performance. Thanks to @SebKrantz for the suggestion (#58)

## breaking changes

* Now, by default, `joyn` will not sort the data. This is to avoid unnecessary 
computational time that most of the time is not needed. 
If the user wants to sort the data, they can use the `sort` argument, which triggers 
the sorting mechanism of `collapse` package.

* report variable (named ".join" by default) is now a factor instead of character. Yet, users can still use character if they want with the `reporttype = "character"`.

# joyn 0.2.0

* `joyn` has gained two new authors: Zander Prinsloo and Rossana Tatulli.


## Breaking changes

* Function `joyn::merge()` was replaced by `joyn::joyn()`. This is now the main function of the `joyn` package.

* Arguments `allow.cartesian`, `yvars`, and `keep_y_in_x` have been deprecated. The latter two have been replaced by `y_vars_to_keep` and `keep_common_vars`, respectively. The new argument names bring more clarity about what they arguments do.

## New features

* New function `joyn::merge()` works as a mask for the `base::merge()` or `data.table::merge.data.table()`. `joyn::merge()` has the same features as the previous two, but includes the features of `joyn::joyn()`.

* Messages style have been improved and categorized. See [message vignette](https://randrescastaneda.github.io/joyn/articles/messages.html) for more information.

* New functions to mimic [dplyr joins](https://dplyr.tidyverse.org/reference/mutate-joins.html). The `joyn` variants have all the features for `joyn::joyn()` but lack some of the most advance features of `dplyr` joins like [`joyn::join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)


## Minor improvements and fixes

* Minor inconsistency bugs were fixed.

# joyn 0.1.4
* update_NAs now could be FALSE even if update_values is TRUE

* Select rows-to-keep before transformation of updated values and NAs to avoid keeping rows from y that did not match in x but whose values got updated because `update_values = TRUE`

* Solve issues #1 and #19

* Change to data.table::merge.data.table syntax in all joins. It makes it easier to work with and consistent across different join types. 

* Remove previous lazy-loaded data.


# joyn 0.1.3
* Convert external data to external data.

# joyn 0.1.2

* Add function `possible_ids()` to identify what variables are suitable for 
uniquely identify the database.

## joyn 0.1.1

* Add function `is_id()` to check whether the table is uniquely identified by 
key variables

* Add function `freq_table()` as a substitute for janitor::tabyl. This makes it 
more convenient for users who do not have janitor installed. 

## joyn 0.1.0

Fix bug on `by` argument when using "=" or "==". 

## joyn 0.0.1
First Public release
