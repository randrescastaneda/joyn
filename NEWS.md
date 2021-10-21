# joyn (development version)
* update_NAs now could be FALSE even if update_values is TRUE

* Select rows-to-keep before transformation of updated values and NAs to avoid keeping rows from y that did not match in x but whose values got updated because `update_values = TRUE`


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
