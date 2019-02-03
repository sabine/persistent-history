Don't use this, since this is an experimental "library" written by a Template Haskell newbie. If you do nonetheless, know that you have been warned.

This is far from complete, and will probably grow if I find the time to pursue the projects I use this in. If you are interested in contributing, please get in touch by opening an issue.

TODO:
- go through https://wiki.haskell.org/How_to_write_a_Haskell_program and write documentation
- ask for a code review

# What is this and how does it work?

I wanted to have a way to record the history of previous revisions of the records in my database. Similar to `persistent-audit` (https://github.com/plow-technologies/persistent-audit), what I do here is based on http://www.4guysfromrolla.com/webtech/041807-1.shtml, i.e. there is a separate history table for every table.

Currently, there are
- types `Created`, `Updated`, `Deleted` that record the `UTCTime` at which the record was updated, or, respectively, a Boolean that says whether the record is deleted.
- a function `addHistoryDefs` that transforms the entity definition list from persistent by
  * adding a history model for every model that is annotated with the attribute `history`, and
  * adding the `created`, `createdBy`, `updated`, `updatedBy`, and `deleted` fields to the annotated model
- a TH function `mkHistory` that creates `insert`, `insertKey`, `select`, `selectFirst`, `update`, and `delete` functions for the model with history
