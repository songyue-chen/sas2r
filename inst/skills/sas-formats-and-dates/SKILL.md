---
name: sas-formats-and-dates
description: Keep SAS dates as day numbers and declare the formats SAS gives a written dataset when translating or reviewing FORMAT, ATTRIB, format= or date code.
metadata:
  sas2r:
    version: 1
    agents: [translator, reviewer, fixer]
    priority: 90
    triggers:
      semantic_rules: [known_domains.datetime_epochs_formats]
      flags: [sas_formats]
    tools: [query_project_graph, read_dependency_context, lookup_rulebook]
---

# SAS Formats and Dates

## What a SAS date is

A SAS date is a number: days from 1 January 1960. A format such as `DATE9.` or `YYMMDD10.` only says how to show it. SAS computes with the number.

- Keep the number in R. Do not turn it into an R `Date` to calculate with it.
- A date literal is that number: `'01JAN2020'd` is 21915.
- A difference of two dates, and a date plus a count of days, are plain numbers.

## How a format travels in SAS

A format belongs to a variable of a dataset, not to a value.

- `SET` and `MERGE` bring a variable's format in with it. When several datasets carry the variable, the first one that gives it a format wins.
- A `FORMAT` or `ATTRIB` statement sets the format for the dataset the step writes. `format x;` with no format removes it.
- A variable removed by `DROP` or `KEEP` takes its format with it. A variable of the same name created afterwards is new and has no format.
- A new variable has no format, even when it is computed from dates: `dur = adt - rfdt + 1` is a number of days, not a date.
- `RENAME` moves the format to the new name.

## Declare the formats where the dataset is written

`lib_write()` takes the formats SAS gives the dataset's variables:

```r
lib_write(adae, "adam", "adae",
          formats = c(astdt = "yymmdd10.", aendt = "yymmdd10."))
```

- Write `formats` out as `c(column = "format.", ...)` with string constants. A computed value fails the checks.
- Give each format as the SAS source writes it.
- Declare a format for every variable of the written dataset that has one in SAS: those the step formats itself, and those that keep a format from a dataset the step reads.
- A column given a date format is stored as a date. `lib_read()` then returns the SAS day number to a later program, so that program computes as the SAS does.
- Datetime and time formats are not converted. Leave those values as numbers.

## Where a column's format comes from

- **This step.** A `FORMAT` or `ATTRIB` statement, or a `format=` option. Declare it.
- **Another program's output.** The column arrives as a day number. If this step keeps it, declare its format again. Use the lineage and dependency tools to find the program that writes that dataset and what it declares.
- **An input file.** A column the file stores as a date arrives as an R `Date`. Left untouched, it is written as a date and needs no declaration. To calculate with it, compute the SAS day number into a working variable.

## What not to do

- Do not declare a format the SAS source does not give the variable. A declaration with no `FORMAT`, `ATTRIB` or `format=` behind it fails the checks.
- Do not declare a format to make an output match anything else.
- Do not declare a date format for a count of days, an age, or a study day.

## Review checkpoints

1. Every variable the SAS formats as a date in a written dataset is declared in that dataset's `lib_write()`, and nothing else is.
2. A variable that was dropped and created again is not declared with the old format.
3. Date arithmetic uses day numbers, with no conversion to or from R `Date` in between.
4. A column from an input file that is used in arithmetic is first turned into its day number.
