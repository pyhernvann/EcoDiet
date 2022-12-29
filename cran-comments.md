# New submission to the CRAN: version 2.0.0


# Second submission to the CRAN (version 1.0.0)

This is our second submission and we thank you for your reviews regarding our package. We have fixed the package according to your recommendations:

* We have changed the package description.

* We have ensured that our functions do not write by default in the user's home filespace.

* We have replaced \dontrun{} with \donttest{} for the examples running in more than 5 seconds.

* We have elaborated the documentation.

* One point of clarification: you have noted that we have 3 .Rd files for data documentation but just one dataset in /data. This is because we have datasets in both \inst\data (6 files) and \data (1 file). We actually have 7 documentation .Rd files, each corresponding to a dataset.

### Test environments
* local macOS 10.13.6 install, R 3.6.2
* local ubuntu 19.10 install, R 3.6.1

### R CMD check results
There were no ERRORS or WARNINGS.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE

Maintainer: ‘Pierre-Yves Hernvann <pierre.yves.hernvann@gmail.com>’

New submission

### Downstream dependencies
There are no downstream dependencies.

---------------

# Reply from the CRAN

* Please omit the "The goal of the package is to" and instead start with
"Estimates ..." or similar.

* Please always write package names, software names and API names in
single quotes in title and description. e.g: --> 'rjags'

* Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). That is not allowed by CRAN policies.
Please only write/save files if the user has specified a directory in
the function themselves. Therefore please omit any default path =
getwd() in writing functions.
In your examples/vignettes/tests you can write to tempdir().
e.g. run_model.R, plot_biotracer_data.R, ..

* Please elaborate in the documentation e.g. \value of preprocess_data.Rd

* You have 3 .Rd files for data documentation but just one dataset in /data ?
Also please elaborate in the documentation of the data.

* \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{}.

Please fix and resubmit.

---------------

# First submission to the CRAN (version 1.0.0)

This is the first submission of the package.

### Test environments
* local macOS 10.13.6 install, R 3.6.2
* local ubuntu 19.10 install, R 3.6.1
* local Windows 10 install, R 3.6.2

### R CMD check results
There were no ERRORS or WARNINGS.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE

Maintainer: ‘Pierre-Yves Hernvann <pierre.yves.hernvann@gmail.com>’

New submission

### Downstream dependencies
There are no downstream dependencies.