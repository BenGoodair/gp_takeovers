# Care-Markets
 
Welcome to a holding repository for reproduction files for a research paper on corporate delivery of primary care in England
# What is available?

The repo currently holds data files, partially cleaned, relating to England GP's ownership status; patient satisfaction; revenue; and QOF points. As well as some geographic data such as deprivation levels.

The repo has two coding files: "01.5_manual_downloads" - this file is how I produced the underlying data. It shows the use of APIs or raw data and how they are cleaned into panel data format.

The other coding file: "02_analysis" - this file should be replicable code which pulls data directly from the Data folder and runs the analyses as per the paper.

# How to use?

## Coders

Download the "02_analysis.R" file, and run the analyses. Upon final publication, a binder file will be produced dockerising the dependencies for perfect replication.

## Non-coders

For non-coders or R users, underlying data for each outcome variable can be downloaded from the Data file. Upon final publication a "Master dataset" will also be published to allow for observation and re-use.

# Notes and considerations

Please reach out to b.goodair@lse.ac.uk for questions - I am very happy to share bespoke code/ data as per requests and individual needs.
