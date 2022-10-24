# Nutrition Tools
 This repository contains useful scripts for taking data to ML analysis, especially with nutritionists in mind.


 ### **Start the Docker Environment**

 ```
## If you need to build it first
docker build -t nutrition_tools:1.0 .

## to run
docker run --rm -it -v /path/to/data:/home/data -v /path/to/github/scripts:/home/ -v path/to/output:/home/output scripts nutriton_tools1.0 /bin/bash -c "cd /home"
 ```


 ### **generic_read_in.R**

```
bash$ Rscript generic_read_in.R -h
Usage: generic_read_in.R [options]


Options:
	-i CHARACTER, --input=CHARACTER
		path to folder with data to import [default= /home/data/]

	-s CHARACTER, --subject_identifier=CHARACTER
		subject key (column name) found in all files [default= subject_id]

	-h, --help
		Show this help message and exit

```

This script will read in a directory of files (.csv | .txt | .tsv | .xls | .xlsx) and attempt to run some basic checks on them and basic cleaning. For example:
1. Check to see if a key is present that will link all the data together if split in multiple files (i.e. "subject_id")
2. Check to see if subject_id is unique across rows or if there are duplicated values
3. Check to see if there are duplicated column names
   1. Sub-check: if the column names are duplicated, is the data also duplicated?
4. Keep a tally of columns that contain NAs.
   
**Output:**

**/home/output_DATESTAMP/:**
 - **na_counts.csv**: counts of nas found in the input data
 - **na_counts.pdf**: vizualization of the nas
 - **summary_problems.csv**: summarizing where problems occured during import, which may cause downstream problems. The columns are as follows:
   - <i>dataset</i>: Name of dataset
   - <i>subject_id_not_found</i>: This file has no identifible key/subject_id
   - <i>subject_id_duplicated</i>: This appears to be longitudinal data Take care in using ML
   - <i>date_not_unix</i>:Time formats are messy Please standarize to Unix or drop
   - <i>duplicated_data</i>: You have columns with the same name and same data.  Please drop redundant columns
   - <i>duplicated_column_names</i>: You have the DIFFERENT data in columns with the same name. WHAT? This is sloppy.
 - **/clean_files**: new set of files that have been cleaned and checked for duplicate data
 - **/duplicate_colnames**: files that show where the column name was duplicated but the underlying data was different
 - **/duplicated_data**: files that show where the column name was duplicated and the data was also duplicated

 ### **microbial_hfe.R**

```
 microbial_HFE.R [--subject_identifier=<subject_colname> --label=<label> --feature_type=<feature_type> --var_control=<pct> --super_filter=<TRUE/FALSE>] <input_metadata> <input_metaphlan> <output_file>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier=<subject_colname> name of columns 
          with subject IDs [default: subject_id]
    --label response feature of interest for classification 
          [default: cluster]
    --feature_type of response i.e. numeric or factor [default: factor]
    --var_control filter features that contain less than this 
          threshold of percentage of unique features [default: 5]
    --super_filter to run a final RF and only take positive 
          values [default: FALSE]
    
Arguments:
    input_meta path to metadata input (CSV)
    input  path to input file from hierarchical data 
           (i.e. metaphlan data) (TSV)
    output_file  output file name 

```

This script will take a metaphlan-style file and perform hierarchical feature engineering on it (determine the most basic (highest taxonomic group) which contains discrimitory information for a given metadata variable, either continous or discreete):
