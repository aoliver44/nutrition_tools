# Nutrition Tools
 This repository contains useful scripts for taking data to ML analysis, especially with nutritionists in mind.

 Prerequisites
- Basic knowledge of a shell terminal 
- [git](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository#:~:text=Cloning%20an%20Existing,prev%20%7C%20next)  (or some other way to download/clone this repository)
- [docker desktop](https://www.docker.com/products/docker-desktop/) for the Rstudio envirnoment
- An internet connection

--------------------------------------------------

### **1. Clone the environment from Github**

```
## download repository
$ git clone https://github.com/aoliver44/nutrition_tools.git
$ cd nutrition_tools
```
This will pull down the scripts and dockerbuild files necessary to run these scripts

--------------------------------------------------

### **2. Build the Docker Environment**

 ```
## Option 1 (preferred), local installation:
$ docker pull aoliver44/nutrition_tools:1.0

## Option 2: Build it yourself! Still local installation.
$ docker build -t nutrition_tools:1.0 .

## Option 3: You are using singularity (assuming its in your path.
## you might need to load a module or something). Usually remote installation.
## **NOTE:** I do not know much about singularity

$ singularity pull nutrition_tools.sif docker://aoliver44/nutrition_tools:1.0
$ singularity run -w -W /path/to/working/directory nutrition_tools.sif bash

## to run
$ docker run --rm -it -v /path/to/data:/home/data \
> -v /path/to/github_repo/nutrition_tools:/home/ \
> nutriton_tools:1.0 bash

## example:
$ docker run --rm -it -v /User/$USER/Downloads/nutrition_tools/:/home nutrition_tools:1.0 bash
$ cd /home/
 ```

The above example command (entierly dependent on where you downloaded the repository to your computer...in this case it was downloaded to a folder with the path ~/Downloads) will start the docker container and provide a bash terminal to the user. 

When you ``cd /home/``, you should see whatever you mounted to this directory inside the docker container (whatever is in the folder before the ":" in the above command, in this case the directory "/User/$USER/Downloads/nutrition_tools/")

You are now operating inside a container, which contains the software necessary to run the following analyses.

**NOTE:** To exit the terminal, run ```exit``` or press ctrl+d on a mac

------------------------------------------

### **3. Run generic_read_in.R**

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

## example:
./generic_read_in.R --subject_identifier subject_id /home/data/ 

```

This script will read in a directory of files (.csv | .txt | .tsv | .xls | .xlsx) and attempt to run some basic checks on them and basic cleaning. For example:
1. Check to see if a key is present that will link all the data together if split in multiple files (i.e. "subject_id")
2. Check to see if subject_id is unique across rows or if there are duplicated values
3. Check to see if there are duplicated column names
   - Sub-check: if the column names are duplicated, is the data also duplicated?
4. Keep a tally of columns that contain NAs.
5. Check to see if you have time-series data and discourage htat
   - Note,  this program and many ML programs struggle with longitudinal data
   
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

**NOTES:** This program came about from large collaborations where many different datasets were being emailed around and subsequently combined for later analysis. It will still check a single file.

Also, if this script identifies problems and creates a summary_problems.csv file, the next script will check and make sure you fixed these problems, otherwise it will drop the dataset from the analysis. The program will error out if that happened to be the only dataset you provided.

------------------------------------------

### **4. Run generic_combine.R**
 ```
Combine data from read_in step, prior to ML
Usage:
    generic_combine.R [--subject_identifier=<subject_colname> --cor_level=<cor_level> --cor_choose=<cor_choose> --preserve_samples=<preserve_samples>] <input> <output_file>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]
    --cor_level level of general feature correlation [default: 0.95]
    --cor_choose choose which features are kept in correlation [default: FALSE]
    --preserve_samples attempt to drop more features to keep samples [default: FALSE]
    
Arguments:
    input  input directory containing files
    output_file  output file name 
 ```


