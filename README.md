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
git clone https://github.com/aoliver44/nutrition_tools.git
cd nutrition_tools
```
This will pull down the scripts and dockerbuild files necessary to run these scripts

--------------------------------------------------

### **2. Build the Docker Environment**

 ```
## Option 1 (preferred), local installation:
docker pull aoliver44/nutrition_tools:1.1

## Option 2: Build it yourself! Still local installation.
## On my quad-core i7 2020 macbook pro, took 43 minutes. 
docker build -t nutrition_tools:latest .

## to run
docker run --rm -it -v /path/to/data:/home/data -v /path/to/github_repo/nutrition_tools:/home/ aoliver44/nutrition_tools:1.1 bash

## Option 3: You are using singularity (assuming its in your path.
## you might need to load a module or something). Usually remote installation.
## **NOTE:** I do not know much about singularity
## pull image from internet
singularity pull nutrition_tools.sif docker://aoliver44/nutrition_tools:1.1
## run image
singularity run -w -W /path/to/working/directory --bind /path/to/cloned/github/repo:/home nutrition_tools.sif bash
cd /home

########### example (local installation): ###########

docker run --rm -it -v /Users/$USER/Downloads/nutrition_tools/:/home aoliver44/nutrition_tools:1.1 bash
cd /home/
 ```

The above example command (entierly dependent on where you downloaded the repository to your computer...in this case it was downloaded to a folder with the path ~/Downloads) will start the docker container and provide a bash terminal to the user. 

When you ``cd /home/``, you should see whatever you mounted to this directory inside the docker container (whatever is in the folder before the ":" in the above command, in this case the directory "/User/$USER/Downloads/nutrition_tools/")

You are now operating inside a container, which contains the software necessary to run the following analyses.

**NOTE:** To exit the terminal, run ```exit``` or press ctrl+d on a mac

------------------------------------------

### **3. Run generic_read_in**

```
generic_read_in -h
Usage: generic_read_in [options]

Options:
	-i CHARACTER, --input=CHARACTER
		path to folder with data to import [default= /home/data/]

	-s CHARACTER, --subject_identifier=CHARACTER
		subject key (column name) found in all files [default= subject_id]

	-h, --help
		Show this help message and exit

########### example: ###########

generic_read_in --subject_identifier subject_id /home/simulated_data/ /home/simulated_output 

```
**Desired input:** A flat-file(s) where each row has a unique identifier (a subject or sample ID) and each column is some feature measured. Should multiple files exist, the unique identifier will be present in all files.

This script will read in a directory of files (.csv | .txt | .tsv) and attempt to run some basic checks on them and basic cleaning. For example:
1. Check to see if a key is present that will link all the data together if split in multiple files (i.e. "subject_id")
2. Check to see if subject_id is unique across rows or if there are duplicated values
   - Duplicated subject ids suggest longitudinal data. The downstream ML methods in this work does not work well with longitudinal data 
3. Check to see if there are duplicated column names
   - Sub-check: if the column names are duplicated, is the data also duplicated?
4. Keep a tally of columns that contain NAs.
5. Check to see if you have time-series data and discourage htat
   - Note,  this program and many ML programs struggle with longitudinal data
   
**Output:**
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

It is good practice generally, and vital for these scripts to work properly, for you to supply full paths to directories (inside the docker container). The examples here show this, i.e. ```/home/simulated_data``` and **NOT** just ```simulated_data``` or ```~/simulated_data```

------------------------------------------

### **4. Run generic_combine**
 ```
Combine data from read_in step, prior to ML
Usage:
    generic_combine [--subject_identifier=<subject_colname> --cor_level=<cor_level> --cor_choose=<cor_choose> --preserve_samples=<preserve_samples>] <input> <output_file>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --subject_identifier name of columns with subject IDs [default: subject_id]
    --label label of column for use in ML [default: label]
    --cor_level level of general feature correlation [default: 0.99]
    --cor_choose choose which features are kept in correlation [default: FALSE]
    --preserve_samples attempt to drop more features to keep samples [default: FALSE]
    
Arguments:
    input  input directory containing files
    output_file  output file name  

########### example: ###########

generic_combine --subject_identifier subject_id --label label --cor_level 0.99 --cor_choose TRUE --preserve_samples FALSE /home/simulated_output/ merged_data.csv
 ```

This script will take the output of ```./generic_read_in``` and combine all the files together. It will do 3 major things:
1. Check and see if problems identified in generic_read_in.R step were addressed. If they were not addressed, these problematic datasets will be DROPPED.
2. Correlate (Spearman) combined featureset at a user-defined threshold [default: 0.99]. This is to identify HIGHLY redundant features. We do not view this as a feature engineering step, because the threshold is so high. In the next ML step, a correlation-based feature engineering step can set a much lower threshold more safely (does not contribute to data leakage like this step could).
3. For the features that are correlated at > 0.99, this program will let the user choose the features that get written to the final dataset. 
   - Note: If your response var of interest is correlated with another feature(s) at > 0.99, please take care to make sure you choose the response var to end up in the final dataset.
4. One-hot encode factors
    - Note: the method for one-hot encoding here creates new factors with new names. Note, it will **NOT** one-hot encode the label that will be used in ML (the factor you want to predict...***if it is a factor***). This is because the core of downstream ML (dietML.R) is the R program ranger. Ranger expects the label used in RF classification to **NOT** be a 0,1 (which is exactly what one-hot encoding does).
  
**OUTPUT:** (1) A .csv file for use in ML in the directory of the input. (2) a feature_summary.csv file which tells you what features you started with
  
  **NOTES:** The ```--preserve_samples``` flag attempts to lower the threshold of what is considered a feature with too many NA's (a sample with any amounts of NAs gets dropped...so it is a balancing act of dropping features and samples in order to have the most complete dataset). The final dataset should be complete; this version of this pipeline does not impute missing data. Default behavior is set to false, which for most of our test cases did not lead to many "extra" lost samples. IF you have a dataset with many NA-replete features, consider setting this flag to ```--preserve_samples TRUE```; however, expect a loss of features.


------------------------------------------

### **5. Run diet_ML**

```
Run random forest regression or classification on a dataframe
Usage:
    dietML [--label=<label> --cor_level=<cor_level> --train_split=<train_split> --type=<type> --seed=<seed> --ncores=<ncores>] <input> <outdir>
    
Options:
    -h --help  Show this screen.
    -v --version  Show version.
    --label=<label> name of column that you are prediction [default: label]
    --cor_level level to group features together [default: 0.80]
    --train_split what percentage of samples should be used in training [default: 0.70]
    --type are you trying to do classification (discrete levels of label) or regression (continous) [default: classification]
    --seed random seed for reproducible results [default: 42]
    --ncores number of processesing cores for parallel computing [default: 2]
    
Arguments:
    input  path to input file for ML (output from generic_combine.R)
    outdir path where results should be written 

########### example: ###########
dietML --label label --cor_level 0.80 --train_split 0.7 --type classification --ncores 2 /home/simulated_output/merged_data.csv ml_results/
```

The final script in this pipeline takes a clean (no missing data!) dataframe and performs a (relatively) basic ML analysis. 

If your factor of interest in discrete (categorical), this analysis will be Random Forest classification. RF classification has shown to generally outperform many other single model algorithms and be fairly robust against over-fitting. 

If your factor is continouous, this analysis will be a Random Forest regression. RF regression has similar benefits as RF classification. 

For both analyses, data will default to a train-test split of 0.70 (70% of data will be used to train the model, and 30% will be COMPLETELY left out in order to test final model). Inside model building, a 10-fold repeated (3x) cross-validation procedure will be used to evaluate pre-processessing steps and hyperparameters. 
   - Preprocessing steps: Near-zero variance filtering, and correlation filtering (default: 0.8 pearson correlation)
   - Hyperparameter tuning: an exhaustive grid search of 3 hyperparameters (mtry, splitrule, and minimum node size). These hyperparameters were chosen because they are the main ones tuned inside the R Caret package for the Ranger random forest models. 