
 # **microbial_hfe.R**

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
