#!/bin/bash
## Probs needs updating for dietML parameters (AO 3.29.23)
base_dir=/home/docker/data/

while read input meta label sample levels metaphlan factor; do

g=$(basename $input .tsv)
mkdir -p ${base_dir}${g}
/home/docker/microbial_HFE/taxaHFE.R --subject_identifier ${sample} --label ${label} --feature_type ${factor} --format_metaphlan ${metaphlan} --write_old_files TRUE --ncores 4 ${base_dir}${meta} ${base_dir}${input} ${base_dir}${g}/${g}.txt 

cd ${base_dir}${g}

for f in {4..7}; do 
dietML --subject_identifier ${sample} --label ${label} --cor_level 0.8 --train_split 0.7 --model rf --type classification --metric bal_accuracy --tune_length 80 --tune_time 10 --shap FALSE --ncores 4 ${base_dir}${g}/*_level_${f}.csv ${base_dir}${g}/
mv ${base_dir}${g}/ML_r_workspace.rds ${base_dir}${g}/ML_r_workspace_L${f}.rds
done

dietML --subject_identifier ${sample} --label ${label} --cor_level 0.8 --train_split 0.7 --model rf --type classification --metric bal_accuracy --tune_length 80 --tune_time 10 --shap FALSE --ncores 4 ${base_dir}${g}/${g}.txt ${base_dir}${g}/
mv ${base_dir}${g}/ML_r_workspace.rds ${base_dir}${g}/ML_r_workspace_HFE.rds

dietML --subject_identifier ${sample} --label ${label} --cor_level 0.8 --train_split 0.7 --model rf --type classification --metric bal_accuracy --tune_length 80 --tune_time 10 --shap FALSE --ncores 4 ${base_dir}${g}/${g}no_sf.txt ${base_dir}${g}/
mv ${base_dir}${g}/ML_r_workspace.rds ${base_dir}${g}/ML_r_workspace_HFE_no_sf.rds

cd ${base_dir}

done < ${base_dir}manifest.txt
