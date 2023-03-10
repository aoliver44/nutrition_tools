## Author: Andrew Oliver
## Version: aoliver44/nutrition_tools:base_1.5
## Date: Jan 19, 2023

## base image to start with
FROM rocker/r-base:4.2.0

## RENV version
ENV RENV_VERSION=0.16.0

RUN apt update
# install some things that R needs
RUN apt install -y libz-dev libxml2-dev

# install RENV, which will then install all R project packages
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# should be in the same directory as this file
COPY renv.lock ./
RUN R -e 'renv::consent(provided = TRUE)'
RUN R -e 'renv::restore()'

# copy in scripts so they are part of container
COPY ./generic_read_in.R ./scripts/generic_read_in
COPY ./generic_combine.R ./scripts/generic_combine
COPY ./dietML.R ./scripts/dietML
COPY models/dietML_glmnet.R ./scripts/models/dietML_glmnet.R
COPY models/dietML_glmnet_tidy_enet.R ./scripts/models/dietML_glmnet_tidy_enet.R
COPY models/dietML_glmnet_tidy_ridge_lasso.R ./scripts/models/dietML_glmnet_tidy_ridge_lasso.R
COPY models/dietML_ranger.R ./scripts/models/dietML_ranger.R
COPY models/dietML_ranger_tidy.R ./scripts/models/dietML_ranger_tidy.R
COPY models/ml_blast_test.R ./scripts/models/ml_blast_test.R
COPY utilities/lime_figures.R ./scripts/utilities/lime_figures.R
COPY utilities/shap_figures.R ./scripts/utilities/shap_figures.R

ENV PATH="${PATH}:/scripts/"

USER docker
