## Author: Andrew Oliver
## Version: aoliver44/nutrition_tools:base_0.3.0a.4
## Date: Jun 7, 2023

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
COPY utilities/lime_figures.R ./scripts/utilities/lime_figures.R
COPY utilities/shap_figures.R ./scripts/utilities/shap_figures.R
#COPY taxaHFE/taxaHFE.R ./scripts/taxaHFE
#COPY taxaHFE/taxaHFE_functions.R ./scripts/utilities/taxaHFE_functions.R
COPY models/dietML_null_tidy.R ./scripts/models/dietML_null_tidy.R

ENV PATH="${PATH}:/scripts/"

USER docker
