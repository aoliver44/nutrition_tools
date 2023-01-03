## Author: Andrew Oliver
## Date updated: 1.3.2023
## Version: aoliver44/nutrition_tools:1.2

FROM rhub/r-minimal:4.2.0-patched

ENV RENV_VERSION=0.16.0

# install some things that R needs
RUN apk add libressl-dev libpng-dev build-base jpeg-dev gfortran libexecinfo-dev linux-headers

# install RENV, which will then install all R project packages
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# bootstrap RENV environment
COPY renv.lock ./
RUN R -e 'renv::consent(provided = TRUE)'
RUN R -e 'renv::restore(lockfile="renv.lock")'

# copy in scripts so they are part of container
COPY generic_read_in.R ./scripts/generic_read_in
COPY generic_combine.R ./scripts/generic_combine
COPY dietML.R ./scripts/dietML

ENV PATH="${PATH}:/root/scripts/"
