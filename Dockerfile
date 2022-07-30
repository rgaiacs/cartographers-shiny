FROM mambaorg/micromamba AS base
USER $MAMBA_USER
RUN micromamba install -y -n base -c conda-forge \
    r-base \
    r-tidyverse \
    r-shiny \
    && micromamba clean --all --yes

FROM base AS production
COPY . /src/cartographers/
WORKDIR /src/cartographers/
CMD Rscript -e 'library(shiny); runApp("/src/cartographers/", port=3030, launch.browser=FALSE)'

