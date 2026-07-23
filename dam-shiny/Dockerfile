# ============================================================
# dam-shiny — container image
#
# Serves the app via shiny-server on port 3838 at path /dam-shiny/.
# Dependencies are restored from renv.lock for reproducibility; the
# restore reuses the base image's configured (binary) package repo so
# the build does not compile every package from source.
#
# Build:  docker build -t dam-shiny .
# Run:    docker run --rm -p 3838:3838 dam-shiny
# Open:   http://localhost:3838/dam-shiny/
# ============================================================

FROM rocker/shiny:latest

# -- System libraries the R packages link against --------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
      libxml2-dev \
      libssl-dev \
      libcurl4-openssl-dev \
      zlib1g-dev \
      libpng-dev \
      libjpeg-dev \
      libtiff5-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# -- Restore R dependencies from the lockfile ------------------------
# renv is pinned; the repos override points renv at the base image's
# repositories (Posit Package Manager binaries on rocker) for a fast
# restore instead of source compilation.
WORKDIR /srv/shiny-server/dam-shiny
RUN Rscript -e 'install.packages("renv", repos = getOption("repos"))'
COPY renv.lock renv.lock
RUN Rscript -e 'options(renv.config.repos.override = getOption("repos")); \
                renv::restore(lockfile = "renv.lock", prompt = FALSE)'

# -- App code --------------------------------------------------------
COPY app.R config.yml ./
COPY R/    R/
COPY inst/ inst/

# shiny-server serves every subdirectory of /srv/shiny-server as an app,
# so this app is reachable at /dam-shiny/. Fix ownership for the runtime user.
RUN chown -R shiny:shiny /srv/shiny-server/dam-shiny

EXPOSE 3838

# Base image's CMD launches shiny-server; keep it.
