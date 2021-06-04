FROM rocker/tidyverse:4.1.0

# Install dependencies
RUN R -e "install.packages(c('RCurl'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Copy script
COPY license-report.R /license-report.R

# Set entrypoint
ENTRYPOINT ["/license-report.R"]
