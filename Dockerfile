FROM rocker/rstudio

RUN apt-get update && \
	apt-get install -y default-jre default-jdk

RUN R -e "install.packages(c('foreach', 'dplyr', 'ggplot2', 'readxl', 'openxlsx', 'xlsx'), repos='https://cran.rstudio.com/')"
	

CMD /init

