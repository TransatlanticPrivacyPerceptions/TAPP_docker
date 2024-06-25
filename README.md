# TAPP - Dockerised Shiny App

This repository deployes the TAPP Shiny application using Docker.

The results of the TAPP Shiny App can be accesed through 
[Results - Transatlantic Privacy Perceptions](https://privacyperceptions.org/results/).

The repository contains all the Shiny app files and other supporting files, as well as a Dockerfile and a `build-docker.yml` file to set up a github action to build the docker image.

## Run a new version

To run a new version of the Shiny app:

1. Push the new changes to github.
2. Re-run the build by clicking "Run workflow" [on this page](https://github.com/TransatlanticPrivacyPerceptions/TAPP_docker/actions/workflows/build-docker.yml).
3. The new created docker image can be found [here](https://github.com/TransatlanticPrivacyPerceptions/TAPP_docker/pkgs/container/tapp_docker).

## Run on your computer

To run the dockerised Shiny app on your computer:

1. Make sure you have Docker installed.
2. Run `docker run -p 3838:3838 ghcr.io/transatlanticprivacyperceptions/tapp_docker:latest` in your terminal.
3. You can then access the Shiny App through [this link](http://localhost:3838).
