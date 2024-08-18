# DESPLEGAR CONTENEDOR DE DOCKER BASADO EN UNA IMAGEN DE DOCKER PREVIAMENTE CREADA

[Source](https://www.statworx.com/en/content-hub/blog/how-to-dockerize-shinyapps/)


# NAVEGACIÓN (Ctrl + clic)

- [Sección 1](#VERIFICACIONES)

- [Sección 2](#PAQUETES_RSTUDIO)

- [Sección 3](#GIT_IGNORE)

- [Sección 4](#DOCKERFILE)

- [Sección 5](#CONSTRUCCIÓN)

- [Sección 6](#CONTENEDOR)

- [Sección 7](#FORMATO_README)


# VERIFICACIONES

Verificar docker en el sistema:
``` docker --version ```

Comprobar Docker-Compose en el sistema:
``` docker-compose --version ```

Verificar el servicio de Docker (ejecución): 
``` sudo systemctl status docker ```

Si Docker está instalado pero no está en ejecución, inicializar:
``` sudo systemctl start docker ```


# PAQUETES_RSTUDIO

Acceder a la aplicación de Shiny e instalar el siguiente paquete:
``` install.packages("renv") ```

Posteriormente, ejecutar en la terminal (dentro de el proyecto):
``` renv::init() ```

Si lo anterior genera un error, verifique la ruta en la que se encuentra el proyecto de la siguiente manera:
``` getwd() ```

De ser necesario, restablezca el directorio actual hacia el directorio del la aplicación:
``` setwd("~/Desktop/SHINY-APPS/shiny_app") ```

Ejecutar de nuevo:
``` renv::init() ```
OR
``` renv::init(force = TRUE) ```

En el directorio raíz de la aplicación, *Renv* debió crear un archivo-directorio con todas la bibliotecas y dependencias.


# GIT_IGNORE

Fuera del directorio raíz de la aplicación, crear un archivo .gitignore, debe contener:

```
    .Rproj.user
    .Rhistory
    .RData
    .Ruserdata
    .DS_Store
    *.md

    # log files
    bash_script_collection/docker_log_files
```

*NOTA*: El archivo .gitignore es meramente opcional.


# DOCKERFILE

Este Dockerfile construye una imagen Docker que contiene todos los requisitos necesarios para ejecutar una aplicación Shiny de R de manera reproducible. 

Automáticamente instala las dependencias del sistema, copia los archivos necesarios, restaura los paquetes R especificados, y configura la aplicación Shiny para ser accesible a través del puerto 8787.

Fuera del directorio raíz de la aplicación, crear un archivo Dockerfile, debe contener:

``` 
    # Base image https://hub.docker.com/u/rocker/
    FROM rocker/shiny:latest

    # system libraries of general use
    ## install debian packages
    RUN apt-get update -qq && apt-get -y --no-install-recommends install \
        libxml2-dev \
        libcairo2-dev \
        libsqlite3-dev \
        libmariadbd-dev \
        libpq-dev \
        libssh2-1-dev \
        unixodbc-dev \
        libcurl4-openssl-dev \
        libssl-dev 

    ## Cmake and other neccesary dependences
    RUN apt-get update && apt-get install -y \
        cmake \
        libnlopt-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev 

    # Install R and other dependencies R
    RUN R -e "install.packages('future')"

    ## update system libraries
    RUN apt-get update && \
        apt-get upgrade -y && \
        apt-get clean && \
        rm -rf /var/lib/apt/lists/*

    ## copy necessary files
    ## renv.lock file
    COPY /shiny_app/renv.lock ./renv.lock

    ## app folder
    COPY /shiny_app ./app

    ## install renv & restore packages
    RUN Rscript -e 'install.packages("renv")'
    RUN Rscript -e 'renv::restore()'

    ## expose port
    EXPOSE 8787

    ## run app on container start
    CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 8787)"]
```


# CONSTRUCCIÓN

Dentro del directorio que contiene el Dockerfile, ejecutar:
``` sudo docker build -t my-shinyapp-image . ```

Lo anterior debe construir una imagen Docker que contenga la aplicación de Shiny, utilizando las instrucciones definidas dentro del Dockerfile.


# CONTENEDOR

Una vez finalizado el proceso de construcción, debe probar la imagen recién creada iniciando un contenedor:
``` sudo docker run -d --rm -p 8787:8787 my-shinyapp-image ```

Detener en cualquier momento el contenedor de la siguiente forma:
``` sudo docker stop <idDelContenedorDelDocker> ```

Si la ejecución se detiene automaticamente, revise los archivos logs del contenedor:
``` sudo docker logs <IdDelContenedor> ```


# FORMATO_README

# *Carácter utilizado para representar secciones (títulos) al interior del archivo.*

- *Carácter utiizado para representar una lista de elementos.*

## *Caracteres utilizados para representar sub-secciones (subtítulos) al interior del archivo.*

* *Carácter utilizado para resaltar palabras, terminos y fragmentos importantes*

### *Caracteres utilizados para representar sub-sub-secciones (sub-subtítulos) al interior del archivo.*

[''](#) Caracteres que permiten navegar al interior del archivo o ir a sitios externos.

``` *Caracteres utilizados para representar comandos o líneas de codigo.*