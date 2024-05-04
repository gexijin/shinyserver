# Scaling Shiny apps securely

 The free version of the [Shiny server](https://posit.co/products/open-source/shinyserver/) uses one R process on a single CPU core. Therefore it can handle few concurrent users. In this repo, we set up a load balancer using Nginx that connects users with one of the many instances of the Shiny server deployed as Docker containers. Developed by Kevin Son, a graduate student in the Ge lab, this method has been used to host large apps like [iDEP](http://bioinformatics.sdstate.edu/idep/) and  [ShinyGO](http://bioinformatics.sdstate.edu/go/) for many years.  To enable the https protocol, a similar [setup](https://github.com/gexijin/RTutor_server) is used by [https://RTutor.ai](https://RTutor.ai) and [https://Chatlize.ai](https://chatlize.ai).
 
## Prerequisites
  + A Linux server with port 80 and 443 open. This has been tested on Ubuntu and CentOS. 
  + Make sure [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) is installed.

## Steps
1.  Install Docker and Docker-compose.
    ```
    curl -fsSL https://get.docker.com -o install_docker.sh
    sudo sh install_docker.sh
    sudo apt install docker-compose
    ```
2.  Fork this repository, which contains two demo Shiny apps under the [shinyapps folder](https://github.com/gexijin/shinyserver/tree/main/shinyapps). You can replace these apps with your own once the server is set up. Log in to the Linux server, and clone the forked repository. Here I am using the URL for this repo.
     ```
     cd
     git clone https://github.com/gexijin/shinyserver.git
     ```
3. Change the nginx.conf file in the nginx folder so that the server_name reflects your own server.
   
4. Build the Nginx Docker image as specified by this [Dockerfile](https://github.com/gexijin/shinyserver/blob/main/nginx/Dockerfile).
    ```
    cd ~/shinyserver/
    sudo docker build ./nginx/. -t nginx  --pull
    ```
5. Build the Docker image for the Shiny server, as configured by this [Dockerfile](https://github.com/gexijin/shinyserver/blob/main/Dockerfile). This process might be slow, as all the R packages needed for the Shiny app must be pre-installed, using this [R script](https://github.com/gexijin/shinyserver/blob/main/config/librarySetup.R), which needs to be changed according to your app. Note that the docker image has to be called 'webapp'.

    ```
    cd ~/shinyserver/
    sudo docker build . -t webapp --pull
    ```
6. Obtain SSL certificate and key files. You can buy SSL certificate. To obtain a certificate freely, try [Certbot](https://certbot.eff.org/). The fullchain.pem and privkey.pem files should be saved to the nginx folder.


   
7. Start the containers. Your Shiny app is cloned in 10 Docker containers, which can run at the same time.  These containers are managed by the Nginx container, as specified in the [Docker-compose.yml](https://github.com/gexijin/shinyserver/blob/main/docker-compose.yml) file. Depending on your resources, you can change the number of containiners. Make sure you edit the [Nginx configuration file.](https://github.com/gexijin/shinyserver/blob/main/nginx/nginx.conf) and this command, so that they are exactly the same.
    ```
    sudo docker-compose up -d --scale webapp=10
    ```
8. The two Shiny apps are hosted at http://xx.xxx.xxx.xxx/app1/ and http://xx.xxx.xxx.xxx/app2/. Note that xxx.xxx is your ip address. Also test the https by visiting https://yourdomain.com/app1/. App1 reads a local file stored in the data folder using relative path (../../data/). App2 is the demo app from RStudio.

9. Replace the apps and deploy. Now you can clone a local copy of your forked repo to your laptop. Replace the app.R code under the  app1 folder with your own R code for the Shiny app.  After your are done with the development, push your code to GitHub. Then update your code on the Linux server and restart the server:
    ```
    cd ~/shinyserver/
    sudo git pull
    sudo docker-compose down  # shut down
    sudo docker-compose up -d --scale webapp=10
    ```
10. Data files. Your data needs to be stored in the shinyserver/data folder, either in the repo or upload directly to the Linux server. From the Shiny apps, these files can be read in by ```df <- read.csv("../../data/demo_data.csv")```.
11. R packages. All the R packages need to be listed in [librarySetup.R](https://github.com/gexijin/shinyserver/blob/main/config/librarySetup.R) file under the config folder. These packages needs to be installed when building the docker image for the Shiny server. If a new package is added to your shiny app, the image needs to be rebuild by repeating step 5. 

Please let [me](https://twitter.com/StevenXGe) know if you have any questions or comments. 
