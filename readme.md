# Hosting Shiny apps via Nginx load balancer

 The free version of the [Shiny server](https://posit.co/products/open-source/shinyserver/) uses one R process on a single CPU core. Therefore it can handle few concurrent users. In this repo, we set up a load balancer using Nginx that connects users with many instances of the Shiny server deployed as Docker containers. Developed by Kevin Son, a graduate student in the Ge lab, this method is used to host [iDEP](http://bioinformatics.sdstate.edu/idep/) and  [ShinyGO](http://bioinformatics.sdstate.edu/go/).
 
## Prerequisites
  + A Linux server with port 80 open. This has been tested on Ubuntu and CentOS.
  + Make sure [Docker](https://docs.docker.com/get-docker/) and [Docker-compose plug-in](https://docs.docker.com/compose/install/linux/) are  installed and the Docker engine is running.
  + Make sure [Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) is installed.

## Steps
1.  Fork this repository, which contains two demo Shiny apps under the [shinyapps folder](https://github.com/gexijin/shinyserver/tree/main/shinyapps). You can replace these apps with your own once the server is set up. 
2.  Log in to the Linux server, and clone the forked repository. Here I am using the URL for this repo.
 ```
 cd
 git clone https://github.com/gexijin/shinyserver.git
 ```
3. Build the Nginx Docker image as specified by this [Dockerfile](https://github.com/gexijin/shinyserver/blob/main/nginx/Dockerfile).
```
cd ~/shinyserver/
sudo docker build ./nginx/. -t nginx  --pull
```
4. Build the Docker image for the Shiny server, as configured by this [Dockerfile](https://github.com/gexijin/shinyserver/blob/main/Dockerfile). This process might be slow, as all the R packages needed for the Shiny app must be pre-installed, using this [R script](https://github.com/gexijin/shinyserver/blob/main/config/librarySetup.R), which needs to be changed according to your app.

```
cd ~/shinyserver/
sudo docker build . -t webapp --pull
```
5. Start the containers. Your Shiny app is cloned 5 times in 5 Docker containers, which can run at the same time. You can change the script to run 30 instances. These containers are managed by the Nginx container, as specified in the [Docker-compose.yml](https://github.com/gexijin/shinyserver/blob/main/docker-compose.yml) file. 
```
sudo docker-compose up -d --scale webapp=5
```
6. The two Shiny apps are hosted at http://xx.xxx.xxx.xxx/app1/ and http://xx.xxx.xxx.xxx/app2/. Note that xxx.xxx is your ip address. 
App1 reads a local file stored in the data folder using relative path (../../data/). App2 is the demo app from RStudio.

7. Replace the apps and deploy. Now you can clone a local copy of your forked repo to your laptop. Replace the app.R code under the  app1 folder with your own R code for the Shiny app. Remember that your data needs to be stored in the shinyserver/data folder, either in the repo or upload directly to the Linux server. After your are done with the development, push your code to GitHub. Then update your code on the Linux server and restart the server:
```
cd ~/shinyserver/
sudo git pull
sudo sh restart.sh
```

Please let [me](https://twitter.com/StevenXGe) know if you have any questions or comments. 
