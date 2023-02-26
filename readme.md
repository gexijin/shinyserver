# Hosting Shiny apps via Nginx load balancer

 The free version of the Shiny server uses one R process and cannot handle more than a few concurrent users. In this repo, we set up a load balancer using Nginx that connects users with many instances of the Shiny server. This method is used in hosting [iDEP](http://bioinformatics.sdstate.edu/idep/) and  [ShinyGO](http://bioinformatics.sdstate.edu/go/).
## Prerequisites
  + A Linux server with port 80 open. This has been tested on Ubuntu and CentOS.
  + With Docker and Docker-compose installed. 
  + Git installed. 

## Steps
1.  Fork this repository. This way you have a copy of the repo that you can modify later. 
2.  Log in to the Linux server , clone your fork of the repository. You should use the URL from your forked repository.
 ```
 cd
 git clone https://github.com/gexijin/shinyserver.git
 ```
3. Build Docker images for both the Shiny server and the Nginx load balancer. 
```
cd ~/shinyserver/
sudo sh setup.sh 
```
4. Start the container. Your Shiny app is cloned 5 times in 5 Docker containers, which can run at the same time. You can change the script to run 30 instances. 
```
sudo sh restart.sh
```
5. The two Shiny apps are hosted at http://xx.xxx.xxx.xxx/app1/ and http://xx.xxx.xxx.xxx/app2/. Note that xxx.xxx is your ip address. 
App1 reads a local file stored in the data folder using relative path (../../data/)

6. Now you can clone a local copy of the your forked repo to your laptop. Replace the app.R code under the  app1 folder with your own R code for the Shiny app. Remember that your data needs to be stored in the data folder, either in the repo or upload directly to the Linux server. After your are done with the development, push your code to GitHub. Then update your code on the Linux server and restart the server:
```
cd ~/shinyserver/
sudo git pull
sudo sh restart.sh
```
