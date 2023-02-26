# Hosting Shiny apps via Nginx load balancer

 
 
## Prerequisites
  + A Linux server with port 80 open. 
  + With Docker and Docker-compose installed. 
  + Git installed. 

1.  Fork this repository and make changes to the Shiny apps
2.  Clone your fork of the repository
 ```
 cd
 git clone https://github.com/gexijin/shinyserver.git
 ```
3. Build Docker images for both the Shiny server and the Nginx load balancer. 
```
cd ~/shinyserver/
sudo sh setup.sh 
```
4. Start the container. 
```
sudo sh restart.sh
```
5. The two Shiny apps are hosted at http://xx.xxx.xxx.xxx/app1/ and http://xx.xxx.xxx.xxx/app2/. Note that xxx.xxx is your ip address. 
App1 reads a local file stored in the data folder using relative path (../../data/)
