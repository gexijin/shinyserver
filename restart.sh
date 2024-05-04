echo $(date) >> log.txt

# shut down
sudo docker-compose  down

# restart with multiple containers. Maximum is 10
sudo docker-compose up -d --scale webapp=10