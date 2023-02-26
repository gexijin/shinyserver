echo $(date) >> log.txt

# shut down
sudo docker-compose  down

# restart with multiple containers.
sudo docker-compose up -d --scale webapp=30