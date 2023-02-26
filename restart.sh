echo $(date) >> log.txt

# shut down
sudo docker-compose -f /home/ubuntu/RTutor_server/docker-compose.yml down

# restart with multiple containers.
sudo docker-compose -f /home/ubuntu/RTutor_server/docker-compose.yml up -d --scale webapp=30