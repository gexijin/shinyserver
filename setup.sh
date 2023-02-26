# Based on digital ocean One click app: Docker-ce

docker build ./nginx/. -t nginx  --pull #nginx image should be build very quick

docker build . -t webapp --pull #webapp image need hours to build

echo 'Docker images have been built. Start downloading data.'


