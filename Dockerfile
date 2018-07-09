# NOTE: this Dockerfile builds Metabase from source. We recommend deploying the pre-built
# images hosted on Docker Hub https://hub.docker.com/r/metabase/metabase/ which use the
# Dockerfile located at ./bin/docker/Dockerfile

FROM airdock/oracle-jdk

ENV JAVA_HOME=/srv/java/jdk
ENV PATH /usr/local/bin:$PATH
ENV LEIN_ROOT 1

ENV FC_LANG en-US
ENV LC_CTYPE en_US.UTF-8

# install core build tools
RUN apt-get update
RUN apt-get install -y nodejs git wget bash python make gcc g++ ca-certificates-java ttf-dejavu fontconfig npm && \
    npm install -g yarn && \
    ln -sf "${JAVA_HOME}/bin/"* "/usr/bin/"

# fix broken cacerts
#RUN rm -f /usr/lib/jvm/default-jvm/jre/lib/security/cacerts && \
#    ln -s /etc/ssl/certs/java/cacerts /usr/lib/jvm/default-jvm/jre/lib/security/cacerts

# install lein
ADD https://raw.github.com/technomancy/leiningen/stable/bin/lein /usr/local/bin/lein
RUN chmod 744 /usr/local/bin/lein

RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get install -y nodejs
# RUN ln -s /usr/bin/nodejs /usr/bin/node

# add the application source to the image
ADD . /app/source

# build the app
WORKDIR /app/source
RUN bin/build

# remove unnecessary packages & tidy up
# RUN apk del nodejs git wget python make g++
# RUN rm -rf /root/.lein /root/.m2 /root/.node-gyp /root/.npm /root/.yarn /root/.yarn-cache /tmp/* /var/cache/apk/* /app/source/node_modules

# expose our default runtime port
EXPOSE 3000 3001

# build and then run it
WORKDIR /app/source
HEALTHCHECK --interval=5s --timeout=3s CMD curl --fail  https://localhost:3000/api/health || exit 1
ENTRYPOINT ["./bin/start"]
