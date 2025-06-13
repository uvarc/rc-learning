# Base image: rocky Linux
FROM rockylinux:9
# Install dependencies
RUN dnf -y update && \
    dnf -y install wget tar which git && \
    dnf clean all
# Installing Hugo (extended version)
ENV HUGO_VERSION 0.117.0
RUN wget https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_extended_${HUGO_VERSION}_Linux-64bit.tar.gz && \
    tar -zxvf hugo_extended_${HUGO_VERSION}_Linux-64bit.tar.gz && \
    mv hugo /usr/local/bin/hugo && \
    rm hugo_extended_${HUGO_VERSION}_Linux-64bit.tar.gz
WORKDIR /rc-learning
COPY . /rc-learning/
# Expose port 1313
EXPOSE 1313
# Run Hugo server
CMD ["hugo", "server", "--bind", "0.0.0.0"]
# docker build -t rc-learning .
# docker run -it -p 1313:1313 rc-learning