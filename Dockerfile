FROM centos:centos7

MAINTAINER Artem Malyshev <proofit404@gmail.com>

ADD MarkLogic.rpm /tmp/MarkLogic.rpm

RUN yum -y install /tmp/MarkLogic.rpm

EXPOSE 7997 7998 7999 8000 8001 8002

CMD /usr/bin/env LD_LIBRARY_PATH=/opt/MarkLogic/lib/:/data/Lib /opt/MarkLogic/bin/MarkLogic && \
 mkdir -p /data/Logs && \
 touch /data/Logs/ErrorLog.txt && \
 tailf /data/Logs/ErrorLog.txt
