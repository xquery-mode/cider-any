FROM centos:centos7

MAINTAINER Artem Malyshev <proofit404@gmail.com>

ADD MarkLogic-RHEL7-8.0-5.8.x86_64.rpm /tmp/MarkLogic-RHEL7-8.0-5.8.x86_64.rpm

RUN yum -y install /tmp/MarkLogic-RHEL7-8.0-5.8.x86_64.rpm

EXPOSE 7997 7998 7999 8000 8001 8002

CMD /usr/bin/env LD_LIBRARY_PATH=/opt/MarkLogic/lib/:/data/Lib /opt/MarkLogic/bin/MarkLogic && \
 mkdir -p /data/Logs && \
 touch /data/Logs/ErrorLog.txt && \
 tailf /data/Logs/ErrorLog.txt
