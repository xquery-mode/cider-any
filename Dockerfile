FROM centos:centos7

MAINTAINER Artem Malyshev <proofit404@gmail.com>

ADD MarkLogic-RHEL7-8.0-5.8.x86_64.rpm /tmp/MarkLogic-RHEL7-8.0-5.8.x86_64.rpm

RUN yum -y install /tmp/MarkLogic-RHEL7-8.0-5.8.x86_64.rpm

ENV LD_LIBRARY_PATH /opt/MarkLogic/lib/:/data/Lib

EXPOSE 7997 7999 8000 8001 8002 8889

CMD /opt/MarkLogic/bin/MarkLogic && /bin/bash
