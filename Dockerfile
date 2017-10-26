FROM grafana/grafana:4.6.0

ARG NODEJS_SETUP_SCRIPT_URL=https://deb.nodesource.com/setup_6.x
ARG YARN_KEY_URL=https://dl.yarnpkg.com/debian/pubkey.gpg
ARG OPENNMS_HELM_GIT_URL=https://github.com/OpenNMS/opennms-helm.git
ARG OPENNMS_HELM_GIT_BRANCH_REF="master"
ARG OPENNMS_HELM_APP_DIR=opennms-helm-app
ENV GF_PATHS_PLUGINS /opt/grafana/plugins
ENV OPENNMS_HELM_HOME=${GF_PATHS_PLUGINS}/${OPENNMS_HELM_APP_DIR}

RUN apt-get update && \
    apt-get -y --no-install-recommends install apt-transport-https && \
    curl -sS ${YARN_KEY_URL} | apt-key add - && \
    echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    curl -sL ${NODEJS_SETUP_SCRIPT_URL} | bash - && \
    apt-get update && \
    apt-get -y --no-install-recommends install git-core \
                                       yarn \
                                       nodejs && \
    apt-get -y autoremove && \
    rm -rf /var/lib/apt/lists/*

RUN git clone ${OPENNMS_HELM_GIT_URL} ${OPENNMS_HELM_HOME} && \
    cd ${OPENNMS_HELM_HOME} && \
    git checkout ${OPENNMS_HELM_GIT_BRANCH_REF} && \
    git describe --all > git.describe && \
    yarn && \
    yarn build

HEALTHCHECK --interval=10s --timeout=3s CMD curl --fail -s -I http://localhost:3000/login | grep "HTTP/1.1 200 OK" || exit 1

VOLUME ["/opt/grafana/plugins", "/var/lib/grafana", "/var/log/grafana", "/etc/grafana"]

EXPOSE 3000

LABEL maintainer="ronny@opennms.org" \
      grafana.version="4.4.1" \
      opennms.helm.git.url=${OPENNMS_HELM_GIT_URL} \
      opennms.helm.git.branch.ref=${OPENNMS_HELM_GIT_BRANCH_REF} \
      opennms.helm.license=MIT \
      opennms.helm.home=${OPENNMS_HELM_HOME}

ENTRYPOINT ["/run.sh"]
