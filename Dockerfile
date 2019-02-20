ARG GRAFANA_VERSION="latest"

FROM grafana/grafana:${GRAFANA_VERSION}

ARG OPENNMS_HELM_VERSION="bleeding"
ARG OPENNMS_HELM_PKG="opennms-helm_3.0.1-SNAPSHOT.tar.gz"

LABEL maintainer "Ronny Trommer <ronny@opennms.org>"

USER grafana

ARG GF_INSTALL_PLUGINS=""

RUN if [ ! -z "${GF_INSTALL_PLUGINS}" ]; then \
    OLDIFS=$IFS; \
        IFS=','; \
    for plugin in ${GF_INSTALL_PLUGINS}; do \
        IFS=$OLDIFS; \
        grafana-cli --pluginsDir "${GF_PATHS_PLUGINS}" plugins install ${plugin}; \
    done; \
fi

ADD dist/packages/${OPENNMS_HELM_PKG} ${GF_PATHS_PLUGINS}/opennms-helm

LABEL license="AGPLv3" \
      org.opennms.helm.version="${OPENNMS_HELM_VERSION}" \
      vendor="The OpenNMS Group, Inc." \
      name="Grafana Helm App"
