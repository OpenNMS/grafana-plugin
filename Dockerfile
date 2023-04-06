ARG GRAFANA_VERSION="latest"

FROM grafana/grafana:${GRAFANA_VERSION}

ARG OPENNMS_GRAFANA_PLUGIN_VERSION="bleeding"

LABEL maintainer "Jeff Gehlbach <jeffg@opennms.com>"

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

ADD artifacts/grafana-plugin*.tar.gz ${GF_PATHS_PLUGINS}/grafana-plugin

LABEL license="AGPLv3" \
      org.opennms.grafana-plugin.version="${OPENNMS_GRAFANA_PLUGIN_VERSION}" \
      vendor="The OpenNMS Group, Inc." \
      name="OpenNMS plugin for Grafana"
