import { loadPluginCss } from '@grafana/runtime';

import { OpenNMSDatasource } from './datasource';
import { OpenNMSQueryCtrl } from './query_ctrl';
import '../../components/timeout';

class GenericConfigCtrl {
  static templateUrl = 'datasources/perf-ds/partials/config.html';
}

class GenericQueryOptionsCtrl {
  static templateUrl = 'datasources/perf-ds/partials/query.options.html';
}

loadPluginCss({
  dark: 'plugins/opennms-helm-app/datasources/perf-ds/css/opennms.dark.css',
  light: 'plugins/opennms-helm-app/datasources/perf-ds/css/opennms.light.css',
});

export {
  OpenNMSDatasource as Datasource,
  OpenNMSQueryCtrl as QueryCtrl,
  GenericConfigCtrl as ConfigCtrl,
  GenericQueryOptionsCtrl as QueryOptionsCtrl,
};
