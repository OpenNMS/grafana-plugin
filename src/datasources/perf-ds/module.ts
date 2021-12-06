import { OpenNMSDatasource } from './datasource';
import { OpenNMSQueryCtrl } from './query_ctrl';
import '../../components/timeout';

class GenericConfigCtrl {
  static templateUrl = 'public/plugins/opennms-helm-app/datasources/perf-ds/partials/config.html';
}

class GenericQueryOptionsCtrl {
  static templateUrl = 'public/plugins/opennms-helm-app/datasources/perf-ds/partials/query.options.html';
}

export {
  OpenNMSDatasource as Datasource,
  OpenNMSQueryCtrl as QueryCtrl,
  GenericConfigCtrl as ConfigCtrl,
  GenericQueryOptionsCtrl as QueryOptionsCtrl,
};
