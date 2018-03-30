import {OpenNMSDatasource} from './datasource';
import {OpenNMSQueryCtrl} from './query_ctrl';
import {loadPluginCss} from 'app/plugins/sdk';
import '../../components/timeout';

class GenericConfigCtrl {}
GenericConfigCtrl.templateUrl = 'datasources/perf-ds/partials/config.html';

class GenericQueryOptionsCtrl {}
GenericQueryOptionsCtrl.templateUrl = 'datasources/perf-ds/partials/query.options.html';

loadPluginCss({
  dark: 'plugins/opennms-helm-app/datasources/perf-ds/css/opennms.dark.css',
  light: 'plugins/opennms-helm-app/datasources/perf-ds/css/opennms.light.css'
});

export {
  OpenNMSDatasource as Datasource,
  OpenNMSQueryCtrl as QueryCtrl,
  GenericConfigCtrl as ConfigCtrl,
  GenericQueryOptionsCtrl as QueryOptionsCtrl
};
