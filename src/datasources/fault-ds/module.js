import {OpenNMSFMDatasource} from './datasource';
import {OpenNMSFMDatasourceQueryCtrl} from './query_ctrl';

class OpenNMSFMDatasourceConfigCtrl {}
OpenNMSFMDatasourceConfigCtrl.templateUrl = 'datasources/fault-ds/partials/config.html';

class OpenNMSFMDatasourceQueryOptionsCtrl {}
OpenNMSFMDatasourceQueryOptionsCtrl.templateUrl = 'datasources/fault-ds/partials/query.options.html';

export {
  OpenNMSFMDatasource as Datasource,
  OpenNMSFMDatasourceQueryCtrl as QueryCtrl,
  OpenNMSFMDatasourceConfigCtrl as ConfigCtrl,
  OpenNMSFMDatasourceQueryOptionsCtrl as QueryOptionsCtrl,
};
