import {FlowDatasource} from './datasource';
import {FlowDatasourceQueryCtrl} from './query_ctrl';
import '../../components/timeout';

class GenericConfigCtrl {}

GenericConfigCtrl.templateUrl = 'datasources/flow-ds/partials/config.html';

class GenericQueryOptionsCtrl {}

GenericQueryOptionsCtrl.templateUrl = 'datasources/flow-ds/partials/query.options.html';

export {
  FlowDatasource as Datasource,
  FlowDatasourceQueryCtrl as QueryCtrl,
  GenericConfigCtrl as ConfigCtrl,
  GenericQueryOptionsCtrl as QueryOptionsCtrl
};
