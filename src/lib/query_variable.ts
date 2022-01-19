import _ from 'lodash';
import { stringToJsRegex } from '@grafana/data';

import { assignModelProperties, containsVariable, setOptionAsCurrent, setOptionFromUrl, validateVariableSelectionState } from './utils';

function getNoneOption() {
  return { text: 'None', value: '', isNone: true, selected: false };
}

export class QueryVariable {
  type = 'query';
  name = '';
  label = null as string | null;
  hide = 0;
  skipUrlSync = false;
  datasource = null as string | null;
  query = '';
  regex = '';
  sort = 0;
  refresh = 0;
  multi = false;
  includeAll = false;
  allValue = null as any[] | null;
  options = [] as any[];
  current = {} as any;
  tags = [] as any[];
  useTags = false;
  tagsQuery = '';
  tagValuesQuery = '';
  definition = '';
  index = -1;

  defaults = {
    type: 'query',
    name: '',
    label: null,
    hide: 0, // VariableHide.dontHide,
    skipUrlSync: false,
    datasource: null,
    query: '',
    regex: '',
    sort: 0, // VariableSort.disabled,
    refresh: 0, // VariableRefresh.never,
    multi: false,
    includeAll: false,
    allValue: null,
    options: [],
    current: {}, // as VariableOption,
    tags: [],
    useTags: false,
    tagsQuery: '',
    tagValuesQuery: '',
    definition: '',
    index: -1,
  };

  initialized: boolean;
  inFlight: Promise<any>;

  /** @ngInject */
  constructor(public model, public filterColumn, public filterState, public dashboardSrv, public datasourceSrv, public templateSrv, public timeSrv) {
    this.initialized = false;
    this.inFlight = Promise.resolve();

    assignModelProperties(this, model, this.defaults);
    this.updateOptionsFromMetricFindQuery.bind(this);
  }

  getSaveModel() {
    // copy back model properties to model
    assignModelProperties(this.model, this, this.defaults);

    // remove options
    if (this.refresh !== 0) {
      this.model.options = [];
    }

    return this.model;
  }

  setValue(option) {
    if (!option && !this.initialized) {
      console.debug('QueryVariable.setValue(): not yet initialized', option);
      return;
    }
    console.debug('QueryVariable.setValue()', option);
    return setOptionAsCurrent(this, option);
  }

  setValueFromUrl(urlValue) {
    return setOptionFromUrl(this, urlValue);
  }

  getValueForUrl() {
    if (this.current.text === 'All') {
      return 'All';
    }
    return this.current.value;
  }

  updateOptions(searchFilter) {
    this.inFlight = this.inFlight.finally(() => {
      console.debug('QueryVariable.updateOptions(' + searchFilter + ')', this);
      return this.datasourceSrv
        .get(this.datasource ? this.datasource : '')
        .then((ds) => this.updateOptionsFromMetricFindQuery(ds, searchFilter))
        .then(this.updateTags.bind(this))
        .then(validateVariableSelectionState(this)).catch((err) => {
          console.debug('QueryVariable.updateOptions(): err=', err);
          return Promise.reject(err);
        });
    });
    this.inFlight.finally(() => {
      console.debug('QueryVariable.updateOptions(): complete', this.options);
      this.initialized = true;
    });
    return this.inFlight;
  }

  updateTags(datasource) {
    console.debug('QueryVariable.updateTags()', datasource);
    if (this.useTags) {
      return this.metricFindQuery(datasource, this.tagsQuery).then((results) => {
        this.tags = [];
        for (let i = 0; i < results.length; i++) {
          this.tags.push(results[i].text);
        }
        return datasource;
      });
    } else {
      this.tags = [];
    }

    return datasource;
  }

  getValuesForTag(tagKey) {
    return this.datasourceSrv.get(this.datasource ? this.datasource : '').then((datasource) => {
      const query = this.tagValuesQuery.replace('$tag', tagKey);
      return this.metricFindQuery(datasource, query).then((results) => {
        return _.map(results, value => {
          return value.text;
        });
      });
    });
  }

  updateOptionsFromMetricFindQuery(datasource, searchFilter) {
    console.debug('QueryVariable.updateOptionsFromMetricFindQuery()', datasource, searchFilter);
    return this.metricFindQuery(datasource, this.query, searchFilter).then((results) => {
      console.debug('QueryVariable.updateOptionsFromMetricFindQuery(): results=', results);
      this.options = this.metricNamesToVariableValues(results);
      if (this.includeAll) {
        this.addAllOption();
      }
      console.debug('QueryVariable.updateOptionsFromMetricFindQuery() options=', this.options);
      if (!this.options.length) {
        this.options.push(getNoneOption());
      }
      return datasource;
    }).catch((err) => {
      console.debug('QueryVariable.updateOptionsFromMetricFindQuery(): err=', err);
      return Promise.reject(err);
    });
  }

  metricFindQuery(datasource: any, query: any, searchFilter?: any) {
    console.debug('QueryVariable.metricFindQuery():', datasource, query, searchFilter);
    const options = { range: undefined, variable: this, searchFilter: searchFilter };

    if (this.refresh === 2) {
      options.range = this.timeSrv.timeRange();
    }

    return datasource.metricFindQuery(query, options).catch((err) => {
      console.debug('QueryVariable.metricFindQuery(): err=', err);
      return Promise.reject(err);
    });
  }

  addAllOption() {
    this.options.unshift({ text: 'All', value: '$__all', selected: false });
  }

  metricNamesToVariableValues(metricNames) {
    let regex, options, i, matches;
    options = [];

    console.debug('QueryVariable.metricNamesToVariableValues()', metricNames);
    if (this.regex) {
      regex = stringToJsRegex(this.templateSrv.replace(this.regex, {}, 'regex'));
    }
    for (i = 0; i < metricNames.length; i++) {
      const item = metricNames[i];
      let text = item.text === undefined || item.text === null ? item.value : item.text;
      if (item.label && text === undefined || text === null) {
        text = item.label;
      }

      let value = item.value === undefined || item.value === null ? text : item.value;
      if (item.id && (value === undefined || value === null)) {
        value = item.id;
      }

      if (_.isNumber(value)) {
        value = value.toString();
      }

      if (_.isNumber(text)) {
        text = text.toString();
      }

      if (regex) {
        matches = regex.exec(value);
        if (!matches) {
          continue;
        }
        if (matches.length > 1) {
          value = matches[1];
          text = matches[1];
        }
      }

      const opt = { text: text, value: value };
      console.debug('metric', item, opt);

      options.push(opt);
    }

    options = _.uniqBy(options, 'value');
    console.debug('QueryVariable.metricNamesToVariableValues(): options=', options);
    return this.sortVariableValues(options, this.sort);
  }

  sortVariableValues(options, sortOrder) {
    if (sortOrder === 0) {
      return options;
    }

    const sortType = Math.ceil(sortOrder / 2);
    const reverseSort = sortOrder % 2 === 0;

    if (sortType === 1) {
      options = _.sortBy(options, 'text');
    } else if (sortType === 2) {
      options = _.sortBy(options, opt => {
        const matches = opt.text.match(/.*?(\d+).*/);
        if (!matches || matches.length < 2) {
          return -1;
        } else {
          return parseInt(matches[1], 10);
        }
      });
    } else if (sortType === 3) {
      options = _.sortBy(options, opt => {
        return _.toLower(opt.text);
      });
    }

    if (reverseSort) {
      options = options.reverse();
    }

    return options;
  }

  dependsOn(variable) {
    return containsVariable(this.query, this.datasource, this.regex, variable.name);
  }

}
