import _ from 'lodash';

import { loadPluginCss } from '@grafana/runtime';

let cssInitialized = false;
export function initializeCss() {
  if (!cssInitialized) {
    cssInitialized = true;
    loadPluginCss({
      dark: 'plugins/opennms-helm-app/styles/dark.css',
      light: 'plugins/opennms-helm-app/styles/light.css',
    });
  }
}

export function assignModelProperties(target, source, defaults = {}) {
  for (const key in defaults) {
    if (!defaults.hasOwnProperty(key)) {
      continue;
    }

    target[key] = source[key] === undefined ? defaults[key] : source[key];
  }
}

export function selectOptionsForCurrentValue(variable) {
  let i, y, value, option;
  const selected = [] as any[];

  for (i = 0; i < variable.options.length; i++) {
    option = variable.options[i];
    option.selected = false;
    if (_.isArray(variable.current.value)) {
      for (y = 0; y < variable.current.value.length; y++) {
        value = variable.current.value[y];
        if (option.value === value) {
          option.selected = true;
          selected.push(option);
        }
      }
    } else if (option.value === variable.current.value) {
      option.selected = true;
      selected.push(option);
    }
  }

  return selected;
}

export function setOptionAsCurrent(variable, option) {
  variable.current = _.cloneDeep(option || {});

  if (_.isArray(variable.current.text) && variable.current.text.length > 0) {
    variable.current.text = variable.current.text.join(' + ');
  } else if (_.isArray(variable.current.value) && variable.current.value[0] !== '$__all') {
    variable.current.text = variable.current.value.join(' + ');
  }

  selectOptionsForCurrentValue(variable);
  return variableUpdated(variable);
}

export function variableUpdated(variable, emitChangeEvents = false) {
  const dashboardSrv = variable.dashboardSrv;
  const templateSrv = variable.templateSrv;

  // if there is a variable lock ignore cascading update because we are in a boot up scenario
  if (variable.initLock) {
    return Promise.resolve();
  }

  const getVariables = templateSrv.getVariables.bind(templateSrv) || dashboardSrv.dashboard.getVariables.bind(dashboardSrv.dashboard);
  const variables = getVariables();
  let promises = [] as Array<Promise<any>>;

  // in theory we should create an efficient sub-list of variables to update, but for now just do them all YOLO
  variables.forEach(v => {
    // variables don't always seem to have updateOptions method, so check here
    if (v.updateOptions) {
      promises.push(v.updateOptions());
    }
  });

  templateSrv.setGlobalVariable(variable.id, variable.current);

  return Promise.all(promises).then(() => {
    if (emitChangeEvents) {
      dashboardSrv.dashboard.templateVariableValueUpdated();
      dashboardSrv.dashboard.startRefresh();
    }
  });
}


export function setOptionFromUrl(variable: any, urlValue: any) {
  let promise = Promise.resolve();

  if (variable.refresh) {
    promise = variable.updateOptions();
  }

  return promise.then(() => {
    // Simple case. Value in url matches existing options text or value.
    let option = _.find(variable.options, op => {
      return op.text === urlValue || op.value === urlValue;
    });

    // No luck either it is array or value does not exist in the variables options.
    if (!option) {
      let defaultText = urlValue;
      const defaultValue = urlValue;

      if (_.isArray(urlValue)) {
        // Multiple values in the url. We construct text as a list of texts from all matched options.
        defaultText = urlValue.reduce((acc, item) => {
          const t = _.find(variable.options, { value: item });
          if (t) {
            acc.push(t.text);
          } else {
            acc.push(item);
          }

          return acc;
        }, []);
      }

      // It is possible that we did not match the value to any existing option. In that case the url value will be
      // used anyway for both text and value.
      option = { text: defaultText, value: defaultValue };
    }

    if (variable.multi) {
      // In case variable is multiple choice, we cast to array to preserve the same behaviour as when selecting
      // the option directly, which will return even single value in an array.
      option = { text: _.castArray(option.text), value: _.castArray(option.value) };
    }

    return variable.setValue(option);
  });
}

export function validateVariableSelectionState(variable: any, defaultValue?: any) {
  if (!variable.current) {
    variable.current = {};
  }

  if (_.isArray(variable.current.value)) {
    let selected = selectOptionsForCurrentValue(variable);

    // if none pick first
    if (selected.length === 0) {
      let firstOption = variable.options[0];
      if (firstOption) {
        return variable.setValue(firstOption);
      }
      return Promise.resolve();
    } else {
      let val = {
        value: _.map(selected, val => {
          return val.value;
        }),
        text: _.map(selected, val => {
          return val.text;
        }),
      };
      return variable.setValue(val);
    }
  } else {
    let option = undefined;

    // 1. find the current value
    option = _.find(variable.options, {
      text: variable.current.text,
    });
    if (option) {
      return variable.setValue(option);
    }

    // 2. find the default value
    if (defaultValue) {
      option = _.find(variable.options, {
        text: defaultValue,
      });
      if (option) {
        return variable.setValue(option);
      }
    }

    // 3. use the first value
    if (variable.options) {
      return variable.setValue(variable.options[0]);
    }

    // 4... give up
    return Promise.resolve();
  }
}

export const variableRegex = /\$(\w+)|\[\[([\s\S]+?)(?::(\w+))?\]\]|\${(\w+)(?:\.([^:^}]+))?(?::(\w+))?}/g;

export const variableRegexExec = (variableString) => {
  variableRegex.lastIndex = 0;
  return variableRegex.exec(variableString);
};

export function containsVariable(...args) {
  const variableName = args[args.length - 1];
  args[0] = _.isString(args[0]) ? args[0] : Object['values'](args[0]).join(' ');
  const variableString = args.slice(0, -1).join(' ');
  const matches = variableString.match(variableRegex);
  const isMatchingVariable =
    matches !== null
      ? matches.find(match => {
          const varMatch = variableRegexExec(match);
          return varMatch !== null && varMatch.indexOf(variableName) > -1;
        })
      : false;

  return !!isMatchingVariable;
}

/**
 * Converts an input string into a string array of selected values.
 *
 * The input string may be the value of a multi-selection template variable. In this case the input string is
 * delimited by curly braces that enclose a comma separated list of values.
 *
 * The dropUnresolved parameter determines if an empty array is returned if the input starts with '$'.
 * The dropAll parameter determines if an empty array is returned if the input is "all".
 */
export function processSelectionVariable(input: string, dropUnresolved: boolean, dropAll: boolean): string[] {
  if (input) {
    if (input.startsWith('{') && input.endsWith('}')) {
      input = input.substring(1, input.length - 1);
    }
    const args = getInputsAsArray(input);

    if (dropAll && args.some(s => s === 'all')) {
      return []
    } else if (dropUnresolved && input.startsWith('$')) {
      return [];
    } else if (dropAll && input === 'all') {
      return []
    } else {
      return args;
    }
  } else {
    return []
  }
}

export function getInputsAsArray(input: string) {
  const pattern = /(\[[^\]]*\])/g;
  //handle conversation type 
  let inputArrays = input.match(pattern);
  if (inputArrays && inputArrays.length > 0) {
    const args: string[] = [];
    inputArrays.forEach(array => {
      args.push(array);
    });
    return args;
  } else if (input.indexOf(',') >= 0) {
    return input.split(',').map(s => s.trim());
  } else {
    return [input];
  }
}

/**
 * Grafana oftens returns values that are either the raw value or else a one-element array with the value.
 * Utility function to get either.
 */
export function getValueOrFirstElement<T>(input: T | T[]): T {
  return _.isArray(input) && input.length > 0 ? input[0] : (input as T);
}

/** Checks if the given index is the first index of the given t. */
export function isFirst<T>(t: T, index: number, array: T[]) {
  return array.indexOf(t) === index;
}

/**
 * Converts multiple (possibly multi-selection) inputs into a string array of all contained selections.
 *
 * Unresolved template variables (detected by a leading '$' sign) are ignored. This allows to remove template variables
 * from a dashboard without having to adjust queries that use them.
 *
 * An empty array is returned if "all" would be included in the selections. Duplicates are removed.
 */
export function processSelectionVariables(input?: string[]): string[] {
  if (input) {
      const mapped = input.map(i => processSelectionVariable(i, true, false))
      if (mapped.some(m => m.some(s => s === 'all'))) {
          return []
      } else {
          return ([] as string[]).concat(...mapped).filter(isFirst)
      }
  } else {
      return []
  }
}


export class OpenNMSGlob {
  private static globExpressions: string[] = ['*', '|'];

  static getGlobAsRegexPattern(expr: string) {
    return _.escapeRegExp(expr).replace(/\\\*/ig, '.*').replace(/\\\|/ig, '|');
  }

  /**
   * Check if expression contains allowed glob characters
   * @param expr expression
   * @returns true if expression contains allowed glob characters ('*', '|')
   */
  static hasGlob(expr: string): boolean {
    return _.some([...expr], (char) => {
      return _.includes(OpenNMSGlob.globExpressions, char);
    });
  }
}

/**
 * Swap items in an array
 * @param thisArray 
 */
export function swap(thisArray: any[], colIndex1: number, colIndex2: number): any[] {
  const tmp = thisArray[colIndex1];
  thisArray[colIndex1] = thisArray[colIndex2];
  thisArray[colIndex2] = tmp;
  return thisArray;
}

/**
 * Swap table rows column values
 * @param colIndex1 
 * @param colIndex2 
 */
export function swapColumns(rows: any[][], colIndex1: number, colIndex2: number): any[][] {
  if (rows && rows.length > 0 && colIndex1 >= 0 && colIndex2 >= 0) {
    for (var i = 0; i < rows.length; i++) {
      if (colIndex1 >= rows[i].length || colIndex2 >= rows[i].length) {
        throw new Error('Index out of bounds');
      }
      rows[i] = swap(rows[i], colIndex1, colIndex2);
    }
  }
  return rows;
}

export function getNodeResource(nodeId: string) {
  let prefix = "";
  if (nodeId.indexOf(":") > 0) {
    prefix = "nodeSource[";
  } else {
    prefix = "node[";
  }
  return prefix + nodeId + "]";
}

export class SimpleOpenNMSRequest {
  backendSrv: any;
  timeout: number = 10000;
  url?: string;
  withCredentials: boolean = false;
  basicAuth?: string;
  searchLimit = 25;

  readonly flows = "/rest/flows";
  readonly locations = "/rest/monitoringLocations";
  readonly nodes = "/rest/nodes"
  readonly interfaces = "/api/v2/snmpinterfaces";
  readonly resources = "/rest/resources"

  constructor(backendSrv, url) {
    this.backendSrv = backendSrv;
    this.url = url;
  }

  doOpenNMSRequest(options: any): Promise<any> {

    if (this.basicAuth || this.withCredentials) {
      options.withCredentials = true;
    }
    if (this.basicAuth) {
      options.headers = options.headers || {};
      options.headers.Authorization = this.basicAuth;
    }

    options.url = this.url + options.url;
    if (this.timeout) {
      options.timeout = this.timeout;
    }

    return this.backendSrv.datasourceRequest(options);
  }

  getLocations(searchLimit = 0) {
    return this.doOpenNMSRequest({
      url: this.locations,
      method: 'GET',
      params: {
        limit: searchLimit
      }
    })
      .then(function (response) {
        if (response.data.count > response.data.totalCount) {
          console.warn("Filter matches " + response.data.totalCount + " records, but only " + response.data.count + " will be used.");
        }
        var results = [] as any[];
        _.each(response.data.location, function (location) {
          let nodeLocation = location['location-name'] ? location['location-name'].toString() : null;
          let exist = _.find(results, (o) => o.text === nodeLocation);
          if (nodeLocation && !exist) {
            results.push({ text: nodeLocation, value: nodeLocation, expandable: true });
          }
        });
        return results;
      });
  }

  async getNodesByFilter(filter: string) {
    const response = await this.doOpenNMSRequest({
      url: this.nodes,
      method: 'GET',
      params: {
        filterRule: filter,
        limit: 0
      }
    })

    if (response.data.count > response.data.totalCount) {
      console.warn("Filter matches " + response.data.totalCount + " records, but only " + response.data.count + " will be used.");
    }
    return response.data.node;
  }

  async getApplications(start: number, end: number, limit = 0) {
    const response = await this.doOpenNMSRequest({
      url: this.flows + '/applications/enumerate',
      method: 'GET',
      params: {
        'start': start,
        'end': end,
        'limit': limit <= 0 ? this.searchLimit : limit
      }
    })

    if (response.data.length === 0) {
      console.warn("No matches found");
      return response.data;
    } else {
      let results: any[] = [];
      response.data.forEach(val => {
        results.push({ text: val, value: val, expandable: true });
      });
      return results;
    }

  }

  async getHosts(start: number, end: number, pattern: string | null, limit = 0) {
    if(!pattern){
      pattern = ".*"
    }

    const response = await this.doOpenNMSRequest({
      url: this.flows + '/hosts/enumerate',
      method: 'GET',
      params: {
        'start': start,
        'end': end,
        'limit': limit <= 0 ? this.searchLimit : limit,
        'pattern': pattern
      }
    })

    if (response.data.length === 0) {
      console.warn("No matches found");
      return response.data;
    } else {
      let results: any[] = [];
      response.data.forEach(val => {
        results.push({ text: val, value: val, expandable: true });
      });
      return results;
    }
  }

  async getConversations(start: number, end: number, application: string | null = null,
    location: string | null = null, protocol: string | null = null, limit = 0) {
    if(!application){
      application = ".*";
    }
    if(!location){
      location = ".*";
    }
    if(!protocol){
      protocol = ".*";
    }
    const response = await this.doOpenNMSRequest({
      url: this.flows + '/conversations/enumerate',
      method: 'GET',
      params: {
        'start': start,
        'end': end,
        'application': application,
        'location': location,
        'protocol': protocol,
        'limit': limit <= 0 ? this.searchLimit : limit
      }
    })

    if (response.data.length === 0) {
      console.warn("No matches found");
      return response.data;
    } else {
      let results: any[] = [];
      response.data.forEach(val => {
        results.push({ text: val, value: val, expandable: true });
      });
      return results;
    }
  }

  async getNodeByIdOrFsFsId(query: string){
    const response = await this.doOpenNMSRequest({
      url: this.nodes + '/' + query.trim(),
      method: 'GET',
      params: {
        limit: 0
      }
    })

    return response.data;
  }

  async getResources(query: string) {
    return await this.doOpenNMSRequest({
      url: this.resources + encodeURIComponent(getNodeResource(query)),
      method: 'GET',
      params: {
        depth: 1
      }
    });
  }

  async getIfIndexFromSnmpResourceIfExixts(node: any, iface: any) {
    const response = await this.getResources(node);
    if (response.data.children.resource) {
      for (const resource of response.data.children.resource) {
        if (resource.id === iface || resource.label === iface || resource.name === iface) {
          const regexSnmpIface = /element\/snmpinterface\.jsp\?node=\d+&ifindex=(\d+)/;
          let ifIndexMatch = resource.link.match(regexSnmpIface);
          if (ifIndexMatch) {
            return ifIndexMatch[0];
          }
        }
      }
    }
    return iface;
  }
}
