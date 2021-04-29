import _ from 'lodash';
import angular from 'angular';

let index = [] as any[];
let categories = {
  Combine: [] as any[],
  Filter: [] as any[],
  Transform: [] as any[]
};

function addFuncDef(funcDef: any) {
  funcDef.params = funcDef.params || [];
  funcDef.defaultParams = funcDef.defaultParams || [];

  if (funcDef.category) {
    funcDef.category.push(funcDef);
  }
  index[funcDef.name] = funcDef;
  index[funcDef.shortName || funcDef.name] = funcDef;
}

export const Cardinality = Object.freeze({
  SINGLE: "single",
  MULTIPLE: "multiple"
});

// Combine

addFuncDef({
  name: 'topN',
  category: categories.Combine,
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['withApplication', 'withHost', 'withConversation'],
  params: [{name: "n", type: "int",}],
  defaultParams: [10]
});

addFuncDef({
  name: 'includeOther',
  cardinality: Cardinality.SINGLE,
  category: categories.Combine
});

// Filter

addFuncDef({
  name: 'withExporterNode',
  category: categories.Filter,
  cardinality: Cardinality.SINGLE,
  params: [{name: "nodeCriteria", type: "string"}]
});

addFuncDef({
  name: 'withIfIndex',
  category: categories.Filter,
  cardinality: Cardinality.SINGLE,
  params: [{name: "ifIndex", type: "int"}]
});


addFuncDef({
  name: 'withApplication',
  category: categories.Filter,
  mutuallyExcludes: ['topN'],
  appliesToSegments: ['applications'],
  params: [{
    name: "application",
    type: "string",
    options: (input, ctx) => {
      return ctx.client.getApplications(input, ctx.getStartTime(), ctx.getEndTime(), ctx.getNodeCriteria(),
          ctx.getInterfaceId());
    }
  }]
});

addFuncDef({
  name: 'withHost',
  category: categories.Filter,
  mutuallyExcludes: ['topN'],
  appliesToSegments: ['hosts'],
  params: [{
    name: "host",
    type: "string",
    options: (input, ctx) => {
      return ctx.client.getHosts(input, ctx.getStartTime(), ctx.getEndTime(), ctx.getNodeCriteria(),
          ctx.getInterfaceId());
    }
  }]
});

addFuncDef({
  name: 'withConversation',
  category: categories.Filter,
  mutuallyExcludes: ['topN'],
  appliesToSegments: ['conversations'],
  params: [{
    name: "conversation",
    type: "string"
  }]
});

// Transform

addFuncDef({
  name: 'perSecond',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  category: categories.Transform
});

addFuncDef({
  name: 'toBits',
  cardinality: Cardinality.SINGLE,
  category: categories.Transform
});

addFuncDef({
  name: 'negativeEgress',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  category: categories.Transform
});

addFuncDef({
  name: 'negativeIngress',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  category: categories.Transform
});

addFuncDef({
  name: 'asTableSummary',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['perSecond', 'negativeEgress', 'negativeIngress', 'combineIngressEgress', 'onlyIngress',
    'onlyEgress', 'withGroupByInterval'],
  category: categories.Transform
});

addFuncDef({
  name: 'combineIngressEgress',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  category: categories.Transform
});

addFuncDef({
  name: 'onlyIngress',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  category: categories.Transform
});

addFuncDef({
  name: 'onlyEgress',
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  category: categories.Transform
});

addFuncDef({
  name: 'withGroupByInterval',
  category: categories.Transform,
  cardinality: Cardinality.SINGLE,
  mutuallyExcludes: ['asTableSummary'],
  params: [{
    name: "interval",
    type: "string"
  }]
});

_.each(categories, function (funcList, catName) {
  categories[catName] = _.sortBy(funcList, 'name');
});

export class FuncInstance {
  added = false;
  def: any;
  params: any[];
  text?: string;

  constructor(funcDef: any, options?: any) {
    this.def = funcDef;
    this.params = [];
  
    if (options && options.withDefaultParams) {
      this.params = funcDef.defaultParams.slice(0);
    }
  
    this.updateText();
  }

  render() {
    return {
      name: this.def.name,
      parameters: _.map(this.params, (value, index) => {
        let paramType = this.def.params[index].type;
        if (paramType === 'int' || paramType === 'value_or_series' || paramType === 'boolean') {
          return value;
        }
        else if (paramType === 'int_or_interval' && angular.isNumber(value)) {
          return value;
        }
        return value;
      })
    };
  }

  _hasMultipleParamsInString(strValue: string, index: number) {
    if (strValue.indexOf(',') === -1) {
      return false;
    }
  
    return this.def.params[index + 1] && this.def.params[index + 1].optional;
  };
  
  updateParam(strValue: string, index: number) {
    // handle optional parameters
    // if string contains ',' and next param is optional, split and update both
    if (this._hasMultipleParamsInString(strValue, index)) {
      _.each(strValue.split(','), (partVal: string, idx: number) => {
        this.updateParam(partVal.trim(), index + idx);
      });
      return;
    }
  
    if (strValue === '' && this.def.params[index].optional) {
      this.params.splice(index, 1);
    }
    else {
      this.params[index] = strValue;
    }
  
    this.updateText();
  };
  
  updateText() {
    if (this.params.length === 0) {
      this.text = this.def.name + '()';
      return;
    }
  
    let text = this.def.name + '(';
    text += this.params.join(', ');
    text += ')';
    this.text = text;
  };
}

export class Gfuncs {
  def: any;

  static createFuncInstance(funcDef: any, options?: any) {
    if (_.isString(funcDef)) {
      if (!index[funcDef]) {
        throw {message: 'Method not found ' + name};
      }
      funcDef = index[funcDef];
    }
    return new FuncInstance(funcDef, options);
  }

  static getFuncDef(name) {
    return index[name];
  }

  static getCategories() {
    let filteredCategories = {};
    _.each(categories, function (functions, category) {
      if (functions.length) {
        filteredCategories[category] = functions;
      }
    });

    return filteredCategories;
  }
}
