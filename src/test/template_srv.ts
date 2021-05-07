import _ from 'lodash';

export class TemplateSrv {
  variables: any[];
  grafanaVariables: any;
  index: any;
  regex: RegExp;

  constructor() {
    this.variables = [];
    this.grafanaVariables = {};
    this.index = {};
    /*
     * This regex matches 3 types of variable reference with an optional format specifier
     * \$(\w+)                          $var1
     * \[\[([\s\S]+?)(?::(\w+))?\]\]    [[var2]] or [[var2:fmt2]]
     * \${(\w+)(?::(\w+))?}             ${var3} or ${var3:fmt3}
    */
    this.regex = /\$(\w+)|\[\[([\s\S]+?)(?::(\w+))?\]\]|\${(\w+)(?::(\w+))?}/g;
  }

  init(variables) {
    this.variables = variables;
    this.updateTemplateData();
  }

  updateTemplateData() {
    this.index = {};

    for (var i = 0; i < this.variables.length; i++) {
      var variable = this.variables[i];

      if (!variable.current || (!variable.current.isNone && !variable.current.value)) {
        continue;
      }

      this.index[variable.name] = variable;
    }
  }

  formatValue(value, format, variable) {
    // for some scopedVars there is no variable
    variable = variable || {};

    if (typeof format === 'function') {
      return format(value, variable, this.formatValue);
    }

    switch (format) {
      case 'regex': {
        throw new Error('not supported');
      }
      case 'lucene': {
        throw new Error('not supported');
      }
      case 'pipe': {
        if (typeof value === 'string') {
          return value;
        }
        return value.join('|');
      }
      case 'distributed': {
        throw new Error('not supported');
      }
      default: {
        if (_.isArray(value)) {
          return '{' + value.join(',') + '}';
        }
        return value;
      }
    }
  }

  getVariableName(expression) {
    this.regex.lastIndex = 0;
    var match = this.regex.exec(expression);
    if (!match) {
      return null;
    }
    return match[1] || match[2];
  }

  getAllValue(variable) {
    if (variable.allValue) {
      return variable.allValue;
    }
    var values = [] as any[];
    for (var i = 1; i < variable.options.length; i++) {
      values.push(variable.options[i].value);
    }
    return values;
  }

  replace(target, scopedVars, format) {
    if (!target) {
      return target;
    }

    var variable, systemValue, value;
    this.regex.lastIndex = 0;

    return target.replace(this.regex, (match, var1, var2, fmt2, var3, fmt3) => {
      variable = this.index[var1 || var2 || var3];
      format = fmt2 || fmt3 || format;
      if (scopedVars) {
        value = scopedVars[var1 || var2 || var3];
        if (value) {
          return this.formatValue(value.value, format, variable);
        }
      }
    
      if (!variable) {
        return match;
      }
    
      systemValue = this.grafanaVariables[variable.current.value];
      if (systemValue) {
        return this.formatValue(systemValue, format, variable);
      }
    
      value = variable.current.value;
      if (this.isAllValue(value)) {
        value = this.getAllValue(variable);
        // skip formating of custom all values
        if (variable.allValue) {
          return value;
        }
      }
    
      var res = this.formatValue(value, format, variable);
      return res;
    });
  }

  isAllValue(value) {
    return value === '$__all' || (Array.isArray(value) && value[0] === '$__all');
  }

}

export default new TemplateSrv();
