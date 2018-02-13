import _ from 'lodash';
import {Model} from '../opennms';

const failed = Symbol('failed');
const escapeRE = /[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g;

function makeVariableRE(variableName) {
  const reString = '\\$(' + variableName.replace(escapeRE, '\\$&') + ')\\b(\\[(.*?)\\])?';
  return new RegExp(reString, 'g');
}

export function getMatch(value, variableName) {
  if (_.isNull(value) || _.isEmpty(value)) {
    return false;
  }

  const match = makeVariableRE(variableName).exec(value);
  if (match) {
    const parsed = parseInt(match[3], 10);
    const ret = {
      token: match[0],
      variable: match[1]
    };
    if (Number.isInteger(parsed)) {
      ret.index = parsed;
      return ret;
    } else if (match[3] !== undefined && match[3].length > 0) {
      ret.index = match[3];
      return ret;
    } else if (match[3] === undefined || match[3] === '') {
      return ret;
    }
  }
  return null;
}

function getValue(v) {
  let ret = failed;
  if (v) {
    ret = v;
    if (v.urlValue) {
      // implements IHasUrlValue in OpenNMS.js
      ret = v.urlValue;
    } else {
      // fallback, try to guess at the proper string type
      if (v.toString) {
        ret = v.toString();
      }
      if (v.label) {
        ret = v.label;
      }
    }
  }
  return ret;
}

function getValueForMatch(match, key, model) {
  let ret = undefined;
  if (match) {
    if (match.index) {
      const v = model[key];
      if (v) {
        let indexed = v[match.index];
        if (Array.isArray(v)
          && v.length > 0
          && v[0]
          && v[0].name && v[0].type && v[0].valueString // can't do TypeScript instanceOf() at runtime
          && typeof match.index === 'string') {
          // special case, handle named event parameters
          const parm = v.filter((p) => {
            return p.name === match.index;
          })[0];
          ret = getValue(parm);
        } else {
          ret = getValue(indexed);
        }
      } else {
        console.warn('Indexed match ($' + key + '[' + match.index + ']) does not exist.');
        ret = failed;
      }
    } else {
      ret = getValue(model[key]);
    }
  }
  return ret;
}

export function replace(text, match, value) {
  const escaped = new RegExp(match.replace(escapeRE, '\\$&'), 'g');
  return text.replace(escaped, value);
}

export class CustomAction {
  constructor(data, url) {
    if (url) {
      this.label = data;
      this.url = url;
    } else {
      this.url = data.url;
      this.label = data.label;
    }
    if (!this.label || !this.url) {
      throw new Error('label and url are required!');
    }
    Object.freeze(this);
  }

  interpolate(model) {
    let interpolated = this.url;
    if (model) {
      for (const key of Object.keys(model)) {
        const match = getMatch(interpolated, key);
        if (match) {
          let value = getValueForMatch(match, key, model);
          if (value === failed) {
            value = '';
          }

          value = encodeURIComponent(value);
          interpolated = replace(interpolated, match.token, value);
        }
      }
    }
    return interpolated;
  }

  validate(model) {
    const interpolated = this.url;
    let passed = true;
    if (model) {
      for (const key of Object.keys(model)) {
        const match = getMatch(interpolated, key);
        const value = getValueForMatch(match, key, model);
        if (value === failed) {
          passed = false;
        }
      }
    }
    return passed;
  }

  open(model) {
    const interpolated = this.interpolate(model);
    console.log('opening: ' + interpolated);
    window.open(interpolated, '_blank');
  }
}