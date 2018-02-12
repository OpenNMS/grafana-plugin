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

export function replace(text, match, value) {
  const escaped = new RegExp(match.replace(escapeRE, '\\$&'), 'g');
  //console.log('replacing ' + match + ' with ' + value + ' (re=' + escaped + ')');
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
          let value = undefined;
          if (match.index) {
            value = getValue(model[key][match.index]);
          } else {
            value = getValue(model[key]);
          }

          if (value === failed) {
            value = '';
          }
          interpolated = replace(interpolated, match.token, value);
        }
      }
    }
    return interpolated;
  }

  validate(model) {
    let passed = true;
    if (model) {
      const interpolated = this.url;
      for (const key of Object.keys(model)) {
        const match = getMatch(interpolated, key);
        if (match) {
          let value = undefined;
          if (match.index) {
            value = getValue(model[key][match.index]);
          } else {
            value = getValue(model[key]);
          }
          if (value === failed) {
            console.warn('Variable $' + key + ' was found in the model object, but no value was found.', model);
            passed = false;
          }
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