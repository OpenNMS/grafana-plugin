const ui = require('@grafana/ui');
const data = require('@grafana/data');

// grafana has been moving methods around between @grafana/ui and @grafana/data; search both for an implementation at runtime

export const grafanaResource = (name) => {
  if (data[name]) {
    console.debug('found ' + name + ' in @grafana/data', data[name]);
    return data[name];
  }
  if (ui[name]) {
    console.debug('found ' + name + ' in @grafana/ui', ui[name]);
    return ui[name];
  }
  console.error('Failed to find ' + name + ' in @grafana/ui or @grafana/data.');
};
