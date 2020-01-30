const ui = require('@grafana/ui');
const data = require('@grafana/data');

// grafana has been moving methods around between @grafana/ui and @grafana/data; search both for an implementation at runtime

/**
 * Load an item from either @grafana/ui or @grafana/data.
 *
 * This provides a shim layer for multiple Grafana versions, since they have been moving
 * various methods around between minor versions.
 *
 * @param {string} name - the name of the property in either @grafana/ui or @grafana/data
 */
export const grafanaResource = (name) => {
  if (data[name]) {
    console.debug('found ' + name + ' in @grafana/data', data[name]);
    return data[name];
  }
  if (ui[name]) {
    console.debug('found ' + name + ' in @grafana/ui', ui[name]);
    return ui[name];
  }
  console.warn('Failed to find ' + name + ' in @grafana/ui or @grafana/data.');
};


/**
 * Load an item from the provided resource at runtime, using `require()`.
 *
 * @param {*} resource - the resource to load
 * @param {*} prop - the property to return from that resource
 */
export const grafanaOptional = (resource, prop) => {
  try {
    // eslint-disable-next-line global-require
    const found = require(resource);
    return found[prop];
  } catch (err) {
    console.warn('Failed to load ' + resource + ' at runtime.');
  }
  return undefined;
}