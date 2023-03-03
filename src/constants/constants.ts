import { NavModelItem } from '@grafana/data';
import pluginJson from '../plugin.json';

export const EntityTypes = {
    Alarms: 'Alarms',
    Nodes: 'Nodes',
    IPInterfaces: 'IP Interfaces',
    SNMPInterfaces: 'SNMP Interfaces',
    MonitoredServices: 'Monitored Services',
    Outages: 'Outages',
}

export const ALL_SELECTION_VALUE = '$__all'

export const PLUGIN_BASE_URL = `/a/${pluginJson.id}`;

export enum ROUTES {
  Migrate = 'migrate',
  Two='two'
}

export const NAVIGATION_TITLE = 'Helm Plugin';
export const NAVIGATION_SUBTITLE = 'Some extra description...';

// Add a navigation item for each route you would like to display in the navigation bar
export const NAVIGATION: Record<string, NavModelItem> = {
  [ROUTES.Migrate]: {
    id: ROUTES.Migrate,
    text: 'Helm Migration',
    icon: 'database',
    url: `${PLUGIN_BASE_URL}/one`,
  },
  [ROUTES.Two]: {
    id: ROUTES.Two,
    text: 'Page Two',
    icon: 'key-skeleton-alt',
    url: `${PLUGIN_BASE_URL}/two`,
  },
  
};
