import { OpenNMSHelmAppConfigCtrl } from './components/config';
import { loadPluginCss } from '@grafana/runtime';

loadPluginCss({
  dark: 'plugins/opennms-helm-app/styles/dark.css',
  light: 'plugins/opennms-helm-app/styles/light.css',
});

export { OpenNMSHelmAppConfigCtrl as ConfigCtrl };
