import { HelmHistogramCtrl } from './ctrl';
import { loadPluginCss } from '@grafana/runtime';

loadPluginCss({
  dark: 'plugins/opennms-helm-app/panels/flow-histogram/css/legend.dark.css',
  light: 'plugins/opennms-helm-app/panels/flow-histogram/css/legend.light.css',
});

export { HelmHistogramCtrl, HelmHistogramCtrl as PanelCtrl };
