import {HelmHistogramCtrl} from './ctrl';
import {loadPluginCss} from 'app/plugins/sdk';

loadPluginCss({
    dark: 'plugins/opennms-helm-app/panels/flow-histogram/css/legend.dark.css',
    light: 'plugins/opennms-helm-app/panels/flow-histogram/css/legend.light.css'
});

export {
    HelmHistogramCtrl as PanelCtrl
};
