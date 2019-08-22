import {FilterCtrl} from './ctrl';
import {loadPluginCss} from 'app/plugins/sdk';

import './value_select_dropdown';

loadPluginCss({
    dark: 'plugins/opennms-helm-app/panels/filter-panel/css/filter.dark.css',
    light: 'plugins/opennms-helm-app/panels/filter-panel/css/filter.light.css'
});

export {
    FilterCtrl as PanelCtrl
};
