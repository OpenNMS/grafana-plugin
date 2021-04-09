// @ts-ignore
import coreModule from 'app/core/core_module';
import { loadPluginCss } from '@grafana/runtime';

import { AlarmTableCtrl } from './alarmTableCtrl';
import { alarmDetailsAsDirective } from './alarm_details';
import { memoEditorAsDirective } from './memo_editor';
import { contextMenuAsDirective } from './context_menu';

loadPluginCss({
  dark: 'plugins/opennms-helm-app/panels/alarm-table/css/table.dark.css',
  light: 'plugins/opennms-helm-app/panels/alarm-table/css/table.light.css',
});

export { AlarmTableCtrl, AlarmTableCtrl as PanelCtrl };

coreModule.directive('alarmDetailsAsModal', alarmDetailsAsDirective);
coreModule.directive('memoEditor', memoEditorAsDirective);
coreModule.directive('contextMenu', contextMenuAsDirective());

coreModule.directive('dynamicHeight', ($window: Window) => {
  // Used to dynamically size the alarm details modal window
  return {
    link: (scope: any, element: JQuery<HTMLElement> /*, attrs */) => {
      const doResize = () => {
        element.css('max-height', $window.innerHeight * 0.8 + 'px');
      };

      doResize();
      element.on('$destroy', () => {
        $window.removeEventListener('resize', doResize);
      });
      $window.addEventListener('resize', doResize);
    },
  };
});
