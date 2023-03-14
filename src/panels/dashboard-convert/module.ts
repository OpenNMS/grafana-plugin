import { PanelPlugin } from '@grafana/data';
import { DashboardConvertPanelControl } from './DashboardConvertPanelControl'

export const plugin = new PanelPlugin(DashboardConvertPanelControl).setPanelOptions((builder) => {
    // no custom editor for now
});
