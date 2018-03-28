import _ from 'lodash';
import {Model} from '../../opennms';
import {CustomAction} from '../../lib/custom_action';

export class ActionMgr {
  constructor(ctrl, rows, appConfig) {
    this.ctrl = ctrl;
    this.rows = rows;
    this.options = [];
    this.appConfig = appConfig;
    this.buildContextMenu();
  }

  getContextMenu() {
    return this.options;
  }

  buildContextMenu() {
    // No selection, no context menu
    if (this.rows.length < 1) {
      return;
    }

    let self = this;
    if (this.rows.length === 1) {
      // Only show the option for the details modal when a single row is selected
      this.addOptionToContextMenu('Details', 'Details', this.rows,
        (row) => self.ctrl.alarmDetails(row.source, row.alarmId));
    }

    // We should only ack alarms that are not already acked
    let acknowledgeableRows = _.filter(this.rows, row => row.alarm.ackTime === void 0);
    this.addOptionToContextMenu('General', 'Acknowledge', acknowledgeableRows,
        (row) => self.ctrl.acknowledgeAlarm(row.source, row.alarmId));

    // We should only nack alarms that ARE already acked
    let unacknowledgeableRows = _.filter(this.rows, row => row.alarm.ackTime);
    this.addOptionToContextMenu('General', 'Unacknowledge', unacknowledgeableRows,
      (row) => self.ctrl.unacknowledgeAlarm(row.source, row.alarmId));

    // We should only escalate alarms that have a severity < CRITICAL
    let escalatableRows = _.filter(this.rows, row => {
      let severity = row.alarm.severity;
      return severity.index >= Model.Severities.CLEARED.index && severity.index < Model.Severities.CRITICAL.index;
    });
    this.addOptionToContextMenu('General', 'Escalate', escalatableRows,
      (row) => self.ctrl.escalateAlarm(row.source, row.alarmId));

    // We should only clear alarms that have a severity > CLEARED
    let cleareableRows = _.filter(this.rows, row => {
      let severity = row.alarm.severity;
      return severity.index > Model.Severities.CLEARED.index;
    });
    this.addOptionToContextMenu('General', 'Clear', cleareableRows,
      (row) => self.ctrl.clearAlarm(row.source, row.alarmId));

    // We should only create tickets for alarms that don't already have a ticket state, or where a previous create failed
    let createTicketRows = _.filter(this.rows, row => {
      return row.ticketerConfig && row.ticketerConfig.enabled
        && (!row.alarm.troubleTicketState || row.alarm.troubleTicketState === Model.TroubleTicketStates.CREATE_FAILED);
    });
    this.addOptionToContextMenu('Ticketing', 'Create Ticket', createTicketRows,
      (row) => self.ctrl.createTicketForAlarm(row.source, row.alarmId));

    // We should only update tickets for alarms that have some ticket state
    let updateTicketRows = _.filter(this.rows, row => {
      return row.ticketerConfig && row.ticketerConfig.enabled
        && (row.alarm.troubleTicketState && row.alarm.troubleTicket);
    });
    this.addOptionToContextMenu('Ticketing', 'Update Ticket', updateTicketRows,
      (row) => self.ctrl.updateTicketForAlarm(row.source, row.alarmId));

    // We should only close tickets for alarms that an open ticket, or where a previous close failed
    let closeTicketRows = _.filter(this.rows, row => {
      return row.ticketerConfig && row.ticketerConfig.enabled
        && row.alarm.troubleTicketState && (alarm.troubleTicketState === Model.TroubleTicketStates.OPEN || row.alarm.troubleTicketState === Model.TroubleTicketStates.CLOSE_FAILED);
    });
    this.addOptionToContextMenu('Ticketing', 'Close Ticket', closeTicketRows,
      (row) => self.ctrl.closeTicketForAlarm(row.source, row.alarmId));

    if (self.rows.length === 1 && self.appConfig && self.appConfig.actions && self.appConfig.actions.length > 0) {
      for (let action of self.appConfig.actions) {
        if (!action.label || !action.url || action.label.trim().length === 0 || action.url.trim().length === 0) {
          console.warn('invalid label or URL:',action);
          continue;
        }
        const a = new CustomAction(action);
        const row = self.rows[0];
        if (row && row.alarm && a.validate(row.alarm)) {
          self.addOptionToContextMenu('Actions', action.label, self.rows, (row) => {
            a.open(row.alarm);
          });
        }
      }
    }
  }

  getSuffix(actionableRows) {
    if (this.rows.length <= 1) {
      // Only show a suffix when multiple rows are selected
      return '';
    }
    return ' (' + actionableRows.length + ')';
  }

  addOptionToContextMenu(group, text, rows, action) {
    if (rows.length < 1) {
      // No rows in selection, skip the option
      return;
    }

    if (this.options.length > 0 && this.lastGroup !== group) {
      // Prepend a separator when we first encounter a new group
      this.options.push(null);
    }
    this.lastGroup = group;

    // Add the option
    this.options.push({
      text: text + this.getSuffix(rows),
      click: () => {
        // Apply the action to each row in the selection
        _.each(rows, row => action(row));
      }
    });
  }

}
