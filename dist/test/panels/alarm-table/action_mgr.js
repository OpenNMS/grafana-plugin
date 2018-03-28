'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.ActionMgr = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../opennms');

var _custom_action = require('../../lib/custom_action');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ActionMgr = exports.ActionMgr = function () {
  function ActionMgr(ctrl, rows, appConfig) {
    _classCallCheck(this, ActionMgr);

    this.ctrl = ctrl;
    this.rows = rows;
    this.options = [];
    this.appConfig = appConfig;
    this.buildContextMenu();
  }

  _createClass(ActionMgr, [{
    key: 'getContextMenu',
    value: function getContextMenu() {
      return this.options;
    }
  }, {
    key: 'buildContextMenu',
    value: function buildContextMenu() {
      // No selection, no context menu
      if (this.rows.length < 1) {
        return;
      }

      var self = this;
      if (this.rows.length === 1) {
        // Only show the option for the details modal when a single row is selected
        this.addOptionToContextMenu('Details', 'Details', this.rows, function (row) {
          return self.ctrl.alarmDetails(row.source, row.alarmId);
        });
      }

      // We should only ack alarms that are not already acked
      var acknowledgeableRows = _lodash2.default.filter(this.rows, function (row) {
        return row.alarm.ackTime === void 0;
      });
      this.addOptionToContextMenu('General', 'Acknowledge', acknowledgeableRows, function (row) {
        return self.ctrl.acknowledgeAlarm(row.source, row.alarmId);
      });

      // We should only nack alarms that ARE already acked
      var unacknowledgeableRows = _lodash2.default.filter(this.rows, function (row) {
        return row.alarm.ackTime;
      });
      this.addOptionToContextMenu('General', 'Unacknowledge', unacknowledgeableRows, function (row) {
        return self.ctrl.unacknowledgeAlarm(row.source, row.alarmId);
      });

      // We should only escalate alarms that have a severity < CRITICAL
      var escalatableRows = _lodash2.default.filter(this.rows, function (row) {
        var severity = row.alarm.severity;
        return severity.index >= _opennms.Model.Severities.CLEARED.index && severity.index < _opennms.Model.Severities.CRITICAL.index;
      });
      this.addOptionToContextMenu('General', 'Escalate', escalatableRows, function (row) {
        return self.ctrl.escalateAlarm(row.source, row.alarmId);
      });

      // We should only clear alarms that have a severity > CLEARED
      var cleareableRows = _lodash2.default.filter(this.rows, function (row) {
        var severity = row.alarm.severity;
        return severity.index > _opennms.Model.Severities.CLEARED.index;
      });
      this.addOptionToContextMenu('General', 'Clear', cleareableRows, function (row) {
        return self.ctrl.clearAlarm(row.source, row.alarmId);
      });

      // We should only create tickets for alarms that don't already have a ticket state, or where a previous create failed
      var createTicketRows = _lodash2.default.filter(this.rows, function (row) {
        return row.ticketerConfig && row.ticketerConfig.enabled && (!row.alarm.troubleTicketState || row.alarm.troubleTicketState === _opennms.Model.TroubleTicketStates.CREATE_FAILED);
      });
      this.addOptionToContextMenu('Ticketing', 'Create Ticket', createTicketRows, function (row) {
        return self.ctrl.createTicketForAlarm(row.source, row.alarmId);
      });

      // We should only update tickets for alarms that have some ticket state
      var updateTicketRows = _lodash2.default.filter(this.rows, function (row) {
        return row.ticketerConfig && row.ticketerConfig.enabled && row.alarm.troubleTicketState && row.alarm.troubleTicket;
      });
      this.addOptionToContextMenu('Ticketing', 'Update Ticket', updateTicketRows, function (row) {
        return self.ctrl.updateTicketForAlarm(row.source, row.alarmId);
      });

      // We should only close tickets for alarms that an open ticket, or where a previous close failed
      var closeTicketRows = _lodash2.default.filter(this.rows, function (row) {
        return row.ticketerConfig && row.ticketerConfig.enabled && row.alarm.troubleTicketState && (alarm.troubleTicketState === _opennms.Model.TroubleTicketStates.OPEN || row.alarm.troubleTicketState === _opennms.Model.TroubleTicketStates.CLOSE_FAILED);
      });
      this.addOptionToContextMenu('Ticketing', 'Close Ticket', closeTicketRows, function (row) {
        return self.ctrl.closeTicketForAlarm(row.source, row.alarmId);
      });

      if (self.rows.length === 1 && self.appConfig && self.appConfig.actions && self.appConfig.actions.length > 0) {
        var _iteratorNormalCompletion = true;
        var _didIteratorError = false;
        var _iteratorError = undefined;

        try {
          var _loop = function _loop() {
            var action = _step.value;

            if (!action.label || !action.url || action.label.trim().length === 0 || action.url.trim().length === 0) {
              console.warn('invalid label or URL:', action);
              return 'continue';
            }
            var a = new _custom_action.CustomAction(action);
            var row = self.rows[0];
            if (row && row.alarm && a.validate(row.alarm)) {
              self.addOptionToContextMenu('Actions', action.label, self.rows, function (row) {
                a.open(row.alarm);
              });
            }
          };

          for (var _iterator = self.appConfig.actions[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var _ret = _loop();

            if (_ret === 'continue') continue;
          }
        } catch (err) {
          _didIteratorError = true;
          _iteratorError = err;
        } finally {
          try {
            if (!_iteratorNormalCompletion && _iterator.return) {
              _iterator.return();
            }
          } finally {
            if (_didIteratorError) {
              throw _iteratorError;
            }
          }
        }
      }
    }
  }, {
    key: 'getSuffix',
    value: function getSuffix(actionableRows) {
      if (this.rows.length <= 1) {
        // Only show a suffix when multiple rows are selected
        return '';
      }
      return ' (' + actionableRows.length + ')';
    }
  }, {
    key: 'addOptionToContextMenu',
    value: function addOptionToContextMenu(group, text, rows, action) {
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
        click: function click() {
          // Apply the action to each row in the selection
          _lodash2.default.each(rows, function (row) {
            return action(row);
          });
        }
      });
    }
  }]);

  return ActionMgr;
}();
//# sourceMappingURL=action_mgr.js.map
