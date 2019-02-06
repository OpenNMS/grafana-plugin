'use strict';

System.register(['lodash', '../../opennms', '../../lib/custom_action'], function (_export, _context) {
  "use strict";

  var _, Model, CustomAction, _createClass, ActionMgr;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }, function (_opennms) {
      Model = _opennms.Model;
    }, function (_libCustom_action) {
      CustomAction = _libCustom_action.CustomAction;
    }],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('ActionMgr', ActionMgr = function () {
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
            var acknowledgeableRows = _.filter(this.rows, function (row) {
              return row.alarm.ackTime === void 0;
            });
            this.addOptionToContextMenu('General', 'Acknowledge', acknowledgeableRows, function (row) {
              return self.ctrl.acknowledgeAlarm(row.source, row.alarmId);
            });

            // We should only nack alarms that ARE already acked
            var unacknowledgeableRows = _.filter(this.rows, function (row) {
              return row.alarm.ackTime;
            });
            this.addOptionToContextMenu('General', 'Unacknowledge', unacknowledgeableRows, function (row) {
              return self.ctrl.unacknowledgeAlarm(row.source, row.alarmId);
            });

            // We should only escalate alarms that have a severity < CRITICAL
            var escalatableRows = _.filter(this.rows, function (row) {
              var severity = row.alarm.severity;
              return severity.index >= Model.Severities.CLEARED.index && severity.index < Model.Severities.CRITICAL.index;
            });
            this.addOptionToContextMenu('General', 'Escalate', escalatableRows, function (row) {
              return self.ctrl.escalateAlarm(row.source, row.alarmId);
            });

            // We should only clear alarms that have a severity > CLEARED
            var cleareableRows = _.filter(this.rows, function (row) {
              var severity = row.alarm.severity;
              return severity.index > Model.Severities.CLEARED.index;
            });
            this.addOptionToContextMenu('General', 'Clear', cleareableRows, function (row) {
              return self.ctrl.clearAlarm(row.source, row.alarmId);
            });

            // We should only create tickets for alarms that don't already have a ticket state, or where a previous create failed
            var createTicketRows = _.filter(this.rows, function (row) {
              return row.ticketerConfig && row.ticketerConfig.enabled && (!row.alarm.troubleTicketState || row.alarm.troubleTicketState === Model.TroubleTicketStates.CREATE_FAILED);
            });
            this.addOptionToContextMenu('Ticketing', 'Create Ticket', createTicketRows, function (row) {
              return self.ctrl.createTicketForAlarm(row.source, row.alarmId);
            });

            // We should only update tickets for alarms that have some ticket state
            var updateTicketRows = _.filter(this.rows, function (row) {
              return row.ticketerConfig && row.ticketerConfig.enabled && row.alarm.troubleTicketState && row.alarm.troubleTicket;
            });
            this.addOptionToContextMenu('Ticketing', 'Update Ticket', updateTicketRows, function (row) {
              return self.ctrl.updateTicketForAlarm(row.source, row.alarmId);
            });

            // We should only close tickets for alarms that an open ticket, or where a previous close failed
            var closeTicketRows = _.filter(this.rows, function (row) {
              return row.ticketerConfig && row.ticketerConfig.enabled && row.alarm.troubleTicketState && (row.alarm.troubleTicketState === Model.TroubleTicketStates.OPEN || row.alarm.troubleTicketState === Model.TroubleTicketStates.CLOSE_FAILED);
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
                  var a = new CustomAction(action);
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
                _.each(rows, function (row) {
                  return action(row);
                });
              }
            });
          }
        }]);

        return ActionMgr;
      }());

      _export('ActionMgr', ActionMgr);
    }
  };
});
//# sourceMappingURL=action_mgr.js.map
