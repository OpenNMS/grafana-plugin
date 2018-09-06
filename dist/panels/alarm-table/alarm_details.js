'use strict';

System.register(['./renderer', '../../crypto-js/md5', '../../opennms'], function (_export, _context) {
  "use strict";

  var TableRenderer, md5, Model, _createClass, AlarmDetailsCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  /** @ngInject */
  function alarmDetailsAsDirective() {
    'use strict';

    return {
      restrict: 'E',
      templateUrl: 'public/plugins/opennms-helm-app/panels/alarm-table/alarm_details.html',
      controller: AlarmDetailsCtrl,
      bindToController: true,
      controllerAs: 'ctrl',
      scope: { dismiss: "&" }
    };
  }

  _export('alarmDetailsAsDirective', alarmDetailsAsDirective);

  return {
    setters: [function (_renderer) {
      TableRenderer = _renderer.TableRenderer;
    }, function (_cryptoJsMd) {
      md5 = _cryptoJsMd.default;
    }, function (_opennms) {
      Model = _opennms.Model;
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

      _export('AlarmDetailsCtrl', AlarmDetailsCtrl = function () {

        /** @ngInject */
        function AlarmDetailsCtrl($scope, backendSrv, contextSrv, datasourceSrv, $q) {
          _classCallCheck(this, AlarmDetailsCtrl);

          this.$scope = $scope;
          this.backendSrv = backendSrv;
          this.contextSrv = contextSrv;
          this.datasourceSrv = datasourceSrv;
          this.editFeedback = false;
          $scope.editor = { index: 0 };

          this.CORRECT_OUTLINED = "/public/plugins/opennms-helm-app/img/if_icon-ios7-checkmark-outline_211714.png";
          this.CORRECT_FILLED = "/public/plugins/opennms-helm-app/img/if_icon-ios7-checkmark_211715.png";

          this.INCORRECT_OUTLINED = "/public/plugins/opennms-helm-app/img/if_icon-close_211652.png";
          this.INCORRECT_FILLED = "/public/plugins/opennms-helm-app/img/if_icon-close-circled_211650.png";

          // Save the alarm
          $scope.alarm = $scope.$parent.alarm;
          $scope.source = $scope.$parent.source;

          // Feedback Counts
          $scope.feedbackCorrectCount = 0;
          $scope.feedbackIncorrectCount = 0;

          // Compute the icon
          var severity = $scope.alarm.severity.label.toLowerCase();
          $scope.severityIcon = TableRenderer.getIconForSeverity(severity);

          // Situation Feedback
          $scope.situationFeebackEnabled = false;
          $scope.feebackButton = this.CORRECT_OUTLINED;

          // Compute the tabs
          $scope.tabs = ['Overview', 'Memos'];
          $scope.ticketingEnabled = $scope.$parent.ticketerConfig && $scope.$parent.ticketerConfig.enabled;
          if ($scope.ticketingEnabled) {
            $scope.tabs.push('Ticketing');
          }
          // If this is a Situation, collect any correlation feedback previously submitted
          if ($scope.alarm.relatedAlarms && $scope.alarm.relatedAlarms.length > 0) {
            $scope.tabs.push('Related Alarms');
            var self = this;
            this.getDatasource().then(function (ds) {
              return ds.getSituationFeedback(self.$scope.alarm.id);
            }).then(function (feedback) {
              $scope.situationFeedback = self.initalizeFeeback();
              if (feedback && feedback.length > 0) {
                self.updateFeedback(feedback);
                $scope.hasSituationFeedback = true;
              }
              $scope.situationFeedbackButton = self.situationFeedbackButton();
              $scope.situationFeebackEnabled = true;
            }).catch(function (reason) {
              console.log("Situation Feedback not supported error: ", reason);
            });
          }

          // Raw global details link
          $scope.detailsLink = $scope.alarm.detailsPage.substring(0, $scope.alarm.detailsPage.indexOf("="));
        }

        _createClass(AlarmDetailsCtrl, [{
          key: 'detailFeedbackIncorrectButton',
          value: function detailFeedbackIncorrectButton(reductionKey) {
            var button = this.INCORRECT_OUTLINED;
            if (this.$scope.situationFeedback && this.$scope.hasSituationFeedback) {
              var _iteratorNormalCompletion = true;
              var _didIteratorError = false;
              var _iteratorError = undefined;

              try {
                for (var _iterator = this.$scope.situationFeedback[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                  var feedback = _step.value;

                  if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
                    button = this.INCORRECT_FILLED;
                    break;
                  }
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
            return button;
          }
        }, {
          key: 'detailFeedbackOkayButton',
          value: function detailFeedbackOkayButton(reductionKey) {
            var button = this.CORRECT_FILLED;
            if (this.$scope.situationFeedback) {
              var _iteratorNormalCompletion2 = true;
              var _didIteratorError2 = false;
              var _iteratorError2 = undefined;

              try {
                for (var _iterator2 = this.$scope.situationFeedback[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
                  var feedback = _step2.value;

                  if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
                    button = this.CORRECT_OUTLINED;
                    break;
                  }
                }
              } catch (err) {
                _didIteratorError2 = true;
                _iteratorError2 = err;
              } finally {
                try {
                  if (!_iteratorNormalCompletion2 && _iterator2.return) {
                    _iterator2.return();
                  }
                } finally {
                  if (_didIteratorError2) {
                    throw _iteratorError2;
                  }
                }
              }
            }
            return button;
          }
        }, {
          key: 'initalizeFeeback',
          value: function initalizeFeeback() {
            this.$scope.feedbackCorrectCount = 0;
            this.$scope.feedbackIncorrectCount = 0;
            var feedback = [];
            var _iteratorNormalCompletion3 = true;
            var _didIteratorError3 = false;
            var _iteratorError3 = undefined;

            try {
              for (var _iterator3 = this.$scope.alarm.relatedAlarms[Symbol.iterator](), _step3; !(_iteratorNormalCompletion3 = (_step3 = _iterator3.next()).done); _iteratorNormalCompletion3 = true) {
                var alarm = _step3.value;

                var alarmFeedback = new Model.OnmsSituationFeedback();
                alarmFeedback.situationKey = this.$scope.alarm.reductionKey;
                alarmFeedback.situationFingerprint = this.fingerPrint(this.$scope.alarm);
                alarmFeedback.alarmKey = alarm.reductionKey;
                alarmFeedback.feedbackType = Model.FeedbackTypes.CORRECT;
                alarmFeedback.reason = "ALL_CORRECT";
                alarmFeedback.user = this.contextSrv.user.login;
                feedback.push(alarmFeedback);
                this.$scope.feedbackCorrectCount++;
              }
            } catch (err) {
              _didIteratorError3 = true;
              _iteratorError3 = err;
            } finally {
              try {
                if (!_iteratorNormalCompletion3 && _iterator3.return) {
                  _iterator3.return();
                }
              } finally {
                if (_didIteratorError3) {
                  throw _iteratorError3;
                }
              }
            }

            return feedback;
          }
        }, {
          key: 'markIncorrect',
          value: function markIncorrect(reductionKey) {
            var _iteratorNormalCompletion4 = true;
            var _didIteratorError4 = false;
            var _iteratorError4 = undefined;

            try {
              for (var _iterator4 = this.$scope.situationFeedback[Symbol.iterator](), _step4; !(_iteratorNormalCompletion4 = (_step4 = _iterator4.next()).done); _iteratorNormalCompletion4 = true) {
                var feedback = _step4.value;

                if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.CORRECT) {
                  feedback.feedbackType = Model.FeedbackTypes.FALSE_POSITIVE;
                  this.$scope.feedbackCorrectCount--;
                  this.$scope.feedbackIncorrectCount++;
                  break;
                }
              }
            } catch (err) {
              _didIteratorError4 = true;
              _iteratorError4 = err;
            } finally {
              try {
                if (!_iteratorNormalCompletion4 && _iterator4.return) {
                  _iterator4.return();
                }
              } finally {
                if (_didIteratorError4) {
                  throw _iteratorError4;
                }
              }
            }
          }
        }, {
          key: 'markCorrect',
          value: function markCorrect(reductionKey) {
            var _iteratorNormalCompletion5 = true;
            var _didIteratorError5 = false;
            var _iteratorError5 = undefined;

            try {
              for (var _iterator5 = this.$scope.situationFeedback[Symbol.iterator](), _step5; !(_iteratorNormalCompletion5 = (_step5 = _iterator5.next()).done); _iteratorNormalCompletion5 = true) {
                var feedback = _step5.value;

                if (feedback.alarmKey === reductionKey && feedback.feedbackType === Model.FeedbackTypes.FALSE_POSITIVE) {
                  feedback.feedbackType = Model.FeedbackTypes.CORRECT;
                  this.$scope.feedbackCorrectCount++;
                  this.$scope.feedbackIncorrectCount--;
                  break;
                }
              }
            } catch (err) {
              _didIteratorError5 = true;
              _iteratorError5 = err;
            } finally {
              try {
                if (!_iteratorNormalCompletion5 && _iterator5.return) {
                  _iterator5.return();
                }
              } finally {
                if (_didIteratorError5) {
                  throw _iteratorError5;
                }
              }
            }
          }
        }, {
          key: 'submitEditedFeedback',
          value: function submitEditedFeedback(form) {
            var _iteratorNormalCompletion6 = true;
            var _didIteratorError6 = false;
            var _iteratorError6 = undefined;

            try {
              for (var _iterator6 = this.$scope.situationFeedback[Symbol.iterator](), _step6; !(_iteratorNormalCompletion6 = (_step6 = _iterator6.next()).done); _iteratorNormalCompletion6 = true) {
                var feedback = _step6.value;

                if (form) {
                  feedback.reason = form.reason;
                }
              }
            } catch (err) {
              _didIteratorError6 = true;
              _iteratorError6 = err;
            } finally {
              try {
                if (!_iteratorNormalCompletion6 && _iterator6.return) {
                  _iterator6.return();
                }
              } finally {
                if (_didIteratorError6) {
                  throw _iteratorError6;
                }
              }
            }

            this.submitFeedback(this.$scope.situationFeedback);
          }
        }, {
          key: 'submitFeedback',
          value: function submitFeedback(feedback) {
            var self = this;
            this.getDatasource().then(function (ds) {
              return ds.submitSituationFeedback(self.$scope.alarm.id, feedback);
            }).then(function (response) {
              self.$scope.editFeedback = false;
              self.$scope.submittedFeedback = true;
              self.$scope.hasSituationFeedback = true;
            }).catch(function (reason) {
              console.log("Got POST error: ", reason);
              self.$scope.editFeedback = false;
            });
          }
        }, {
          key: 'updateFeedback',
          value: function updateFeedback(feedback) {
            var _iteratorNormalCompletion7 = true;
            var _didIteratorError7 = false;
            var _iteratorError7 = undefined;

            try {
              for (var _iterator7 = feedback[Symbol.iterator](), _step7; !(_iteratorNormalCompletion7 = (_step7 = _iterator7.next()).done); _iteratorNormalCompletion7 = true) {
                var fb = _step7.value;
                var _iteratorNormalCompletion8 = true;
                var _didIteratorError8 = false;
                var _iteratorError8 = undefined;

                try {
                  for (var _iterator8 = this.$scope.situationFeedback[Symbol.iterator](), _step8; !(_iteratorNormalCompletion8 = (_step8 = _iterator8.next()).done); _iteratorNormalCompletion8 = true) {
                    var ifb = _step8.value;

                    if (fb.alarmKey === ifb.alarmKey) ifb = fb;
                  }
                } catch (err) {
                  _didIteratorError8 = true;
                  _iteratorError8 = err;
                } finally {
                  try {
                    if (!_iteratorNormalCompletion8 && _iterator8.return) {
                      _iterator8.return();
                    }
                  } finally {
                    if (_didIteratorError8) {
                      throw _iteratorError8;
                    }
                  }
                }
              }
            } catch (err) {
              _didIteratorError7 = true;
              _iteratorError7 = err;
            } finally {
              try {
                if (!_iteratorNormalCompletion7 && _iterator7.return) {
                  _iterator7.return();
                }
              } finally {
                if (_didIteratorError7) {
                  throw _iteratorError7;
                }
              }
            }
          }
        }, {
          key: 'editSituationFeedback',
          value: function editSituationFeedback() {
            this.$scope.editFeedback = true;
            this.$scope.submittedFeedback = false;
          }
        }, {
          key: 'situationFeedbackButton',
          value: function situationFeedbackButton() {
            var button = this.CORRECT_OUTLINED;
            var fingerprint = this.fingerPrint(this.$scope.alarm);
            if (this.$scope.situationFeedback) {
              var _iteratorNormalCompletion9 = true;
              var _didIteratorError9 = false;
              var _iteratorError9 = undefined;

              try {
                for (var _iterator9 = this.$scope.situationFeedback[Symbol.iterator](), _step9; !(_iteratorNormalCompletion9 = (_step9 = _iterator9.next()).done); _iteratorNormalCompletion9 = true) {
                  var feedback = _step9.value;

                  if (feedback.situationFingerprint == fingerprint && this.$scope.hasSituationFeedback) {
                    button = this.CORRECT_FILLED;
                    break;
                  }
                }
              } catch (err) {
                _didIteratorError9 = true;
                _iteratorError9 = err;
              } finally {
                try {
                  if (!_iteratorNormalCompletion9 && _iterator9.return) {
                    _iterator9.return();
                  }
                } finally {
                  if (_didIteratorError9) {
                    throw _iteratorError9;
                  }
                }
              }
            }
            return button;
          }
        }, {
          key: 'cancelEditedFeedback',
          value: function cancelEditedFeedback() {
            this.$scope.situationFeedback = this.initalizeFeeback();
            this.$scope.editFeedback = false;
            this.$scope.submittedFeedback = false;
          }
        }, {
          key: 'fingerPrint',
          value: function fingerPrint(situation) {
            return btoa(md5(situation.relatedAlarms));
          }
        }, {
          key: 'getDatasource',
          value: function getDatasource() {
            return this.datasourceSrv.get(this.$scope.source).then(function (ds) {
              if (ds.type && ds.type.indexOf("fault-datasource") < 0) {
                throw { message: 'Only OpenNMS datasources are supported' };
              } else {
                return ds;
              }
            });
          }
        }]);

        return AlarmDetailsCtrl;
      }());

      _export('AlarmDetailsCtrl', AlarmDetailsCtrl);
    }
  };
});
//# sourceMappingURL=alarm_details.js.map
