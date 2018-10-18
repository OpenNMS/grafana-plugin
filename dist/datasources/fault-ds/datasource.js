'use strict';

System.register(['../../lib/client_delegate', '../../opennms', './FilterCloner', './Mapping', 'lodash'], function (_export, _context) {
    "use strict";

    var ClientDelegate, API, Model, FilterCloner, Mapping, _, _createClass, FeaturedAttributes, isNumber, OpenNMSFMDatasource;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_libClient_delegate) {
            ClientDelegate = _libClient_delegate.ClientDelegate;
        }, function (_opennms) {
            API = _opennms.API;
            Model = _opennms.Model;
        }, function (_FilterCloner) {
            FilterCloner = _FilterCloner.FilterCloner;
        }, function (_Mapping) {
            Mapping = _Mapping.Mapping;
        }, function (_lodash) {
            _ = _lodash.default;
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

            FeaturedAttributes = ["affectedNodeCount", "alarmAckTime", "category", "ipAddress", "isSituation", "isInSituation", "location", "node", "node.label", "reductionKey", "service", "severity", "situtationAlarmCount", "uei"];

            isNumber = function isNumber(num) {
                return parseInt(num, 10) + '' === num + '';
            };

            _export('OpenNMSFMDatasource', OpenNMSFMDatasource = function () {
                function OpenNMSFMDatasource(instanceSettings, $q, backendSrv, templateSrv, contextSrv) {
                    _classCallCheck(this, OpenNMSFMDatasource);

                    this.type = instanceSettings.type;
                    this.url = instanceSettings.url;
                    this.name = instanceSettings.name;
                    this.q = $q;
                    this.backendSrv = backendSrv;
                    this.templateSrv = templateSrv;
                    this.alarmClient = new ClientDelegate(instanceSettings, backendSrv, $q);

                    // When enabled in the datasource, the grafana user should be used instead of the datasource username on
                    // supported operations
                    if (instanceSettings.jsonData && instanceSettings.jsonData.useGrafanaUser) {
                        // If the datasource contains the field which should be used and that field is set, use it
                        if (instanceSettings.jsonData.grafanaUserField && contextSrv.user[instanceSettings.jsonData.grafanaUserField]) {
                            this.user = contextSrv.user[instanceSettings.jsonData.grafanaUserField];
                        } else {
                            // otherwise the login is used instead
                            this.user = contextSrv.user.login;
                        }
                    }
                }

                _createClass(OpenNMSFMDatasource, [{
                    key: 'query',
                    value: function query(options) {
                        var _this = this;

                        // Initialize filter
                        var filter = options.targets[0].filter || new API.Filter();
                        filter.limit = 0; // no limit

                        options.enforceTimeRange = true;
                        var clonedFilter = this.buildQuery(filter, options);

                        var self = this;
                        return this.alarmClient.findAlarms(clonedFilter).then(function (alarms) {
                            return _this.alarmClient.getClientWithMetadata().then(function (client) {
                                return {
                                    data: self.toTable(alarms, client.server.metadata)
                                };
                            });
                        });
                    }
                }, {
                    key: 'buildQuery',
                    value: function buildQuery(filter, options) {
                        var clonedFilter = new FilterCloner().cloneFilter(filter);

                        // Before replacing any variables, add a global time range restriction (which is hidden to the user)
                        if (options && options.enforceTimeRange) {
                            clonedFilter.withAndRestriction(new API.NestedRestriction().withAndRestriction(new API.Restriction("lastEventTime", API.Comparators.GE, "$range_from")).withAndRestriction(new API.Restriction("lastEventTime", API.Comparators.LE, "$range_to")));
                        }

                        // Substitute $<variable> or [[variable]] in the restriction value
                        this.substitute(clonedFilter.clauses, options);
                        return clonedFilter;
                    }
                }, {
                    key: '_getTemplateVariable',
                    value: function _getTemplateVariable(name) {
                        if (this.templateSrv.variables && this.templateSrv.variables.length > 0) {
                            return this.templateSrv.variables.filter(function (v) {
                                return v.name === name;
                            })[0];
                        }
                        return undefined;
                    }
                }, {
                    key: 'subtituteNodeRestriction',
                    value: function subtituteNodeRestriction(clause) {
                        var restriction = clause.restriction;
                        // Handle "node" as a special case, updating restrictions to either foreignSource+foreignId or node.id
                        if (restriction.attribute === 'node') {
                            if (restriction.value.indexOf(':') > 0) {
                                if (restriction.comparator.id !== API.Comparators.EQ.id) {
                                    console.log('WARNING: Using a comparator other than EQ will probably not work as expected with a foreignSource:foreignId node criteria.');
                                }
                                var nodeCriteria = restriction.value.split(':');
                                var replacement = new API.NestedRestriction(new API.Clause(new API.Restriction('node.foreignSource', restriction.comparator, nodeCriteria[0]), API.Operators.AND), new API.Clause(new API.Restriction('node.foreignId', restriction.comparator, nodeCriteria[1]), API.Operators.AND));
                                clause.restriction = replacement;
                            } else if (isNumber(restriction.value)) {
                                clause.restriction = new API.Restriction('node.id', restriction.comparator, restriction.value);
                            } else {
                                console.log('WARNING: found a "node" criteria but it does not appear to be a node ID nor a foreignSource:foreignId tuple.', restriction);
                            }
                        }
                    }
                }, {
                    key: 'substitute',
                    value: function substitute(clauses, options) {
                        var self = this;
                        var remove = [];
                        _.each(clauses, function (clause) {
                            if (clause.restriction) {
                                var restriction = clause.restriction;
                                if (restriction instanceof API.NestedRestriction) {
                                    self.substitute(restriction.clauses, options);
                                } else if (restriction.value) {
                                    var variableName = self.templateSrv.getVariableName(restriction.value);
                                    var templateVariable = self._getTemplateVariable(variableName);

                                    // Process multi-selects
                                    if (templateVariable && templateVariable.multi) {
                                        if (templateVariable.current.value && self.templateSrv.isAllValue(templateVariable.current.value)) {
                                            // if we're querying "all" we just dump the clause altogether
                                            remove.push(clause);
                                        } else {
                                            // annoyingly, depending on how you interact with the UI, if one value is selected it will
                                            // *either* be an array with 1 entry, or just the raw value >:|
                                            // so we normalize it back to just the raw value here if necessary
                                            if (_.isArray(templateVariable.current.value) && templateVariable.current.value.length === 1) {
                                                templateVariable.current.value = templateVariable.current.value[0];
                                            }

                                            // now if it's *still* an array, we chop it up into nested restrictions
                                            if (_.isArray(templateVariable.current.value)) {
                                                var replacement = new API.NestedRestriction();
                                                var values = templateVariable.current.value;
                                                if (!_.isArray(values)) {
                                                    values = [values];
                                                }
                                                var _iteratorNormalCompletion = true;
                                                var _didIteratorError = false;
                                                var _iteratorError = undefined;

                                                try {
                                                    for (var _iterator = values[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                                                        var value = _step.value;

                                                        if (restriction.comparator.id === API.Comparators.EQ.id) {
                                                            replacement.withOrRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value));
                                                        } else if (restriction.comparator.id === API.Comparators.NE.id) {
                                                            replacement.withAndRestriction(new API.Restriction(restriction.attribute, restriction.comparator, value));
                                                        } else {
                                                            throw new Error('Unable to query "' + restriction.attribute + '": multi-select values with variable substitution must be either "=" or "!="');
                                                        }
                                                    }

                                                    // we've turned a single restriction into a nested one, so re-process it as a
                                                    // collection and skip the simple replacement below
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

                                                clause.restriction = replacement;
                                                self.substitute(clause.restriction.clauses, options);
                                                return;
                                            }
                                        }
                                    }

                                    // Range must be of type date, otherwise it is not parseable by the OpenNMS client
                                    if (variableName === 'range_from') {
                                        restriction.value = options.range.from;
                                    } else if (variableName === 'range_to') {
                                        restriction.value = options.range.to;
                                    } else {
                                        restriction.value = self.templateSrv.replace(restriction.value, options.scopedVars);
                                    }

                                    self.subtituteNodeRestriction(clause);
                                }
                            }
                        });
                        var _iteratorNormalCompletion2 = true;
                        var _didIteratorError2 = false;
                        var _iteratorError2 = undefined;

                        try {
                            for (var _iterator2 = remove[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
                                var r = _step2.value;

                                var i = clauses.indexOf(r);
                                if (i >= 0) {
                                    clauses.splice(i, 1);
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
                }, {
                    key: 'testDatasource',
                    value: function testDatasource() {
                        return this.alarmClient.getClientWithMetadata().then(function (metadata) {
                            if (metadata) {
                                return {
                                    status: "success",
                                    message: "Data source is working",
                                    title: "Success"
                                };
                            } else {
                                return {
                                    status: "danger",
                                    message: "OpenNMS provided a response, but no metadata was found.",
                                    title: "Unexpected Response"
                                };
                            }
                        }).catch(function (e) {
                            if (e.message === "Unsupported Version") {
                                return {
                                    status: "danger",
                                    message: "The OpenNMS version you are trying to connect to is not supported. " + "OpenNMS Horizon version >= 20.1.0 or OpenNMS Meridian version >= 2017.1.0 is required.",
                                    title: e.message
                                };
                            } else {
                                throw e;
                            }
                        });
                    }
                }, {
                    key: 'annotationQuery',
                    value: function annotationQuery(options) {
                        return this.q.when([]);
                    }
                }, {
                    key: 'metricFindQuery',
                    value: function metricFindQuery(query) {
                        if (!query || !query.find) {
                            return this.q.when([]);
                        }

                        if (query.find === "attributes") {
                            if (query.strategy === 'featured') {
                                var featuredAttributes = _.map(_.sortBy(FeaturedAttributes), function (attribute) {
                                    return { id: attribute };
                                });
                                return this.q.when(featuredAttributes);
                            }
                            // assume all
                            return this.alarmClient.getProperties();
                        }
                        if (query.find === "comparators") {
                            var attribute = new Mapping.AttributeMapping().getApiAttribute(query.attribute);
                            return this.alarmClient.getPropertyComparators(attribute);
                        }
                        if (query.find == 'values') {
                            return this.searchForValues(query);
                        }
                        if (query.find === 'operators') {
                            return this.alarmClient.findOperators();
                        }
                        return this.q.when([]);
                    }
                }, {
                    key: 'searchForValues',
                    value: function searchForValues(query) {
                        var _this2 = this;

                        var attribute = new Mapping.AttributeMapping().getApiAttribute(query.attribute);
                        if (attribute === 'ipAddr') {
                            attribute = 'ipInterface.ipAddress';
                        }
                        if (attribute === 'isSituation' || attribute === 'isInSituation') {
                            return this.q.when([{ id: 'false', label: 'false' }, { id: 'true', label: 'true' }]);
                        }
                        return this.alarmClient.findProperty(attribute).then(function (property) {
                            if (!property) {
                                return _this2.q.when([]);
                            }
                            // Special handling for properties
                            switch (property.id) {
                                // Severity is handled separately as otherwise the severity ordinal vs the severity label would be
                                // used, but that may not be ideal for the user
                                case 'severity':
                                    var severities = _.map(Model.Severities, function (severity) {
                                        return {
                                            id: severity.id,
                                            label: severity.label
                                        };
                                    });
                                    return _this2.q.when(severities);
                            }
                            return property.findValues({ limit: 1000 }).then(function (values) {
                                return values.map(function (value) {
                                    return { id: value, label: value };
                                });
                            });
                        });
                    }
                }, {
                    key: 'toTable',
                    value: function toTable(alarms, metadata) {
                        var _this3 = this;

                        var columnNames = ["ID", "Count", "Acked By", "Ack Time", "UEI", "Severity", "Type", "Description", "Location", "Log Message", "Reduction Key", "Trouble Ticket", "Trouble Ticket State", "Node ID", "Node Label", "Service", "Suppressed Time", "Suppressed Until", "Suppressed By", "IP Address", "First Event Time", "Last Event ID", "Last Event Time", "Last Event Source", "Last Event Creation Time", "Last Event Severity", "Last Event Label", "Last Event Location", "Sticky ID", "Sticky Note", "Sticky Author", "Sticky Update Time", "Sticky Creation Time", "Journal ID", "Journal Note", "Journal Author", "Journal Update Time", "Journal Creation Time", "Is Situation", "Situation Alarm Count", "Affected Node Count", "Data Source"];

                        // Build a sorted list of (unique) event parameter names
                        var parameterNames = _.uniq(_.sortBy(_.flatten(_.map(alarms, function (alarm) {
                            if (!alarm.lastEvent || !alarm.lastEvent.parameters) {
                                return [];
                            }
                            return _.map(alarm.lastEvent.parameters, function (parameter) {
                                return parameter.name;
                            });
                        })), function (name) {
                            return name;
                        }), true);

                        // Include the event parameters as columns
                        _.each(parameterNames, function (parameterName) {
                            columnNames.push("Param_" + parameterName);
                        });

                        var columns = _.map(columnNames, function (column) {
                            return { "text": column };
                        });

                        var self = this;
                        var rows = _.map(alarms, function (alarm) {
                            var row = [alarm.id, alarm.count, alarm.ackUser, alarm.ackTime, alarm.uei, alarm.severity.label, alarm.type ? alarm.type.label : undefined, alarm.description, alarm.location, alarm.logMessage, alarm.reductionKey, alarm.troubleTicket, alarm.troubleTicketState ? alarm.troubleTicketState.label : undefined, alarm.nodeId, alarm.nodeLabel, alarm.service ? alarm.service.name : undefined, alarm.suppressedTime, alarm.suppressedUntil, alarm.suppressedBy, alarm.lastEvent ? alarm.lastEvent.ipAddress ? alarm.lastEvent.ipAddress.address : undefined : undefined,

                            // Event
                            alarm.firstEventTime, alarm.lastEvent ? alarm.lastEvent.id : undefined, alarm.lastEvent ? alarm.lastEvent.time : undefined, alarm.lastEvent ? alarm.lastEvent.source : undefined, alarm.lastEvent ? alarm.lastEvent.createTime : undefined, alarm.lastEvent ? alarm.lastEvent.severity.label : undefined, alarm.lastEvent ? alarm.lastEvent.label : undefined, alarm.lastEvent ? alarm.lastEvent.location : undefined,

                            // Sticky Note
                            alarm.sticky ? alarm.sticky.id : undefined, alarm.sticky ? alarm.sticky.body : undefined, alarm.sticky ? alarm.sticky.author : undefined, alarm.sticky ? alarm.sticky.updated : undefined, alarm.sticky ? alarm.sticky.created : undefined,

                            // Journal Note
                            alarm.journal ? alarm.journal.id : undefined, alarm.journal ? alarm.journal.body : undefined, alarm.journal ? alarm.journal.author : undefined, alarm.journal ? alarm.journal.updated : undefined, alarm.journal ? alarm.journal.created : undefined,

                            // Situation Data
                            alarm.relatedAlarms && alarm.relatedAlarms.length > 0 ? 'Y' : 'N', alarm.relatedAlarms ? alarm.relatedAlarms.length.toFixed(0) : undefined, alarm.affectedNodeCount ? alarm.affectedNodeCount.toFixed(0) : undefined,

                            // Data Source
                            self.name];

                            // Index the event parameters by name
                            var eventParametersByName = {};
                            if (alarm.lastEvent && alarm.lastEvent.parameters) {
                                _.each(alarm.lastEvent.parameters, function (parameter) {
                                    eventParametersByName[parameter.name] = parameter.value;
                                });
                            }

                            // Append the event parameters to the row
                            row = row.concat(_.map(parameterNames, function (parameterName) {
                                if (_.has(eventParametersByName, parameterName)) {
                                    return eventParametersByName[parameterName];
                                } else {
                                    return undefined;
                                }
                            }));

                            row.meta = {
                                // Store the alarm for easy access by the panels - may not be necessary
                                'alarm': alarm,
                                // Store the name of the data-source as part of the data so that
                                // the panel can grab an instance of the DS to perform actions
                                // on the alarms
                                "source": _this3.name,
                                // Store the ticketerConfig here
                                "ticketerConfig": metadata.ticketerConfig
                            };
                            return row;
                        });

                        return [{
                            "columns": columns,
                            "rows": rows,
                            "type": "table"
                        }];
                    }
                }, {
                    key: 'getAlarm',
                    value: function getAlarm(alarmId) {
                        return this.alarmClient.getAlarm(alarmId);
                    }
                }, {
                    key: 'acknowledgeAlarm',
                    value: function acknowledgeAlarm(alarmId) {
                        return this.alarmClient.doAck(alarmId, this.user);
                    }
                }, {
                    key: 'unacknowledgeAlarm',
                    value: function unacknowledgeAlarm(alarmId) {
                        return this.alarmClient.doUnack(alarmId, this.user);
                    }
                }, {
                    key: 'clearAlarm',
                    value: function clearAlarm(alarmId) {
                        return this.alarmClient.doClear(alarmId, this.user);
                    }
                }, {
                    key: 'escalateAlarm',
                    value: function escalateAlarm(alarmId) {
                        return this.alarmClient.doEscalate(alarmId, this.user);
                    }
                }, {
                    key: 'createTicketForAlarm',
                    value: function createTicketForAlarm(alarmId) {
                        return this.alarmClient.doTicketAction(alarmId, "create");
                    }
                }, {
                    key: 'updateTicketForAlarm',
                    value: function updateTicketForAlarm(alarmId) {
                        return this.alarmClient.doTicketAction(alarmId, "update");
                    }
                }, {
                    key: 'closeTicketForAlarm',
                    value: function closeTicketForAlarm(alarmId) {
                        return this.alarmClient.doTicketAction(alarmId, "close");
                    }
                }, {
                    key: 'saveSticky',
                    value: function saveSticky(alarmId, sticky) {
                        return this.alarmClient.saveSticky(alarmId, sticky, this.user);
                    }
                }, {
                    key: 'deleteSticky',
                    value: function deleteSticky(alarmId) {
                        return this.alarmClient.deleteSticky(alarmId);
                    }
                }, {
                    key: 'saveJournal',
                    value: function saveJournal(alarmId, journal) {
                        return this.alarmClient.saveJournal(alarmId, journal, this.user);
                    }
                }, {
                    key: 'deleteJournal',
                    value: function deleteJournal(alarmId) {
                        return this.alarmClient.deleteJournal(alarmId);
                    }
                }, {
                    key: 'getSituationFeedback',
                    value: function getSituationFeedback(situationId) {
                        return this.alarmClient.getSituationfeedback(situationId);
                    }
                }, {
                    key: 'submitSituationFeedback',
                    value: function submitSituationFeedback(situationId, feedback) {
                        return this.alarmClient.submitSituationFeedback(situationId, feedback);
                    }
                }]);

                return OpenNMSFMDatasource;
            }());

            _export('OpenNMSFMDatasource', OpenNMSFMDatasource);
        }
    };
});
//# sourceMappingURL=datasource.js.map
