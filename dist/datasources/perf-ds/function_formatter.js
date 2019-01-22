'use strict';

System.register([], function (_export, _context) {
    "use strict";

    var _slicedToArray, _createClass, FUNCTION_MATCH, ARGUMENT_MATCH, FunctionFormatter;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [],
        execute: function () {
            _slicedToArray = function () {
                function sliceIterator(arr, i) {
                    var _arr = [];
                    var _n = true;
                    var _d = false;
                    var _e = undefined;

                    try {
                        for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) {
                            _arr.push(_s.value);

                            if (i && _arr.length === i) break;
                        }
                    } catch (err) {
                        _d = true;
                        _e = err;
                    } finally {
                        try {
                            if (!_n && _i["return"]) _i["return"]();
                        } finally {
                            if (_d) throw _e;
                        }
                    }

                    return _arr;
                }

                return function (arr, i) {
                    if (Array.isArray(arr)) {
                        return arr;
                    } else if (Symbol.iterator in Object(arr)) {
                        return sliceIterator(arr, i);
                    } else {
                        throw new TypeError("Invalid attempt to destructure non-iterable instance");
                    }
                };
            }();

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

            FUNCTION_MATCH = /(\w+)\(([^\)]*)\)/g;
            ARGUMENT_MATCH = /\s*,\s*/;

            _export('FunctionFormatter', FunctionFormatter = function () {
                function FunctionFormatter() {
                    _classCallCheck(this, FunctionFormatter);
                }

                _createClass(FunctionFormatter, null, [{
                    key: 'findFunctions',
                    value: function findFunctions(label) {
                        var match = void 0,
                            ret = [];
                        while ((match = FUNCTION_MATCH.exec(label)) !== null) {
                            var args = FunctionFormatter.getArguments(match[2]);
                            ret.push({
                                name: match[1],
                                arguments: FunctionFormatter.getArguments(match[2])
                            });
                        }
                        return ret;
                    }
                }, {
                    key: 'getArguments',
                    value: function getArguments(args) {
                        var argsString = args === null ? '' : args.trim();
                        if (argsString.length === 0) {
                            return [];
                        }
                        var split = argsString.split(ARGUMENT_MATCH);
                        return Array.isArray(split) ? split : [split];
                    }
                }, {
                    key: 'replace',
                    value: function replace(label, replacements) {
                        var match = void 0;
                        var ret = label;
                        while ((match = FUNCTION_MATCH.exec(label)) !== null) {
                            var func = match[1],
                                args = FunctionFormatter.getArguments(match[2]);
                            if (replacements.hasOwnProperty(func)) {
                                var result = replacements[func].apply(replacements[func], args);
                                ret = ret.replace(match[0], result);
                            } else {
                                console.warn('LabelFormatter.replace: unhandled function ' + func);
                            }
                        }
                        return ret;
                    }
                }, {
                    key: 'format',
                    value: function format(label, metadata) {
                        return FunctionFormatter.replace(label, {
                            nodeToLabel: function nodeToLabel(nodeCriteria) {
                                var node = FunctionFormatter._getNodeFromCriteria(metadata, nodeCriteria);
                                if (node) {
                                    return node.label;
                                }
                                return nodeCriteria;
                            },
                            resourceToLabel: function resourceToLabel(criteriaOrResourceId, partialResourceId) {
                                var resource = FunctionFormatter._getResource(metadata, criteriaOrResourceId, partialResourceId);
                                if (resource) {
                                    return resource.label;
                                }
                                return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId;
                            },
                            resourceToName: function resourceToName(criteriaOrResourceId, partialResourceId) {
                                var resource = FunctionFormatter._getResource(metadata, criteriaOrResourceId, partialResourceId);
                                if (resource) {
                                    return resource.name;
                                }
                                return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId;
                            },
                            resourceToInterface: function resourceToInterface(criteriaOrResourceId, partialResourceId) {
                                var resource = FunctionFormatter._getResource(metadata, criteriaOrResourceId, partialResourceId);
                                if (resource) {
                                    var match = resource.name.match(/^(\w+)/);
                                    if (!match) {
                                        match = resource.label.match(/^(\w+)/);
                                    }
                                    if (match) {
                                        return match[1];
                                    }
                                }
                                return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId;
                            }
                        });
                    }
                }, {
                    key: '_getNodeFromCriteria',
                    value: function _getNodeFromCriteria(metadata, nodeCriteria) {
                        var nodeId = void 0,
                            foreignSource = void 0,
                            foreignId = void 0;
                        if (nodeCriteria && nodeCriteria.indexOf(':') > 0) {
                            var _nodeCriteria$split = nodeCriteria.split(':');

                            var _nodeCriteria$split2 = _slicedToArray(_nodeCriteria$split, 2);

                            foreignSource = _nodeCriteria$split2[0];
                            foreignId = _nodeCriteria$split2[1];
                        } else {
                            nodeId = parseInt(nodeCriteria, 10);
                        }
                        return FunctionFormatter._getNodeFromMetadata(metadata, nodeId, foreignSource, foreignId);
                    }
                }, {
                    key: '_getNodeFromMetadata',
                    value: function _getNodeFromMetadata(metadata, nodeId, foreignSource, foreignId) {
                        if (metadata && metadata.nodes) {
                            var ret = metadata.nodes.filter(function (node) {
                                return nodeId !== undefined && node.id === nodeId || foreignSource !== undefined && foreignId !== undefined && node['foreign-source'] === foreignSource && node['foreign-id'] === foreignId;
                            })[0];
                            if (ret !== undefined) {
                                return ret;
                            }
                        }
                        //console.warn('Unable to locate node ' + [nodeId, foreignSource, foreignId].join(',') + ' in metadata.', metadata);
                        return null;
                    }
                }, {
                    key: '_getResource',
                    value: function _getResource(metadata, criteriaOrResourceId, partialResourceId) {
                        if (partialResourceId === undefined) {
                            var _iteratorNormalCompletion = true;
                            var _didIteratorError = false;
                            var _iteratorError = undefined;

                            try {
                                for (var _iterator = metadata.resources[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                                    var resource = _step.value;

                                    if (resource.id === criteriaOrResourceId) {
                                        return resource;
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
                        } else {
                            var node = FunctionFormatter._getNodeFromCriteria(metadata, criteriaOrResourceId);
                            if (node) {
                                var _resource = FunctionFormatter._getResourceFromCriteria(metadata, partialResourceId, 'node[' + node['foreign-source'] + ':' + node['foreign-id'] + ']', 'node[' + node.id + ']');
                                if (_resource) {
                                    return _resource;
                                }
                            }
                        }
                        //console.warn('Unable to locate resource ' + [criteriaOrResourceId,partialResourceId].join('.') + ' in metadata.', metadata);
                        return null;
                    }
                }, {
                    key: '_getResourceFromCriteria',
                    value: function _getResourceFromCriteria(metadata, resourceCriteria) {
                        for (var _len = arguments.length, nodeCriterias = Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
                            nodeCriterias[_key - 2] = arguments[_key];
                        }

                        if (metadata && metadata.resources) {
                            var ret = metadata.resources.filter(function (resource) {
                                if (resource.id === resourceCriteria) return true;
                                var _iteratorNormalCompletion2 = true;
                                var _didIteratorError2 = false;
                                var _iteratorError2 = undefined;

                                try {
                                    for (var _iterator2 = nodeCriterias.map(function (c) {
                                        return c + '.' + resourceCriteria;
                                    })[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
                                        var criteria = _step2.value;

                                        if (resource.id === criteria) {
                                            return true;
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

                                return false;
                            })[0];
                            if (ret !== undefined) {
                                return ret;
                            }
                        }
                        //console.warn('Unable to locate resource ' + resourceCriteria + ' in metadata.', metadata);
                        return null;
                    }
                }]);

                return FunctionFormatter;
            }());

            _export('FunctionFormatter', FunctionFormatter);
        }
    };
});
//# sourceMappingURL=function_formatter.js.map
