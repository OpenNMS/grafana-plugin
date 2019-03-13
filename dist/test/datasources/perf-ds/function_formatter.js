'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.FunctionFormatter = undefined;

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _index = require('../../parenthesis/index');

var _index2 = _interopRequireDefault(_index);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ARGUMENT_MATCH = /\s*,\s*/;

var isString = function isString(value) {
    return typeof value === 'string' || value instanceof String;
};

var getLast = function getLast(arr) {
    if (arr) {
        if (Array.isArray(arr) && arr.length > 0) {
            return arr[arr.length - 1];
        }
    }
    return undefined;
};

var FunctionFormatter = exports.FunctionFormatter = function () {
    function FunctionFormatter() {
        _classCallCheck(this, FunctionFormatter);
    }

    _createClass(FunctionFormatter, null, [{
        key: 'parenthesize',

        /**
         * Convert the provided label into an array containing a mix of string values
         * and function definitions for replacement.
         */
        value: function parenthesize(label) {
            return FunctionFormatter._process((0, _index2.default)(label, {
                brackets: ['()']
            }));
        }

        /**
         * Preprocess the parenthesized output so that format object arguments are parameterized
         */

    }, {
        key: 'parenthesizeWithArguments',
        value: function parenthesizeWithArguments(label) {
            var parenthesized = FunctionFormatter.parenthesize(label);
            return parenthesized.map(function (entry) {
                if (entry && entry.arguments) {
                    if (entry.arguments.length < 2) {
                        entry.arguments = FunctionFormatter.getArguments(entry.arguments[0]);
                    } else {
                        console.log('unexpected arguments, expected a single string:', entry);
                    }
                }
                return entry;
            });
        }

        /**
         * Given a label, return the list of potential functions found in it.
         */

    }, {
        key: 'findFunctions',
        value: function findFunctions(label) {
            return FunctionFormatter.parenthesizeWithArguments(label).filter(function (entry) {
                return entry && entry.name !== undefined;
            });
        }

        /**
         * Given an argument string, return a list of arguments.
         */

    }, {
        key: 'getArguments',
        value: function getArguments(args) {
            var argsString = args === undefined || args === null ? '' : args;
            if (argsString.length === 0) {
                return [];
            }
            var split = argsString.split(ARGUMENT_MATCH);
            return Array.isArray(split) ? split : [split];
        }

        /**
         * Given a label, replace instances of the functions in the replacements object.
         * @param {string} label - the label string
         * @param {*} replacements - an object of function names and their callbacks
         */

    }, {
        key: 'replace',
        value: function replace(label, replacements) {
            var parenthesized = FunctionFormatter.parenthesizeWithArguments(label);

            var ret = '';
            parenthesized.forEach(function (token) {
                if (isString(token)) {
                    // just a regular scalar
                    ret += token;
                } else if (token.name) {
                    // potential function, check against replacements
                    if (replacements && replacements.hasOwnProperty(token.name)) {
                        ret += replacements[token.name].apply(replacements[token.name], token.arguments);
                    } else {
                        // not a matching function, just put it back
                        ret += token.name + '(';
                        if (token.arguments) {
                            ret += token.arguments.join(', ');
                        }
                        ret += ')';
                    }
                } else {
                    console.log('this should not happen... token=', token);
                }
            });
            return ret;
        }

        /**
         * Given a label and a set of OpenNMS measurements metadata, replace default
         * functions like `nodeToLabel` and `resourceToName`.
         * @param {string} label - the label string
         * @param {*} replacements - an object of function names and their callbacks
         */

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

        /**
         * Process the raw output of `parenthesis.parse` to detect functions.
         */

    }, {
        key: '_process',
        value: function _process(args) {
            var ret = [];
            var matcher = /^(.*?)(\w+?)\($/;
            var skip = false;
            args.forEach(function (arg, index) {
                if (skip) {
                    skip = false;
                    return;
                }
                var prev = ret.length ? ret[ret.length - 1] : undefined;
                var next = args[index + 1];

                var match = void 0;
                if (Array.isArray(arg)) {
                    ret.push(FunctionFormatter._process(arg));
                } else if ((match = matcher.exec(arg)) !== null) {
                    var prefix = match[1];
                    if (prefix && prefix.length > 0) {
                        if (prefix.startsWith(')') && prev && prev.name) {
                            prefix = prefix.replace(/^\)/, '');
                        }
                        ret.push(prefix);
                    }
                    ret.push({
                        name: match[2],
                        arguments: FunctionFormatter._process(next)
                    });
                    skip = true;
                } else if (isString(arg) && arg.startsWith(')') && prev && prev.name) {
                    var replacement = arg.replace(/^\)/, '');
                    if (replacement.length > 0) {
                        ret.push(replacement);
                    }
                } else {
                    ret.push(arg);
                }
            });
            return FunctionFormatter._flatten(ret);
        }

        /**
         * Combine string values in processed parenthesized output (from `_process`)
         * so that we end up with a flat array of scalar strings and function replacements.
         */

    }, {
        key: '_flatten',
        value: function _flatten(args) {
            var ret = [];
            args.forEach(function (arg) {
                if (isString(arg)) {
                    if (arg.length === 0) {
                        return;
                    }
                    var prev = getLast(ret);
                    // argument is a string-part of the parsed label
                    if (isString(prev)) {
                        ret[ret.length - 1] += arg;
                    } else {
                        ret.push(arg);
                    }
                } else if (arg && arg.arguments) {
                    // argument is a function, whose arguments may be flattenable as well
                    arg.arguments = FunctionFormatter._flatten(arg.arguments);
                    ret.push(arg);
                } else if (Array.isArray(arg)) {
                    // argument is sub-parens that need further flattening
                    var result = FunctionFormatter._flatten(arg);
                    result.forEach(function (res) {
                        var prev = getLast(ret);
                        if (isString(res)) {
                            if (res.trim().length === 0) {
                                return;
                            }
                            if (isString(prev)) {
                                ret[ret.length - 1] += res;
                            } else {
                                ret.push(res);
                            }
                        } else if (res && res.arguments) {
                            // argument is a function
                            ret.push(res);
                        } else {
                            throw new Error('cannot reach here');
                        }
                    });
                } else {
                    throw new Error('cannot reach here');
                }
            });
            return ret;
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
}();
//# sourceMappingURL=function_formatter.js.map
