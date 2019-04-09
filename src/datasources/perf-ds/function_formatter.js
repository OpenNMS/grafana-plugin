const ARGUMENT_MATCH = /\s*,\s*/;

import parse from 'parenthesis/index';

const isString = (value) => {
    return typeof value === 'string' || value instanceof String;
};

const getLast = (arr) => {
    if (arr) {
        if (Array.isArray(arr) && arr.length > 0) {
            return arr[arr.length - 1];
        }
    }
    return undefined;
};

export class FunctionFormatter {
    /**
     * Convert the provided label into an array containing a mix of string values
     * and function definitions for replacement.
     */
    static parenthesize(label) {
        return FunctionFormatter._process(parse(label, {
            brackets: ['()']
        }));
    }

    /**
     * Preprocess the parenthesized output so that format object arguments are parameterized
     */
    static parenthesizeWithArguments(label) {
        const parenthesized = FunctionFormatter.parenthesize(label);
        return parenthesized.map(entry => {
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
    static findFunctions(label) {
        return FunctionFormatter.parenthesizeWithArguments(label).filter(entry => entry && entry.name !== undefined);
    }

    /**
     * Given an argument string, return a list of arguments.
     */
    static getArguments(args) {
        const argsString = (args === undefined || args === null)? '' : args;
        if (argsString.length === 0) {
            return [];
        }
        const split = argsString.split(ARGUMENT_MATCH);
        return Array.isArray(split) ? split : [split];
    }

    /**
     * Given a label, replace instances of the functions in the replacements object.
     * @param {string} label - the label string
     * @param {*} replacements - an object of function names and their callbacks
     */
    static replace(label, replacements) {
        const parenthesized = FunctionFormatter.parenthesizeWithArguments(label);

        let ret = '';
        parenthesized.forEach(token => {
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
    static format(label, metadata) {
        return FunctionFormatter.replace(label, {
            nodeToLabel: (nodeCriteria) => {
                const node = FunctionFormatter._getNodeFromCriteria(metadata, nodeCriteria);
                if (node) {
                    return node.label;
                }
                return nodeCriteria;
            },
            resourceToLabel: (criteriaOrResourceId, partialResourceId) => {
                const resource = FunctionFormatter._getResource(metadata, criteriaOrResourceId, partialResourceId);
                if (resource) {
                    return resource.label;
                }
                return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId;
            },
            resourceToName: (criteriaOrResourceId, partialResourceId) => {
                const resource = FunctionFormatter._getResource(metadata, criteriaOrResourceId, partialResourceId);
                if (resource) {
                    return resource.name;
                }
                return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId;
            },
            resourceToInterface: (criteriaOrResourceId, partialResourceId) => {
                const resource = FunctionFormatter._getResource(metadata, criteriaOrResourceId, partialResourceId);
                if (resource) {
                    let match = resource.name.match(/^(\w+)/);
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
    static _process(args) {
        const ret = [];
        const matcher = /^(.*?)(\w+?)\($/;
        let skip = false;
        args.forEach((arg, index) => {
            if (skip) {
                skip = false;
                return;
            }
            const prev = ret.length ? ret[ret.length - 1] : undefined;
            const next = args[index + 1];

            let match;
            if (Array.isArray(arg)) {
                ret.push(FunctionFormatter._process(arg));
            } else if ((match = matcher.exec(arg)) !== null) {
                let prefix = match[1];
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
                const replacement = arg.replace(/^\)/, '');
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
    static _flatten(args) {
        let ret = [];
        args.forEach((arg) => {
            if (isString(arg)) {
                if (arg.length === 0) {
                    return;
                }
                const prev = getLast(ret);
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
                const result = FunctionFormatter._flatten(arg);
                result.forEach((res) => {
                    const prev = getLast(ret);
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

    static _getNodeFromCriteria(metadata, nodeCriteria) {
        let nodeId, foreignSource, foreignId;
        if (nodeCriteria && nodeCriteria.indexOf(':') > 0) {
            [foreignSource, foreignId] = nodeCriteria.split(':');
        } else {
            nodeId = parseInt(nodeCriteria, 10);
        }
        return FunctionFormatter._getNodeFromMetadata(metadata, nodeId, foreignSource, foreignId);
    }

    static _getNodeFromMetadata(metadata, nodeId, foreignSource, foreignId) {
        if (metadata && metadata.nodes) {
            const ret = metadata.nodes.filter((node) => {
                return (nodeId !== undefined && node.id === nodeId) ||
                (foreignSource !== undefined && foreignId !== undefined &&
                    node['foreign-source'] === foreignSource && node['foreign-id'] === foreignId);
            })[0];
            if (ret !== undefined) {
                return ret;
            }
        }
        //console.warn('Unable to locate node ' + [nodeId, foreignSource, foreignId].join(',') + ' in metadata.', metadata);
        return null;
    }

    static _getResource(metadata, criteriaOrResourceId, partialResourceId) {
        if (partialResourceId === undefined) {
            for (const resource of metadata.resources) {
                if (resource.id === criteriaOrResourceId) {
                    return resource;
                }
            }
        } else {
            const node = FunctionFormatter._getNodeFromCriteria(metadata, criteriaOrResourceId);
            if (node) {
                const resource = FunctionFormatter._getResourceFromCriteria(metadata, partialResourceId, 'node[' + node['foreign-source'] + ':' + node['foreign-id'] + ']', 'node[' + node.id + ']');
                if (resource) {
                    return resource;
                }
            }
        }
        //console.warn('Unable to locate resource ' + [criteriaOrResourceId,partialResourceId].join('.') + ' in metadata.', metadata);
        return null;
    }

    static _getResourceFromCriteria(metadata, resourceCriteria, ...nodeCriterias) {
        if (metadata && metadata.resources) {
            const ret = metadata.resources.filter((resource) => {
                if (resource.id === resourceCriteria) return true;
                for (const criteria of nodeCriterias.map(c => c + '.' + resourceCriteria)) {
                    if (resource.id === criteria) {
                        return true;
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
}