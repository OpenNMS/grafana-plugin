const FUNCTION_MATCH = /(\w+)\(([^\)]*)\)/g;
const ARGUMENT_MATCH = /\s*,\s*/;

export class FunctionFormatter {
    static findFunctions(label) {
        let match, ret = [];
        while ((match = FUNCTION_MATCH.exec(label)) !== null) {
            const args = FunctionFormatter.getArguments(match[2]);
            ret.push({
                name: match[1],
                arguments: FunctionFormatter.getArguments(match[2])
            });
        }
        return ret;
    }

    static getArguments(args) {
        const argsString = args === null? '' : args.trim();
        if (argsString.length === 0) {
            return [];
        }
        const split = argsString.split(ARGUMENT_MATCH);
        return Array.isArray(split) ? split : [split];
    }

    static replace(label, replacements) {
        let match;
        let ret = label;
        while ((match = FUNCTION_MATCH.exec(label)) !== null) {
            const func = match[1],
                args = FunctionFormatter.getArguments(match[2]);
            if (replacements.hasOwnProperty(func)) {
                const result = replacements[func].apply(replacements[func], args);
                ret = ret.replace(match[0], result);
            } else {
                console.warn('LabelFormatter.replace: unhandled function ' + func);
            }
        }
        return ret;
    }
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