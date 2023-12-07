import parse from 'parenthesis'
import { EntityQueries } from '../constants/constants'

const ARGUMENT_MATCH = /\s*,\s*/

interface OnmsEntityFunctionInfo {
  entityType: string
  funcName: string
  attribute: string
  labelFormat: string
  valueFormat: string
}

const isString = (value) => {
  return typeof value === 'string' || value instanceof String
}

const getLast = (arr) => {
  if (arr) {
    if (Array.isArray(arr) && arr.length > 0) {
      return arr[arr.length - 1]
    }
  }

  return undefined
}

/**
 * Combine string values in processed parenthesized output (from `_process`)
 * so that we end up with a flat array of scalar strings and function replacements.
 */
const _flatten = (args) => {
  let ret = [] as any[]

  args.forEach((arg) => {
    if (isString(arg)) {
      if (arg.length === 0) {
        return
      }

      const prev = getLast(ret)

      // argument is a string-part of the parsed label
      if (isString(prev)) {
        ret[ret.length - 1] += arg
      } else {
        ret.push(arg)
      }
    } else if (arg && arg.arguments) {
      // argument is a function, whose arguments may be flattenable as well
      arg.arguments = _flatten(arg.arguments)
      ret.push(arg)
    } else if (Array.isArray(arg)) {
      // argument is sub-parens that need further flattening
      const result = _flatten(arg)

      result.forEach((res) => {
          const prev = getLast(ret)

          if (isString(res)) {
            if (res.trim().length === 0) {
                return
            }
            if (isString(prev)) {
                ret[ret.length - 1] += res
            } else {
                ret.push(res)
            }
          } else if (res && res.arguments) {
            // argument is a function
            ret.push(res)
          } else {
            console.warn('cannot reach here (result)', prev, res, result)
            throw new Error('cannot reach here (result)')
          }
      })
    } else {
      console.warn('cannot reach here (args)', arg, args)
      throw new Error('cannot reach here (args)')
    }
  })

  return ret
}

/**
 * Process the raw output of `parenthesis.parse` to detect functions.
 */
const _process = (args) => {
  const ret = [] as any[]
  const matcher = /^(.*?)(\w+?)\($/          // NOSONAR: typescript:S5852
  let skip = false

  args.forEach((arg, index) => {
    if (skip) {
      skip = false
      return
    }

    const prev = ret.length ? ret[ret.length - 1] : undefined
    const next = args[index + 1]

    let match

    if (Array.isArray(arg)) {
      ret.push(_process(arg))
    } else if ((match = matcher.exec(arg)) !== null) {
      let prefix = match[1]

      if (prefix && prefix.length > 0) {
        if (prefix.startsWith(')') && prev && prev.name) {
          prefix = prefix.replace(/^\)/, '')
        }
        ret.push(prefix)
      }

      ret.push({
        name: match[2],
        arguments: _process(next)
      })
      skip = true
    } else if (isString(arg) && arg.startsWith(')') && prev && prev.name) {
      const replacement = arg.replace(/^\)/, '')

      if (replacement.length > 0) {
        ret.push(replacement)
      }
    } else {
      ret.push(arg)
    }
  })

  return _flatten(ret)
}

/**
 * Given an argument string, return a list of arguments.
 */
const _getArguments = (args) => {
  const argsString = args ?? ''

  if (argsString.length === 0) {
    return []
  }

  const split = argsString.split(ARGUMENT_MATCH)
  return Array.isArray(split) ? split : [split]
}

const _getNodeFromMetadata = (metadata: any, nodeId: number | undefined, foreignSource: string | undefined, foreignId: string | undefined) => {
  if (metadata && metadata.nodes) {
    const filtered = metadata.nodes.filter((node) => {
      return (nodeId !== undefined && node.id === nodeId) ||
      (foreignSource !== undefined && foreignId !== undefined &&
        node['foreign-source'] === foreignSource && node['foreign-id'] === foreignId)
    })

    const ret = filtered.length > 0 ? filtered[0] : undefined

    if (ret !== undefined) {
      return ret
    }
  }

  // console.warn('Unable to locate node ' + [nodeId, foreignSource, foreignId].join(',') + ' in metadata.', metadata)
  return null
}

const _getNodeFromCriteria = (metadata: any, nodeCriteria: string) => {
  let nodeId, foreignSource, foreignId

  if (nodeCriteria && nodeCriteria.indexOf(':') > 0) {
    [foreignSource, foreignId] = nodeCriteria.split(':')
  } else {
    nodeId = parseInt(nodeCriteria, 10)
  }

  return _getNodeFromMetadata(metadata, nodeId, foreignSource, foreignId)
}

const _getResourceFromCriteria = (metadata, resourceCriteria, ...nodeCriterias) => {
  if (metadata?.resources) {
    const filtered = metadata.resources.filter((resource) => {
      if (resource.id === resourceCriteria) {
        return true
      }

      if (nodeCriterias.some(c => resource.id === `${c}.${resourceCriteria}`)) {
        return true
      }

      return false
    })

    const ret = filtered.length > 0 ? filtered[0] : undefined

    if (ret !== undefined) {
      return ret
    }
  }

  // console.warn('Unable to locate resource ' + resourceCriteria + ' in metadata.', metadata)
  return null
}

const _getResource = (metadata, criteriaOrResourceId, partialResourceId) => {
  if (partialResourceId === undefined) {
    for (const resource of metadata.resources) {
      if (resource.id === criteriaOrResourceId) {
        return resource
      }
    }
  } else {
    const node = _getNodeFromCriteria(metadata, criteriaOrResourceId)

    if (node) {
      const fsFid = `node[${node['foreign-source']}:${node['foreign-id']}]`
      const nodeId = `node[${node.id}]`

      const resource = _getResourceFromCriteria(metadata, partialResourceId, fsFid, nodeId)

      if (resource) {
        return resource
      }
    }
  }

  // console.warn('Unable to locate resource ' + [criteriaOrResourceId,partialResourceId].join('.') + ' in metadata.', metadata)
  return null
}

/**
 * Convert the provided label into an array containing a mix of string values
 * and function definitions for replacement.
 */
const parenthesize = (label: string) => {
  return _process(parse(label, {
    brackets: ['()']
  }))
}

/**
 * Preprocess the parenthesized output so that format object arguments are parameterized
 */
const parenthesizeWithArguments = (label: string) => {
  try {
    const parenthesized = parenthesize(label)

    return parenthesized.map(entry => {
      if (entry && entry.arguments) {
        if (entry.arguments.length < 2) {
          entry.arguments = _getArguments(entry.arguments[0])
        } else {
          console.warn('unexpected arguments, expected a single string:', entry)
        }
      }

      return entry
    })
  } catch (e) {
    return []
  }
}

/**
 * Given a label, return the list of potential functions found in it.
 */
const findFunctions = (label: string) => {
  return parenthesizeWithArguments(label).filter(entry => entry?.name !== undefined)
}

/**
 * Given a label, replace instances of the functions in the replacements object.
 * @param {string} label - the label string
 * @param {*} replacements - an object of function names and their callbacks
 */
const replaceFunctions = (label: string, replacements: any) => {
  const parenthesized = parenthesizeWithArguments(label)

  let ret = ''

  parenthesized.forEach(token => {
    if (isString(token)) {
      // just a regular scalar
      ret += token
    } else if (token.name) {
      // potential function, check against replacements
      if (replacements && replacements.hasOwnProperty(token.name)) {
        ret += replacements[token.name].apply(replacements[token.name], token.arguments)
      } else {
        // not a matching function, just put it back
        ret += token.name + '('

        if (token.arguments) {
          ret += token.arguments.join(', ')
        }
        ret += ')'
      }
    } else {
      console.warn('this should not happen... token=', token)
    }
  })

  return ret
}

/**
 * Given a label and a set of OpenNMS measurements metadata, replace default
 * functions like `nodeToLabel` and `resourceToName`.
 * @param {string} label - the label string
 * @param {*} replacements - an object of function names and their callbacks
 */
const formatFunctions = (label, metadata) => {
  const replacements = {
    nodeToLabel: (nodeCriteria) => {
      const node = _getNodeFromCriteria(metadata, nodeCriteria)

      if (node) {
        return node.label
      }

      return nodeCriteria
    },
    resourceToLabel: (criteriaOrResourceId, partialResourceId) => {
      const resource = _getResource(metadata, criteriaOrResourceId, partialResourceId)

      if (resource) {
        return resource.label
      }

      return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId
    },
    resourceToName: (criteriaOrResourceId, partialResourceId) => {
        const resource = _getResource(metadata, criteriaOrResourceId, partialResourceId)

        if (resource) {
          return resource.name
        }

        return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId
    },
    resourceToInterface: (criteriaOrResourceId, partialResourceId) => {
      const resource = _getResource(metadata, criteriaOrResourceId, partialResourceId)

      if (resource) {
        let match = resource.name.match(/^(\w+)/)

        if (!match) {
          match = resource.label.match(/^(\w+)/)
        }

        if (match) {
          return match[1]
        }
      }

      return partialResourceId ? [criteriaOrResourceId, partialResourceId].join('.') : criteriaOrResourceId
    }
  }

  return replaceFunctions(label, replacements)
}

const getEntityTypeFromFuncName = (funcName: string) => {
  // key is start of function name, this will also match e.g. 'outage()' and 'outages()'
  if (funcName) {
    const item = EntityQueries.find(d => funcName.startsWith(d[0]))

    if (item) {
      return item[1]
    }
  }

  return null
}

/**
 * Given an EntityType, return the entity datasource function name for it. 
 */
const getFuncNameFromEntityType = (entityType: string) => {
  if (entityType) {
    const item = EntityQueries.find(d => entityType === d[1])

    if (item) {
      return item[0]
    }
  }

  return ''
}

/**
 * Parse some relevant function info out of an attribute or query.
 */
const parseFunctionInfo = (query: string): OnmsEntityFunctionInfo => {
  let entityType = ''
  let funcName = ''
  let attr = ''
  let labelFormat = ''
  let valueFormat = ''
  const labelFormatArg = 'labelFormat='
  const valueFormatArg = 'valueFormat='

  const functions = findFunctions(query)

  for (const func of functions) {
    funcName = func.name

    let foundFirstArg = false

    for (const arg of func.arguments) {
      const trimmed = (arg || '').trim()

      if (trimmed) {
        if (trimmed.startsWith(labelFormatArg)) {
          labelFormat = trimmed.substring(labelFormatArg.length)
        } else if (trimmed.startsWith(valueFormatArg)) {
          valueFormat = trimmed.substring(valueFormatArg.length)
        } else if (!foundFirstArg) {
          attr = trimmed
          foundFirstArg = true
        }
      }
    }

    const e = getEntityTypeFromFuncName(func.name)

    if (e) {
      entityType = e
      break
    }
  }

  return {
    entityType,
    funcName,
    attribute: attr,
    labelFormat,
    valueFormat
  } as OnmsEntityFunctionInfo
}

export {
  findFunctions,
  formatFunctions,
  getEntityTypeFromFuncName,
  getFuncNameFromEntityType,
  parenthesize,
  parenthesizeWithArguments,
  parseFunctionInfo,
  replaceFunctions,
  OnmsEntityFunctionInfo
}
