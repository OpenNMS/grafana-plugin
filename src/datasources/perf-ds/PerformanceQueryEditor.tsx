import React, { useEffect, useState } from 'react'
import { Segment } from '@grafana/ui';
import { getTemplateSrv } from '@grafana/runtime';
import { API } from 'opennms';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import { PerformanceTypeOptions } from './constants';
import { PerformanceAttribute } from './PerformanceAttribute';
import { PerformanceExpression } from './PerformanceExpression';
import { PerformanceFilter } from './PerformanceFilter';
import { PerformanceStringProperty } from './PerformanceStringProperty';
import { PerformanceQueryEditorProps, QuickSelect, PerformanceStringPropertyState } from './types';
import { collectInterpolationVariables, interpolate } from './queries/interpolate'
import { getRemoteResourceId } from './queries/queryBuilder'
import { getTemplateVariable, isTemplateVariable, getStringProperties } from './PerformanceHelpers';
import { OnmsResourceDto, OnmsRrdGraphAttribute } from '../../lib/api_types'
import { getMultiValues } from 'lib/utils';

export const PerformanceQueryEditor: React.FC<PerformanceQueryEditorProps> = ({ onChange, query, onRunQuery, datasource, ...rest }) => {
    const [performanceType, setPerformanceType] = useState<QuickSelect>(query.performanceType);

    const updateAttributeQuery = (attribute) => {
        onChange({
            ...query,
            performanceType,
            attribute
        })
    }

    const updateExpressionQuery = (expression, label) => {
        onChange({
            ...query,
            performanceType,
            expression,
            label
        })
    }

    const updateFilterQuery = (filter, filterState) => {
        onChange({
            ...query,
            performanceType,
            filter,
            filterState
        })
    }

    const updateStringQuery = (stringPropertyState) => {
        onChange({
            ...query,
            performanceType,
            stringPropertyState,
        })
    }

    useEffect(() => {
      onRunQuery()
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [query])

    const isPerformanceType = (value: number) => {
        if (performanceType && performanceType.value) {
            return performanceType.value === value
        }

        return false
    }

    const loadNodes = async () => {
        const nodes = await datasource.client.findNodes(new API.Filter(), true)
        return nodes;
    }

    /**
     * Load resources for the PerformanceAttribute Resources dropdown by either a node id (selected from
     * Node dropdown) or from a template variable that evaluates to a node id.
     */
    const loadResourcesByNode = async (value) => {
        let nodeId = value

        if (isTemplateVariable(value)) {
            nodeId = getTemplateVariable(value)
        } else if (value instanceof Object && value.id) {
            nodeId = value.id
        }

        return loadResourcesByNodeId(nodeId)
    }

    const loadResourcesByNodeId = async (nodeId) => {
        const resourceData = await datasource.doResourcesForNodeRequest(nodeId)

        if (resourceData) {
            return resourceData.children.resource.map(r => {
                const label = (r.label && r.name && r.label !== r.name) ?
                    `${r.label} | ${r.name}` :
                    (r.label || r.name || '')

                const id = fixNodeResourceId(r.id)

                return {
                    ...r,
                    label,
                    id
                } as OnmsResourceDto
            })
        } else {
            return []
        }
    }

    // get just the resource portion of a node/resource expression
    const fixNodeResourceId = (id: string) => {
      const re = /node(Source)?\[.*?]\.(.*)$/
      const match = re.exec(id)
      
      if (match && match.length > 2) {
        return match[2]
      }

      return id
    }

    const loadAttributesByResourceAndNode = async (node, resource) => {
        // Note: similar to legacy 'suggestAttributes'
        const interpolationVars = collectInterpolationVariables(getTemplateSrv())

        const interpolatedNodeId = interpolate({ value: node.id || node.label }, ['value'], interpolationVars)?.[0].value || ''
        const interpolatedResourceId = interpolate({ value: resource.id || resource.label }, ['value'], interpolationVars)?.[0].value || ''
        const remoteResourceId = getRemoteResourceId(interpolatedNodeId, interpolatedResourceId)

        const resourceData = await datasource.doResourcesRequest(remoteResourceId)

        if (resourceData) {
            return Object.entries(resourceData.rrdGraphAttributes).map(([key, item]: [string, OnmsRrdGraphAttribute]) => {
                return { ...(item as {}), label: key }
            })
        } else {
            return []
        }
    }

    const loadFilters = async () => {
        const filters = await datasource.simpleRequest.doOpenNMSRequest({
            url: '/rest/measurements/filters',
            method: 'GET'
        })

        return filters.data.map((filter) => {
            return { ...filter, label: filter.name }
        })
    }

    const loadResourcesForStringPropertyState = async (performanceState: PerformanceStringPropertyState) => {
        // get all resources for the performanceState.node using select single node, multi nodes and template variables
        const resources: OnmsResourceDto[] = []

        if (performanceState?.node?.id || performanceState?.node?.label) {
            const nodeValues = getTemplateVariable(performanceState?.node)

            for (const node of getMultiValues(nodeValues)) {
                const result = await loadResourcesByNode(node)
                resources.push(...result)
            }
        }

        return resources
    }

    const loadStringPropertiesForState = async (performanceState: PerformanceStringPropertyState) => {
        const stringProperties: Array<{ label: string, value: string }> = []

        if (performanceState?.resource?.stringPropertyAttributes) {
            stringProperties.push(...getStringProperties(performanceState?.resource))
        } else if (isTemplateVariable(performanceState?.resource)) {
            const nodeValues = getTemplateVariable(performanceState?.node)
            const resourceIds = getTemplateVariable(performanceState?.resource)

            if (nodeValues && resourceIds) {
                const resources = await datasource.simpleRequest.getResourcesFor(nodeValues, resourceIds)
                resources.forEach(r => {
                    stringProperties.push(...getStringProperties(r))
                })
            }
        }

        return stringProperties
    }

    return (
        <div className='pf-query-editor'>
            <style>
                {`
                .bigger-labels label {
                    min-width: 32px;
                }
                .spacer {
                    margin-bottom: 6px;
                }
                .max-input {
                    max-width: 150px;
                }
                .pf-query-editor label, .pf-query-editor input {
                    min-width: 230px;
                    width: auto;
                }
                .pf-query-editor label.segment-with-icon {
                    min-width: 160px;
                    width: auto;
                }
                .pf-query-editor input {
                    border:1px solid transparent;
                    margin-bottom:0;
                }
            `}
            </style>
            <SegmentSectionWithIcon label='Type'>
                <Segment
                    placeholder='Choose Type'
                    value={performanceType}
                    onChange={(d) => setPerformanceType(d)}
                    options={[
                        PerformanceTypeOptions.Attribute,
                        PerformanceTypeOptions.Expression,
                        PerformanceTypeOptions.Filter,
                        PerformanceTypeOptions.StringProperty,
                    ]}
                />
            </SegmentSectionWithIcon>

            {
                isPerformanceType(PerformanceTypeOptions.Attribute.value) &&

                <PerformanceAttribute
                    performanceAttributeState={query.attribute}
                    updateQuery={updateAttributeQuery}
                    loadNodes={loadNodes}
                    loadResourcesByNode={loadResourcesByNode}
                    loadAttributesByResourceAndNode={loadAttributesByResourceAndNode}
                />
            }

            {
                isPerformanceType(PerformanceTypeOptions.Expression.value) &&

                <PerformanceExpression query={query} updateQuery={updateExpressionQuery} />
            }

            {
                isPerformanceType(PerformanceTypeOptions.Filter.value) &&

                <PerformanceFilter query={query} updateQuery={updateFilterQuery} loadFilters={loadFilters} />
            }

            {
                isPerformanceType(PerformanceTypeOptions.StringProperty.value) &&

                <PerformanceStringProperty
                    query={query}
                    updateQuery={updateStringQuery}
                    loadNodes={loadNodes}
                    loadResourcesByNode={loadResourcesByNode}
                    loadResourcesForStringPropertyState={loadResourcesForStringPropertyState}
                    loadStringPropertiesForState={loadStringPropertiesForState}
                />
            }
        </div>
    )
}
