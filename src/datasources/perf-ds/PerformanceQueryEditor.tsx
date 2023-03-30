import React, { useState } from 'react'
import { Segment } from '@grafana/ui';
import { getTemplateSrv } from '@grafana/runtime';
import { API } from 'opennms';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import { PerformanceTypeOptions } from './constants';
import { PerformanceAttribute } from './PerformanceAttribute';
import { PerformanceExpression } from './PerformanceExpression';
import { PerformanceFilter } from './PerformanceFilter';
import { PerformanceStringProperty } from './PerformanceStringProperty';
import { OnmsRrdGraphAttribute, PerformanceQueryEditorProps, PerformanceTemplateVariableStatus, QuickSelect } from './types';
import { collectInterpolationVariables, interpolate } from './queries/interpolate'
import { getRemoteResourceId } from './queries/queryBuilder'
import { isTemplateVariable } from './PerformanceHelpers';

export const PerformanceQueryEditor: React.FC<PerformanceQueryEditorProps> = ({ onChange, query, onRunQuery, datasource, ...rest }) => {
    const [performanceType, setPerformanceType] = useState<QuickSelect>(query.performanceType);

    const updateAttributeQuery = (attribute) => {
        onChange({
            ...query,
            performanceType,
            attribute
        })
        onRunQuery();
    }

    const updateExpressionQuery = (expression, label) => {
        onChange({
            ...query,
            performanceType,
            expression,
            label
        })
        onRunQuery();
    }

    const updateFilterQuery = (filter, filterState) => {
        onChange({
            ...query,
            performanceType,
            filter,
            filterState
        })
        onRunQuery();
    }

    const updateStringQuery = (stringPropertyState) => {
        onChange({
            ...query,
            performanceType,
            stringPropertyState,
        })
        onRunQuery();
    }

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
        let templateVariable: PerformanceTemplateVariableStatus = { isTemplateVariable: false }
        let nodeId = value
        if ((templateVariable = isTemplateVariable(value)).isTemplateVariable) {
            nodeId = templateVariable.value
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

                return {
                    ...r,
                    label
                }
            })
        } else {
            return []
        }
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
        });

        return filters.data.map((filter) => {
            return { ...filter, label: filter.name }
        });
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
                />
            }
        </div>
    )
}
