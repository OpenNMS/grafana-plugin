import { Segment } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import { API } from 'opennms';
import React, { useState } from 'react'
import { PerformanceTypeOptions } from './constants';
import { PerformanceAttribute } from './PerformanceAttribute';
import { PerformanceExpression } from './PerformanceExpression';
import { PerformanceFilter } from './PerformanceFilter';
import { PerformanceStringProperty } from './PerformanceStringProperty';
import { PerformanceQueryEditorProps, QuickSelect } from './types';

export const PerformanceQueryEditor: React.FC<PerformanceQueryEditorProps> = ({ onChange, query, onRunQuery, datasource, ...rest }) => {
    const [performanceType, setPerformanceType] = useState<QuickSelect>({});

    const updateAttributeQuery = (attribute) => {
        onChange({
            ...query,
            performanceType,
            attribute
        })
        onRunQuery();
    }

    const updateExpressionQuery = (expression,label) => {
        onChange({
            ...query,
            performanceType,
            expression,
            label
        })
        onRunQuery();
    }

    const loadNodes = async () => {
        const nodes = await datasource.client.findNodes(new API.Filter(), true)
        return nodes;
    }

    const loadResourcesByNodeId = async (nodeId) => {
        const resources = await datasource.simpleRequest.doOpenNMSRequest({
            url: '/rest/resources/fornode/' + encodeURIComponent(nodeId),
            method: 'GET'
        })
        return resources.data.children.resource;
    }

    const loadAttributesByResourceAndNode = async (node, resource) => {
        const subbed = resource.id.replace('node[', 'nodeSource[')
        const resources = await datasource.simpleRequest.doOpenNMSRequest({
            url: '/rest/resources/' + encodeURIComponent(subbed),
            method: 'GET',
            params: {
                depth: -1
            }
        })
        return Object.entries(resources.data.rrdGraphAttributes).map(([key, item]: [string, unknown]) => {
            return { ...(item as {}), label: key }
        })
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
                performanceType.value === PerformanceTypeOptions.Attribute.value &&

                <PerformanceAttribute
                    updateQuery={updateAttributeQuery}
                    loadNodes={loadNodes}
                    loadResourcesByNodeId={loadResourcesByNodeId}
                    loadAttributesByResourceAndNode={loadAttributesByResourceAndNode}
                />
            }

            {
                performanceType.value === PerformanceTypeOptions.Expression.value &&

                <PerformanceExpression updateQuery={updateExpressionQuery} />
            }

            {
                performanceType.value === PerformanceTypeOptions.Filter.value &&

                <PerformanceFilter updateQuery={updateAttributeQuery} loadFilters={loadFilters} />
            }

            {
                performanceType.value === PerformanceTypeOptions.StringProperty.value &&

                <PerformanceStringProperty
                    updateQuery={updateAttributeQuery}
                    loadNodes={loadNodes}
                    loadResourcesByNodeId={loadResourcesByNodeId}
                />
            }
        </div>
    )
}
