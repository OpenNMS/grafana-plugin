import { SelectableValue } from '@grafana/data';
import { Segment, SegmentAsync } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import React, { useState } from 'react'

export interface PerformanceStringPropertyProps {
    updateQuery: Function;
    loadNodes: (query?: string | undefined) => Promise<Array<SelectableValue<{ id: string }>>>;
    loadResourcesByNodeId: Function;
}

export interface PerformanceStringPropertyState {
    node: { id: string };
    resource: { id: string, stringPropertyAttributes: Record<string, string> };
    stringProperty: { label: string, value: string };
}

export const defaultPerformanceStringState = {
    node: { id: '' },
    resource: { id: '', stringPropertyAttributes: {} },
    stringProperty: { label: '', value: '' },
}
export const PerformanceStringProperty: React.FC<PerformanceStringPropertyProps> = ({
    updateQuery,
    loadNodes,
    loadResourcesByNodeId,
}) => {

    const [performanceState, setPerformanceState] = useState<PerformanceStringPropertyState>(defaultPerformanceStringState)

    const setPerformanceStateProperty = (propertyName: string, propertyValue: unknown) => {
        setPerformanceState({ ...performanceState, [propertyName]: propertyValue })
    }

    const stringPropertyAttributes = Object.entries(performanceState?.resource?.stringPropertyAttributes).map(([key, item]) => {
        return { label: key, value: key }
    })

    return (
        <>
            <div className='spacer' />
            <SegmentSectionWithIcon label='Node' icon='tree'>
                <SegmentAsync
                    value={performanceState?.node}
                    placeholder='Select Node'
                    loadOptions={loadNodes}
                    onChange={(value) => {
                        setPerformanceStateProperty('node', value);
                    }}
                />
            </SegmentSectionWithIcon>
            <div className='spacer' />

            {
                performanceState?.node?.id &&

                <SegmentSectionWithIcon label='Resource' icon='leaf'>
                    <SegmentAsync
                        value={performanceState?.resource}
                        placeholder='Select Resource'
                        loadOptions={() => loadResourcesByNodeId(performanceState?.node?.id)}
                        onChange={(value) => {
                            setPerformanceStateProperty('resource', value);
                        }}
                    />
                </SegmentSectionWithIcon>
            }
            <div className='spacer' />
            {
                performanceState?.node?.id && performanceState?.resource?.id &&

                <SegmentSectionWithIcon label='String Property' icon='tag'>
                    <Segment
                        value={performanceState?.stringProperty}
                        placeholder='Select String Property'
                        options={stringPropertyAttributes}
                        onChange={(value) => {
                            setPerformanceStateProperty('stringProperty', value);
                        }}
                    />
                </SegmentSectionWithIcon>
            }
        </>
    )

}
