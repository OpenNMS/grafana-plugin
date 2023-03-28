import React, { useState, useEffect } from 'react'
import { SegmentAsync } from '@grafana/ui';
import { SegmentSectionWithIcon } from 'components/SegmentSectionWithIcon';
import { PerformanceStringPropertyProps, PerformanceStringPropertyState } from './types';
import { isTemplateVariable, getStringPropertiesForState } from './PerformanceHelpers'

export const defaultPerformanceStringState = {
    node: { id: '' },
    resource: { id: '', stringPropertyAttributes: {} },
    stringProperty: { label: '', value: '' },
}

export const PerformanceStringProperty: React.FC<PerformanceStringPropertyProps> = ({
    query,
    updateQuery,
    loadNodes,
    loadResourcesByNode,
}) => {

    const [performanceState, setPerformanceState] = useState<PerformanceStringPropertyState>(query.stringPropertyState || defaultPerformanceStringState)

    const setPerformanceStateProperty = (propertyName: string, propertyValue: unknown) => {
        setPerformanceState({ ...performanceState, [propertyName]: propertyValue })
    }

    useEffect(() => {
        if (performanceState?.stringProperty){
            updateQuery(performanceState)
        }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [performanceState])


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
                (performanceState?.node?.id  || isTemplateVariable(performanceState?.node)) &&

                <SegmentSectionWithIcon label='Resource' icon='leaf'>
                    <SegmentAsync
                        value={performanceState?.resource}
                        placeholder='Select Resource'
                        loadOptions={() => loadResourcesByNode(performanceState?.node)}
                        onChange={(value) => {
                            setPerformanceStateProperty('resource', value);
                        }}
                    />
                </SegmentSectionWithIcon>
            }
            <div className='spacer' />
            {
                (performanceState?.node?.id || isTemplateVariable(performanceState?.node)) && 
                (performanceState?.resource?.id || isTemplateVariable(performanceState?.resource)) &&

                <SegmentSectionWithIcon label='String Property' icon='tag'>
                    <SegmentAsync
                        value={performanceState?.stringProperty}
                        placeholder='Select String Property'
                        loadOptions={() => getStringPropertiesForState(performanceState, loadResourcesByNode)}
                        onChange={(value) => {
                            setPerformanceStateProperty('stringProperty', value);
                        }}
                    />
                </SegmentSectionWithIcon>
            }
        </>
    )
}
