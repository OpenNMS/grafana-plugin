import React, { useEffect, useState } from 'react'
import { Segment, SegmentInput, SegmentAsync } from "@grafana/ui"
import { SelectableValue } from '@grafana/data';
import { FlowFunctions } from './constants';
import { FlowFunction, FlowQueryFunctionProps } from './types';
import { Tools } from './Tools';


export const FlowQueryFunction = ({
    autofocus,
    options,
    onChange,
    value,
    moveMeLeft,
    moveMeRight,
    index,
    last,
    removeMe,
    client,
    start,
    end,
    activeParameter,
    setActiveParameter,
    parameterOption,
    setParameterOption
}: FlowQueryFunctionProps) => {

    const [activeFunction, setActiveFunction] = useState<FlowFunction>();
    const [tools, setTools] = useState(false);
    const [hideTimeout, setHideTimeout] = useState<number>();

    useEffect(() => {
        if (value.label) {
            setActiveFunction((oldActiveFunction) => value.label ? FlowFunctions.get(value.label) : oldActiveFunction)
        }
    }, [value, activeFunction?.parameter])

    const stringToSelectableValue = (bareString: string) => {
        return { label: bareString }
    }

    const getLatestOptions = async (optionQuery: string | undefined) => {
        let options: Array<SelectableValue<string>> = [];
        if (activeFunction?.parameterOptions) {
            options = (await activeFunction.parameterOptions(optionQuery || '',client,start,end)).map(stringToSelectableValue)
        }
        return options
    }

    const openTools = () => {
        if (hideTimeout) {
            clearTimeout(hideTimeout);
            setHideTimeout(0);
        } else {
            setTools(true);
        }
    }

    const hideTools = () => {
        setHideTimeout(window.setTimeout(() => {
            setTools(false);
            setHideTimeout(0)
        }, 200));
    }

    return (
        <div
            onMouseLeave={hideTools}
            onMouseEnter={openTools}
            style={{
                display: 'flex',
                border: '1px solid #626262',
                borderRadius: '4px',
                marginRight: '4px',
                position: 'relative',
            }}>
            <Tools
                moveMeLeft={moveMeLeft}
                moveMeRight={moveMeRight}
                hideTools={() => {
                    removeMe(index);
                    setTools(false)
                }}
                index={index}
                visible={tools}
                last={last}
            />
            <Segment
                autofocus={autofocus}
                options={options}
                onBlur={() => {
                    if (!value.label) {
                        removeMe(index);
                    }
                }}
                onChange={onChange}
                value={value}
            />
            {typeof activeParameter !== 'undefined' && activeParameter !== null && 
            <SegmentInput
                value={activeParameter}
                onChange={(text: string | number) => {
                    if (typeof text === 'string'){
                        setActiveParameter(text, index)
                    }
                }}
            />}
            {activeFunction?.parameterOptions ?
                <SegmentAsync
                    reloadOptionsOnChange
                    onChange={(nVal) => { setParameterOption(nVal, index) }}
                    loadOptions={getLatestOptions} value={parameterOption} />
                : null}
        </div>
    )
}
