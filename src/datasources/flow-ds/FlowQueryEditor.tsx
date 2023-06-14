import React, { useState, useEffect } from 'react';
import { Segment, SegmentSection, Button } from '@grafana/ui'
import { QueryEditorProps, SelectableValue } from '@grafana/data';
import { FlowDataSource } from './FlowDataSource';
import { FlowDataSourceOptions, FlowQuery } from './types';
import { useSegmentOptions } from './useSegmentOptions';
import { FlowFunctions, FlowSegments, segmentOptionValues } from './constants';
import { FlowQueryFunction } from './FlowQueryFunction';

type Props = QueryEditorProps<FlowDataSource, FlowQuery, FlowDataSourceOptions>;

const initSegment = (segment: any) => {
    let value = -1

    if (typeof segment === 'number') {
      value = segment
    } else if (typeof segment?.value === 'number') {
      value = segment.value
    } else if (typeof segment?.id === 'number') {
      value = segment.id
    }

    return (value >= FlowSegments.Applications && value <= FlowSegments.Dscps) ? segmentOptionValues[value] : { value: -1, label: '' }
}

export const FlowQueryEditor: React.FC<Props> = ({ onChange, onRunQuery, query, ...rest }) => {
    const [focusList, setFocusList] = useState<boolean[]>([]);
    const [segmentValue, setSegmentValue] = useState<SelectableValue<number>>(initSegment(query.segment || {}))
    const [functionNameList, setFunctionNameList] = useState<Array<SelectableValue<string>>>(query.functions || [])
    const [activeParameterList, setActiveParameterList] = useState<Array<string | undefined>>(query.functionParameters || []);
    const [parameterOptionList, setParameterOptionList] = useState<Array<SelectableValue<string>>>(query.parameterOptions || []);

    const { options } = useSegmentOptions(functionNameList, segmentValue);

    const updateFunctionList = (functionName, item, index) => {
        if (functionNameList) {
            const newNameList = [...functionNameList]
            newNameList[index] = functionName;
            setFunctionNameList(newNameList);
            const oldList = [...activeParameterList]
            oldList[index] = FlowFunctions.get(functionName.label)?.parameter
            setActiveParameterList(oldList)
            const oldOptionList = [...parameterOptionList]            
            oldOptionList[index] = { }
            setParameterOptionList(oldOptionList)
        }
    }

    const addFunction = () => {
        if (functionNameList) {
            const newNameList = [...functionNameList]
            newNameList.push({})
            setFunctionNameList(newNameList);
            const newFocusList: boolean[] = [];
            newFocusList[newNameList.length - 1] = true;
            setFocusList(newFocusList);
        } else {
            setFunctionNameList([{}])
            setFocusList([true])
        }
    }

    useEffect(() => {
        if (segmentValue?.label) {
            setTimeout(() => {
                const updatedQuery = {
                    ...query,
                    segment: segmentValue,
                    functions: functionNameList,
                    functionParameters: activeParameterList,
                    parameterOptions: parameterOptionList
                }

                onChange(updatedQuery)
                onRunQuery()
            }, 1000)
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [segmentValue, functionNameList, activeParameterList, parameterOptionList])

    const shiftFunctionPosition = (direction, index) => {
        setFunctionNameList(oldList => { return setListItemPosition(oldList, direction, index) })
        setActiveParameterList(oldList => { return setListItemPosition(oldList, direction, index) })
        setParameterOptionList(oldList => { return setListItemPosition(oldList, direction, index) })
    }

    const setListItemPosition = (oldList: any[], direction, index) => {
        const newList = [...oldList]
        if (newList) {
            const temp = newList[index + direction]
            newList[index + direction] = newList[index]
            newList[index] = temp
        }
        return newList
    }

    const moveItemLeft = (index: number) => {
        if (functionNameList?.[index] && index > 0) {
            shiftFunctionPosition(-1, index);
        }
    }

    const moveItemRight = (index: number) => {
        if (functionNameList?.[index] && index < functionNameList.length - 1) {
            shiftFunctionPosition(1, index);
        }
    }

    const removeItem = (index: number) => {
        setFunctionNameList(oldList => removeListItem(oldList, index))
        setActiveParameterList(oldList => removeListItem(oldList, index))
        setParameterOptionList(oldList => removeListItem(oldList, index))
    }

    const removeListItem = (oldList: any[], index: number) => {
       const newList = [...oldList];
        if (newList) {            
            newList.splice(index, 1);
        }
        return newList;
    }

    return (
        <SegmentSection label='Flow'>
            <Segment placeholder='select metric' options={segmentOptionValues} value={segmentValue} onChange={setSegmentValue} />
            {functionNameList?.map((item, key) => {
                return <FlowQueryFunction
                    autofocus={focusList[key]}
                    key={key}
                    options={options}
                    onChange={(functionName) => updateFunctionList(functionName, item, key)}
                    moveMeLeft={moveItemLeft}
                    moveMeRight={moveItemRight}
                    removeMe={removeItem}
                    index={key}
                    value={item}
                    client={rest.datasource.client}
                    start={rest.range?.from.valueOf()}
                    end={rest.range?.to.valueOf()}
                    last={key === functionNameList.length - 1}
                    activeParameter={activeParameterList[key]}
                    parameterOption={parameterOptionList[key]}
                    setParameterOption={(option, index) => {
                        const newArray = [...parameterOptionList]
                        newArray[index] = option;
                        setParameterOptionList(newArray);
                    }}
                    setActiveParameter={(option, index) => {
                        const newArray = [...activeParameterList]
                        newArray[index] = option;
                        setActiveParameterList(newArray);
                    }}
                />
            })}
            <Button onClick={() => addFunction()} size='xs'>+</Button>
        </SegmentSection>
    )
}
