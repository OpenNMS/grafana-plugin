import React, { useState, useEffect } from 'react';
import { Segment, SegmentSection, Button } from '@grafana/ui'
import { QueryEditorProps, SelectableValue } from '@grafana/data';
import { FlowDataSource } from './FlowDataSource';
import { FlowDataSourceOptions, FlowQuery } from './types';
import { useSegmentOptions } from './useSegmentOptions';
import { FlowFunctions, FlowSegments, segmentOptionValues } from './constants';
import { FlowQueryFunction } from './FlowQueryFunction';

type Props = QueryEditorProps<FlowDataSource, FlowQuery, FlowDataSourceOptions>;

export const FlowQueryEditor: React.FC<Props> = ({ onChange, onRunQuery, query, ...rest }) => {
    const [focusList, setFocusList] = useState<boolean[]>([]);
    const [segmentValue, setSegmentValue] = useState<SelectableValue<number>>(
        typeof query.segment === 'number' ? { value: query.segment, label: FlowSegments[query.segment] } : { value: -1 }
    )
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

                onChange({
                    ...query,
                    segment: segmentValue.value,
                    functions: functionNameList,
                    functionParameters: activeParameterList,
                    parameterOptions: parameterOptionList
                });
                onRunQuery();
            }, 1000)
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [segmentValue, functionNameList, activeParameterList, parameterOptionList])

    const shiftFunctionPosition = (direction, index) => {
        setFunctionNameList((oldFunctionList) => {
            let newFunctionList = oldFunctionList;
            let newFunctionParameterList = activeParameterList
            let newFunctionOptionParameterList = parameterOptionList
            if (oldFunctionList) {
                newFunctionList = [...oldFunctionList]
                let temp = newFunctionList[index + direction]
                let tempParam = newFunctionParameterList[index + direction]
                let tempParamOpt = newFunctionOptionParameterList[index + direction]
                newFunctionList[index + direction] = newFunctionList[index]
                newFunctionList[index] = temp;
                newFunctionParameterList[index + direction] = newFunctionParameterList[index]
                newFunctionParameterList[index] = tempParam
                newFunctionOptionParameterList[index + direction] = newFunctionOptionParameterList[index]
                newFunctionOptionParameterList[index] = tempParamOpt
                return newFunctionList
            }
            return newFunctionList
        })
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
        setFunctionNameList((oldList) => {
            let newList = oldList;
            if (oldList) {
                newList = [...oldList];
                newList.splice(index, 1);
            }
            return newList;
        })
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
