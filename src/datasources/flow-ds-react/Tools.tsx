import React from 'react'
import { ToolButton } from './ToolButton';
import { ToolsProps } from './types';


export const Tools = ({ moveMeLeft, moveMeRight, index, hideTools, visible, last }: ToolsProps) => {
    return (
        <div style={{
            position: 'absolute',
            top: -31,
            left: 0,
            borderRadius: 4,
            justifyContent: 'center',
            alignItems: 'center',
            backgroundColor: 'rgb(34, 37, 43)',
            opacity: visible ? 1 : 0,
            transitionProperty: 'opacity',
            transitionTimingFunction: 'ease-in-out',
            transitionDuration: '0.2s',
            display: 'flex'
        }}>
            {index !== 0 && <ToolButton onClick={() => moveMeLeft(index)}>&lt;</ToolButton>}
            <ToolButton style={{ margin: '0 3px' }} onClick={() => hideTools()}>X</ToolButton>
            {!last && <ToolButton onClick={() => moveMeRight(index)}>&gt;</ToolButton>}
        </div>
    )
};
